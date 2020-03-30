# This function aggregates temporally dependent samples for acute ammonia

# Inputs:   data frame 
# Output:   data frames with acute ammonia aggregated values


# df_ammonia_in <- df_ammonia
# df_pH_in = df_pH
# df_coeff_in = df_coeff

f_temp_ind_agg_acute_ammonia <- function(df_pH_in = df_pH, df_ammonia_in = df_ammonia, df_coeff_in = df_coeff){
      
      df_ammonia_in <- df_ammonia_in %>% 
            
            # Add Year, Week of Year to Sample Test Results Data
            mutate(my_year = year(ActivityStartDate)) %>% 
            mutate(week_of_year = week(ActivityStartDate)) %>% 
            
            # This is for Acute Standards 
            filter(Condition == "ACUTE") 
      
      # Get pH Data
      df_pH_in <- df_pH_in %>% 
            
            ungroup() %>% 
            
            # Only pull design uses associated with Acute Ammonia
            filter(Desig_Use %in% c("AWW", "AWC", "AWEDW")) %>% 
            # filter(Condition == "ACUTE") %>% 
            
            # Just relevant columns
            select(WBID, 
                   CharacteristicName, 
                   Desig_Use, 
                   # Condition,
                   # Method,
                   ActivityStartDate, 
                   ActivityStartTime.Time, 
                   ActivityDepthHeightMeasure.MeasureValue,
                   ResultMeasureValue) 
      
      # Join Data for Ammonia and pH (in order to calculate ratio, max toxicity)
      df_out <- left_join(
            df_ammonia_in %>% rename(Ammonia = ResultMeasureValue),
            df_pH_in %>% rename(pH = ResultMeasureValue) %>% select(-CharacteristicName)
      ) 
      
      # Get Coefficients
      m <- df_coeff_in %>% 
            filter(grepl("AMMONIA", substance_name)) %>% 
            filter(Condition == "acute") %>% 
            # Note Alpha Order
            arrange(desig_use) %>% 
            select(a,b,c,d) %>% 
            as.matrix()
      
      rownames(m) <- c("awc", "awedw", "aww")
      
      # Calculate Ratio
      df_out <- df_out %>% 
            
            filter(!is.na(Ammonia) & !is.na(pH)) %>%
            
            rowwise() %>% 
            
            # Calculate Result Dependent Standard
            mutate(acute_standard = case_when(
                  
                  # Note Matrix m[ , ] Structure / Order
                  Desig_Use == "AWC"  ~  m[1,1]/(1 + 10^(m[1,2] - max(6.5, min(9, pH)) )) + m[1,3]/(1 + 10^(max(6.5, min(9, pH)) + m[1,4])),
                  Desig_Use == "AWEDW"  ~  m[2,1]/(1 + 10^(m[2,2] - max(6.5, min(9, pH)) )) + m[2,3]/(1 + 10^(max(6.5, min(9, pH)) + m[2,4])),
                  Desig_Use == "AWW"  ~  m[3,1]/(1 + 10^(m[3,2] - max(6.5, min(9, pH)) )) + m[3,3]/(1 + 10^(max(6.5, min(9, pH)) + m[3,4]))
                  
            # Original Hard Coded Coefficients
            # mutate(acute_standard = case_when(
            #       
            #       Desig_Use == "AWC"  ~  0.275/(1 + 10^(7.204 - max(6.5, min(9, pH)) )) + 39/(1 + 10^(max(6.5, min(9, pH)) - 7.204)),
            #       Desig_Use == "AWW"  ~  0.411/(1 + 10^(7.204 - max(6.5, min(9, pH)) )) + 58.4/(1 + 10^(max(6.5, min(9, pH)) - 7.204)),
            #       Desig_Use == "AWEDW"  ~  0.411/(1 + 10^(7.204 - max(6.5, min(9, pH)) )) + 58.4/(1 + 10^(max(6.5, min(9, pH)) - 7.204))
                  
            )) %>% 
            
            # Calculate Ratio
            mutate(ammonia_ratio = Ammonia / acute_standard)
      
      
      # Find Max Toxicity
      df_out_max_ratio <- df_out %>% 
            
            # Pull Max for Week
            select(-ActivityStartDate, -ActivityStartTime.Time, -ActivityDepthHeightMeasure.MeasureValue) %>% 
            group_by(WBID, Desig_Use, CharacteristicName, my_year, week_of_year) %>% 
            mutate(max_ratio = max(ammonia_ratio)) %>% 
            filter(ammonia_ratio == max_ratio) 
            
      # Extract Ammonia (Max Ratio)
      df_out <- df_out %>% 
            select(WBID, Desig_Use, CharacteristicName, my_year, week_of_year, ResultMeasureValue = Ammonia, ammonia_ratio) %>% 
            inner_join(df_out_max_ratio) %>% 
            # Some Reps Even After Max Aggregation
            unique()
      
      return(df_out)
      
}
