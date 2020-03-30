# This function aggregates temporally dependent samples for acute ammonia

# Inputs:   data frame 
# Output:   data frames with acute ammonia aggregated values

# df_pH_in <- df_pH
# df_ammonia_in <- df_ammonia
# df_temp_in = df_temperature
# df_coeff_in = df_coeff

f_temp_ind_agg_chronic_ammonia <- function(df_ammonia_in = df_ammonia, df_pH_in = df_pH, df_temp_in = df_temperature, df_coeff_in = df_coeff){
      
      # Get Ammonia Data
      df_ammonia_in <- df_ammonia_in %>% 
            
            ungroup() %>% 
            
            # Weeks
            mutate(my_year = year(ActivityStartDate)) %>% 
            mutate(week_of_year = week(ActivityStartDate)) %>% 
            
            # Chronic Only
            filter(Condition == "CHRONIC") %>% 

            # Core Columns
            select(
                  WBID, 
                  Desig_Use, 
                  CharacteristicName,
                  my_year,
                  week_of_year,
                  ResultMeasureValue
            )
      
      
      # Get pH Data
      df_pH_in <- df_pH_in %>% 
            
            ungroup() %>% 

            # Weeks
            mutate(my_year = year(ActivityStartDate)) %>% 
            mutate(week_of_year = week(ActivityStartDate)) %>% 
            
            # Only pull desig_uses associated with Chronic Ammonia
            filter(Desig_Use %in% c("AWW", "AWC", "AWEDW")) %>% 
            # filter(Condition == "CHRONIC") %>% 
            
            # Just relevant columns
            select(
                  WBID, 
                  CharacteristicName, 
                  Desig_Use, 
                  my_year,
                  week_of_year,
                  ResultMeasureValue
            ) 
      
     
      # Get Temp Data (C)
      df_temp_in <- df_temp_in %>% 
            
            ungroup() %>% 
            
            # Weeks
            mutate(my_year = year(ActivityStartDate)) %>% 
            mutate(week_of_year = week(ActivityStartDate)) %>% 
            
            # Core Columns
            select(
                  WBID, 
                  CharacteristicName, 
                  Desig_Use, 
                  my_year,
                  week_of_year,
                  ResultMeasureValue
            ) 
      
            
      # Calculate Means (Week)
      df_ammonia_in <- df_ammonia_in %>% 
            group_by_at(vars(-ResultMeasureValue)) %>% 
            summarise(ammonia = mean(ResultMeasureValue))
      
      df_temp_in <- df_temp_in %>% 
            group_by_at(vars(-ResultMeasureValue)) %>% 
            summarise(temp = mean(ResultMeasureValue))
      
      df_pH_in <- df_pH_in %>% 
            group_by_at(vars(-ResultMeasureValue)) %>% 
            summarise(ph = f_neg_log_10_mean(ResultMeasureValue))
      
      
      # Join
      df_out <-  
            inner_join(
                  
                  df_ammonia_in,
                  df_temp_in %>% ungroup() %>% select(-CharacteristicName)
                  
            ) %>% 
            inner_join(
                  df_pH_in %>% ungroup() %>% select(-CharacteristicName)
            )
      
      
      # Get Coefficients
      m <- df_coeff_in %>% 
            filter(grepl("AMMONIA", substance_name)) %>% 
            filter(Condition == "chronic") %>% 
            # Note Alpha Order
            arrange(desig_use) %>% 
            select(a,b,c,d) %>% 
            as.matrix()
      
      rownames(m) <- c("awc", "awedw", "aww")
      
      # Calculate Chronic Standards
      df_out <- df_out %>% 
            
            filter(!is.na(ammonia) & !is.na(ph) & !is.na(temp)) %>% 
            
            rowwise() %>% 
            
            # Assumes Formula Results is Used vs Standard
            mutate(chronic_standard = case_when(
                  
                  # Note Matrix Order
                  Desig_Use == "AWC"  ~  (m[1,1]/(1 + 10^(m[1,2] - max(6.5, min(9, ph)))) + m[1,3]/(1 + 10^(max(6.5, min(9, ph)) + m[1,4]))) * min(2.85,  1.45*10^(0.028*(25 - max(0, min(30, temp ))))),
                  Desig_Use == "AWEDW"  ~  (m[2,1]/(1 + 10^(m[2,2] - max(6.5, min(9, ph)))) + m[2,3]/(1 + 10^(max(6.5, min(9, ph)) + m[2,4]))) * min(2.85,  1.45*10^(0.028*(25 - max(0, min(30, temp ))))),
                  Desig_Use == "AWW"  ~  (m[3,1]/(1 + 10^(m[3,2] - max(6.5, min(9, ph)))) + m[3,3]/(1 + 10^(max(6.5, min(9, ph)) + m[3,4]))) * min(2.85,  1.45*10^(0.028*(25 - max(0, min(30, temp )))))
                  
                  # Note Matrix Order (Original Hard Coded Coefficients)
                  # Desig_Use == "AWC"  ~  (0.0577/(1 + 10^(7.688 - max(6.5, min(9, ph)))) + 2.487/(1 + 10^(max(6.5, min(9, ph)) - 7.688)) ) * min(2.85,  1.45*10^(0.028*(25 - max(0, min(30, temp ))))),
                  # Desig_Use == "AWEDW"  ~  (0.0577/(1 + 10^(7.688 - max(6.5, min(9, ph)))) + 2.487/(1 + 10^(max(6.5, min(9, ph)) - 7.688)) ) * min(2.85,  1.45*10^(0.028*(25 - max(0, min(30, temp ))))),
                  # Desig_Use == "AWW"  ~  (0.0577/(1 + 10^(7.688 - max(6.5, min(9, ph)))) + 2.487/(1 + 10^(max(6.5, min(9, ph)) - 7.688)) ) * min(2.85,  1.45*10^(0.028*(25 - max(0, min(30, temp )))))
                  
            )) 
      
      return(df_out)
      
}
