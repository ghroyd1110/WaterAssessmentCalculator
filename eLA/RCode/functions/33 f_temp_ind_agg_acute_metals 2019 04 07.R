# This function aggregates temporally dependent samples for acute metals

# Inputs:   data frame 
# Output:   data frames with acute metals aggregated values

# df_metals_in = df_metals
# df_hardness_in = df_hardness
# df_coeff_in = df_coeff

f_temp_ind_agg_acute_metals <- function(df_metals_in = df_metals, df_hardness_in = df_hardness, df_coeff_in = df_coeff){
      
# Add Year, Week of Year to Data ==== 
      
      df_metals_in <- df_metals_in %>% 
            
            # Weeks
            mutate(my_year = year(ActivityStartDate)) %>% 
            mutate(week_of_year = week(ActivityStartDate)) %>% 
            
            # Just Acute
            filter(Condition == "ACUTE") 
      
      # Non Detect ====
      df_metals_in <- f_non_detect_simple(df_metals_in)
      
      # Core Columns
      df_metals_in <- df_metals_in %>% 
            
            select(
                  
                  WBID, 
                  Desig_Use, 
                  CharacteristicName,
                  ActivityStartDate, 
                  ActivityStartTime.Time, 
                  ActivityDepthHeightMeasure.MeasureValue,
                  my_year,
                  week_of_year,
                  ResultMeasureValue
            )
      
      
      
# Hardness Data ====
      
      df_hardness_in <- df_hardness_in %>% 
            
            ungroup() %>% 
            
            # Already ran through non-detect
            
            # Acute Only
            filter(Condition == "ACUTE") %>% 
            
            # Dissolved Only - I Don't Think So...
            # filter(ResultSampleFractionText =="DISSOLVED") %>% 
            
            # Weeks
            mutate(my_year = year(ActivityStartDate)) %>% 
            mutate(week_of_year = week(ActivityStartDate)) %>%
            
            select(
                  
                  WBID, 
                  Desig_Use, 
                  CharacteristicName,
                  # ResultSampleFractionText, 
                  ActivityStartDate, 
                  ActivityStartTime.Time, 
                  ActivityDepthHeightMeasure.MeasureValue,
                  my_year,
                  week_of_year,
                  ResultMeasureValue
            ) 
      
      # Drop if Hardness = 0 (Because Melts Down with log in Formulas)
      df_hardness_in <- df_hardness_in %>% filter(ResultMeasureValue != 0)
      
      
# Join Data for Metals and Hardness (in order to calculate ratio, max toxicity) ====
      
      df_out <- inner_join(
            df_metals_in,
            df_hardness_in %>% 
                  ungroup() %>% 
                  rename(hardness = ResultMeasureValue) %>% 
                  select(-CharacteristicName)
      ) 
      
      # Get Coefficients
      f_m_coeff <- function(substance){
            
            m <- df_coeff_in %>% 
                  filter(substance_name == substance) %>% 
                  filter(Condition == "acute") %>% 
                  # Note Alpha Order
                  arrange(substance_name, desig_use)
            
            m_rownames <- m %>% pull(desig_use)
            
            m <- m %>% select(a,b,c,d) %>% as.matrix()
            
            rownames(m) <- m_rownames
            
            return(m)
      }
      
      m_cd <- f_m_coeff(substance = "CADMIUM")
      m_cu <- f_m_coeff(substance = "COPPER")
      m_pb <- f_m_coeff(substance = "LEAD")
      m_zn <- f_m_coeff(substance = "ZINC")
      
      
      # Calculate Toxicity
      df_out <- df_out %>% 
            
            # Check Present
            filter(!is.na(hardness) & !is.na(ResultMeasureValue)) %>% 
            
            # To calculate min, max within row (instead of column)
            rowwise() %>% 
            
            # Calculate Result Dependent Standard
            mutate(acute_standard = case_when(
                  
                  CharacteristicName == "CADMIUM" & Desig_Use == "AWC"  ~  exp(m_cd[1,4] * log(max(0, min(400, hardness))) + m_cd[1,2]) * (m_cd[1,1] - log(max(0, min(400, hardness))) * m_cd[1,3]),
                  CharacteristicName == "CADMIUM" & Desig_Use == "AWE" ~ exp(m_cd[2,4] * log(max(0, min(400, hardness))) + m_cd[2,2]) * (m_cd[2,1] - log(max(0, min(400, hardness))) * m_cd[2,3]),
                  CharacteristicName == "CADMIUM" & Desig_Use == "AWEDW"  ~ exp(m_cd[3,4] * log(max(0, min(400, hardness))) + m_cd[3,2]) * (m_cd[3,1] - log(max(0, min(400, hardness))) * m_cd[3,3]),
                  CharacteristicName == "CADMIUM" & Desig_Use == "AWW"  ~  exp(m_cd[4,4] * log(max(0, min(400, hardness))) + m_cd[4,2]) * (m_cd[4,1] - log(max(0, min(400, hardness))) * m_cd[4,3]),
                 
                  CharacteristicName == "COPPER" & Desig_Use == "AWC"  ~  exp(m_cu[1,4] * (log(max(0, min(400, hardness)))) + m_cu[1,2]) * m_cu[1,1],
                  CharacteristicName == "COPPER" & Desig_Use == "AWE" ~	exp(m_cu[2,4] * (log(max(0, min(400, hardness)))) + m_cu[2,2]) * m_cu[2,1],
                  CharacteristicName == "COPPER" & Desig_Use == "AWEDW"  ~ exp(m_cu[3,4] * (log(max(0, min(400, hardness)))) + m_cu[3,2]) * m_cu[3,1],
                  CharacteristicName == "COPPER" & Desig_Use == "AWW"  ~  exp(m_cu[4,4] * (log(max(0, min(400, hardness)))) + m_cu[4,2]) * m_cu[4,1],
                  
                  CharacteristicName == "LEAD" & Desig_Use == "AWC"  ~ exp(m_pb[1,4] * (log(max(0, min(400, hardness)))) + m_pb[1,2]) * (m_pb[1,1] - (log(max(0, min(400, hardness))) * m_pb[1,3])),
                  CharacteristicName == "LEAD" & Desig_Use == "AWE" ~ exp(m_pb[2,4] * (log(max(0, min(400, hardness)))) + m_pb[2,2]) * (m_pb[2,1] - (log(max(0, min(400, hardness))) * m_pb[2,3])),
                  CharacteristicName == "LEAD" & Desig_Use == "AWEDW"  ~ exp(m_pb[3,4] * (log(max(0, min(400, hardness)))) + m_pb[3,2]) * (m_pb[3,1] - (log(max(0, min(400, hardness))) * m_pb[3,3])),
                  CharacteristicName == "LEAD" & Desig_Use == "AWW"  ~ exp(m_pb[4,4] * (log(max(0, min(400, hardness)))) + m_pb[4,2]) * (m_pb[4,1] - (log(max(0, min(400, hardness))) * m_pb[4,3])),
                  
                  # Not sure about m_zn[i,3] - zeros as in NA in these formulas, or should they be in the log max 0's place?
                  CharacteristicName == "ZINC" & Desig_Use == "AWC"  ~ exp(m_zn[1,4] * (log(max(0, min(400, hardness)))) + m_zn[1,2]) * m_zn[1,1],
                  CharacteristicName == "ZINC" & Desig_Use == "AWE" ~ exp(m_zn[2,4] * (log(max(0, min(400, hardness)))) + m_zn[2,2]) * m_zn[2,1],
                  CharacteristicName == "ZINC" & Desig_Use == "AWEDW"  ~ exp(m_zn[3,4] * (log(max(0, min(400, hardness)))) + m_zn[3,2]) * m_zn[3,1],
                  CharacteristicName == "ZINC" & Desig_Use == "AWW"  ~ exp(m_zn[4,4] * (log(max(0, min(400, hardness)))) + m_zn[4,2]) * m_zn[4,1]
                  
            )) %>% 
            
            # Calculate Result Dependent Standard (Original Hard Coded)
            # mutate(acute_standard = case_when(
            #       
            #       CharacteristicName == "CADMIUM" & Desig_Use == "AWC"  ~  exp(1.0166 * log(max(0, min(400, hardness))) - 3.924) * (1.136672 - log(max(0, min(400, hardness))) * 0.041838),
            #       CharacteristicName == "CADMIUM" & Desig_Use == "AWE" ~ exp(1.128 * log(max(0, min(400, hardness))) - 0.969) * (1.136672 - log(max(0, min(400, hardness))) * 0.041838),
            #       CharacteristicName == "CADMIUM" & Desig_Use == "AWEDW"  ~ exp(1.128 * log(max(0, min(400, hardness))) - 3.6867) * (1.136672 - log(max(0, min(400, hardness))) * 0.041838),
            #       CharacteristicName == "CADMIUM" & Desig_Use == "AWW"  ~  exp(1.128 * log(max(0, min(400, hardness))) - 3.6867) * (1.136672 - log(max(0, min(400, hardness))) * 0.041838),
            #       
            #       CharacteristicName == "COPPER" & Desig_Use == "AWC"  ~  exp(0.9422 * (log(max(0, min(400, hardness)))) - 1.702) * 0.96,
            #       CharacteristicName == "COPPER" & Desig_Use == "AWE" ~	exp(0.9422 * (log(max(0, min(400, hardness)))) - 1.1514) * 0.96,
            #       CharacteristicName == "COPPER" & Desig_Use == "AWEDW"  ~ exp(0.9422 * (log(max(0, min(400, hardness)))) - 1.702) * 0.96,
            #       CharacteristicName == "COPPER" & Desig_Use == "AWW"  ~  exp(0.9422 * (log(max(0, min(400, hardness)))) - 1.702) * 0.96,
            #       
            #       CharacteristicName == "LEAD" & Desig_Use == "AWC"  ~ exp(1.273 * (log(max(0, min(400, hardness)))) - 1.46) * (1.46203 - (log(max(0, min(400, hardness))) * 0.145712)),
            #       CharacteristicName == "LEAD" & Desig_Use == "AWE" ~ exp(1.273 * (log(max(0, min(400, hardness)))) - 0.7131) * (1.46203 - (log(max(0, min(400, hardness))) * 0.145712)),
            #       CharacteristicName == "LEAD" & Desig_Use == "AWEDW"  ~ exp(1.273 * (log(max(0, min(400, hardness)))) - 1.46) * (1.46203 - (log(max(0, min(400, hardness))) * 0.145712)),
            #       CharacteristicName == "LEAD" & Desig_Use == "AWW"  ~ exp(1.273 * (log(max(0, min(400, hardness)))) - 1.46) * (1.46203 - (log(max(0, min(400, hardness))) * 0.145712)),
            #       
            #       CharacteristicName == "ZINC" & Desig_Use == "AWC"  ~ exp(0.8473 * (log(max(0, min(400, hardness)))) + 0.884) * 0.978,
            #       CharacteristicName == "ZINC" & Desig_Use == "AWE" ~ exp(0.8473 * (log(max(0, min(400, hardness)))) + 3.1342) * 0.978,
            #       CharacteristicName == "ZINC" & Desig_Use == "AWEDW"  ~ exp(0.8473 * (log(max(0, min(400, hardness)))) + 0.884) * 0.978,
            #       CharacteristicName == "ZINC" & Desig_Use == "AWW"  ~ exp(0.8473 * (log(max(0, min(400, hardness)))) + 0.884) * 0.978
            #       
            # )) %>%
            
            # Calculate Ratio
            mutate(metal_ratio = ResultMeasureValue / acute_standard) 
      
     
      
      # Find Max Toxicity
      df_out_max_ratio <- 
            
            df_out %>% 
            
            ungroup() %>% 
            
            # Pull Max
            select(-ActivityStartDate, -ActivityStartTime.Time, -ActivityDepthHeightMeasure.MeasureValue) %>% 
            group_by(WBID, Desig_Use, CharacteristicName, my_year, week_of_year) %>% 
            mutate(max_ratio = max(metal_ratio)) %>% 
            filter(metal_ratio == max_ratio) 
            
      
      # Extract Metals (Max Ratio)
      df_out <- df_out %>% 
            select(WBID, Desig_Use, CharacteristicName, my_year, week_of_year, ResultMeasureValue, metal_ratio) %>% 
            inner_join(df_out_max_ratio) %>% 
            # Some Reps Even After Max Aggregation
            unique()
      
      
      return(df_out)
      
}
