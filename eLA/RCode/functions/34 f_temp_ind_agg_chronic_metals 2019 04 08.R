# This function aggregates temporally dependent samples for chronic metals

# Inputs:   data frame 
# Output:   data frames with chronic metals aggregated values

# df_metals_in = df_metals
# df_hardness_in = df_hardness
# df_coeff_in = df_coeff

f_temp_ind_agg_chronic_metals <- function(df_metals_in = df_metals, df_hardness_in = df_hardness, df_coeff_in = df_coeff){
      
# Metals ====
      
      df_metals_in <- df_metals_in %>% 
            
            # Weeks
            mutate(my_year = year(ActivityStartDate)) %>% 
            mutate(week_of_year = week(ActivityStartDate)) %>% 
      
            # Chronic only
            filter(Condition == "CHRONIC") 
      
      
      # Non Detect ====
      
      df_metals_in <- f_non_detect_simple(df_metals_in)
      
      # Core Columns
      df_metals_in <- df_metals_in %>% 
            
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
      
      
      
# Hardness Data ====
      
      df_hardness_in <- df_hardness_in %>% 
            
            ungroup() %>% 
            
            # Acute Only
            filter(Condition == "CHRONIC") %>% 
            
            # Weeks
            mutate(my_year = year(ActivityStartDate)) %>% 
            mutate(week_of_year = week(ActivityStartDate)) %>%
            
            select(
                  
                  WBID, 
                  Desig_Use, 
                  # CharacteristicName,
                  # ResultSampleFractionText,
                  # ActivityStartDate, 
                  # ActivityStartTime.Time, 
                  # ActivityDepthHeightMeasure.MeasureValue,
                  my_year,
                  week_of_year,
                  ResultMeasureValue
            ) 
      
      
# Metals Weekly Agg ====
      
      df_metals_in <- df_metals_in %>% 
            group_by(WBID, CharacteristicName, Desig_Use, my_year, week_of_year) %>% 
            summarise(ResultMeasureValue = mean(ResultMeasureValue))
            
# Hardness Weekly Agg ====
      
      df_hardness_in <- df_hardness_in %>% 
            group_by(WBID, Desig_Use, my_year, week_of_year) %>% 
            summarise(hardness = mean(ResultMeasureValue))
      
      
# Join Data for Metals and Hardness (in order to calculate ratio, max toxicity) ====
      
      df_out <- inner_join(
            df_metals_in,
            df_hardness_in 
      ) 
      
      # Get Coefficients
      f_m_coeff <- function(substance){
            
            m <- df_coeff_in %>% 
                  filter(substance_name == substance) %>% 
                  filter(Condition == "chronic") %>% 
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
      
      # Calculate standards
      df_out <- df_out %>% 
            
            rowwise() %>% 
            
            mutate(chronic_standard = case_when(
                  
                  CharacteristicName == "CADMIUM" & Desig_Use == "AWC"  ~  exp(m_cd[1,4] * log(max(0, min(400, hardness))) + m_cd[1,2]) * (m_cd[1,1] - log(max(0, min(400, hardness))) * m_cd[1,3]),
                  CharacteristicName == "CADMIUM" & Desig_Use == "AWEDW"  ~ exp(m_cd[2,4] * log(max(0, min(400, hardness))) + m_cd[2,2]) * (m_cd[2,1] - log(max(0, min(400, hardness))) * m_cd[2,3]),
                  CharacteristicName == "CADMIUM" & Desig_Use == "AWW"  ~  exp(m_cd[3,4] * log(max(0, min(400, hardness))) + m_cd[3,2]) * (m_cd[3,1] - log(max(0, min(400, hardness))) * m_cd[3,3]),
                  
                  CharacteristicName == "COPPER" & Desig_Use == "AWC"  ~  exp(m_cu[1,4] * (log(max(0, min(400, hardness)))) + m_cu[1,2]) * m_cu[1,1],
                  CharacteristicName == "COPPER" & Desig_Use == "AWEDW"  ~ exp(m_cu[2,4] * (log(max(0, min(400, hardness)))) + m_cu[2,2]) * m_cu[2,1],
                  CharacteristicName == "COPPER" & Desig_Use == "AWW"  ~  exp(m_cu[3,4] * (log(max(0, min(400, hardness)))) + m_cu[3,2]) * m_cu[3,1],

                  CharacteristicName == "LEAD" & Desig_Use == "AWC"  ~ exp(m_pb[1,4] * (log(max(0, min(400, hardness)))) + m_pb[1,2]) * (m_pb[1,1] - (log(max(0, min(400, hardness))) * m_pb[1,3])),
                  CharacteristicName == "LEAD" & Desig_Use == "AWEDW"  ~ exp(m_pb[2,4] * (log(max(0, min(400, hardness)))) + m_pb[2,2]) * (m_pb[2,1] - (log(max(0, min(400, hardness))) * m_pb[2,3])),
                  CharacteristicName == "LEAD" & Desig_Use == "AWW"  ~ exp(m_pb[3,4] * (log(max(0, min(400, hardness)))) + m_pb[3,2]) * (m_pb[3,1] - (log(max(0, min(400, hardness))) * m_pb[3,3])),
                 
                  # There are no coefficients for chronic Zinc - Leave with Original Numerical Values
                  CharacteristicName == "ZINC" & Desig_Use == "AWC"  ~ exp(0.8473 * (log(max(0, min(400, hardness)))) + 0.884) * 0.978,
                  CharacteristicName == "ZINC" & Desig_Use == "AWEDW"  ~ exp(0.8473 * (log(max(0, min(400, hardness)))) + 0.884) * 0.978,
                  CharacteristicName == "ZINC" & Desig_Use == "AWW"  ~ exp(0.8473 * (log(max(0, min(400, hardness)))) + 0.884) * 0.978
                  
                 
                  
            )) 
            
            # # Calculate Result Dependent Standard (Original Hard Wired)
            # mutate(chronic_standard = case_when(
            #       
            #       CharacteristicName == "CADMIUM" & Desig_Use == "AWC"  ~  exp(0.7409 * log(max(0, min(400, hardness))) - 4.719) * (1.101672 - log(max(0, min(400, hardness))) * 0.041838),
            #       CharacteristicName == "CADMIUM" & Desig_Use == "AWEDW"  ~ exp(0.7852 * log(max(0, min(400, hardness))) - 2.715) * (1.101672 - log(max(0, min(400, hardness))) * 0.041838),
            #       CharacteristicName == "CADMIUM" & Desig_Use == "AWW"  ~  exp(0.7852 * log(max(0, min(400, hardness))) - 2.715) * (1.101672 - log(max(0, min(400, hardness))) * 0.041838),
            #       
            #       CharacteristicName == "COPPER" & Desig_Use == "AWC"  ~  exp(0.8545 * (log(max(0, min(400, hardness)))) - 1.702) * 0.96,
            #       CharacteristicName == "COPPER" & Desig_Use == "AWEDW"  ~ exp(0.8545 * (log(max(0, min(400, hardness)))) - 1.702) * 0.96,
            #       CharacteristicName == "COPPER" & Desig_Use == "AWW"  ~  exp(0.8545 * (log(max(0, min(400, hardness)))) - 1.702) * 0.96,
            #       
            #       CharacteristicName == "LEAD" & Desig_Use == "AWC"  ~ exp(1.273 * (log(max(0, min(400, hardness)))) - 4.705) * (1.46203 - (log(max(0, min(400, hardness))) * 0.145712)),
            #       CharacteristicName == "LEAD" & Desig_Use == "AWEDW"  ~ exp(1.273 * (log(max(0, min(400, hardness)))) - 4.705) * (1.46203 - (log(max(0, min(400, hardness))) * 0.145712)),
            #       CharacteristicName == "LEAD" & Desig_Use == "AWW"  ~ exp(1.273 * (log(max(0, min(400, hardness)))) - 4.705) * (1.46203 - (log(max(0, min(400, hardness))) * 0.145712)),
            #       
            #       CharacteristicName == "ZINC" & Desig_Use == "AWC"  ~ exp(0.8473 * (log(max(0, min(400, hardness)))) + 0.884) * 0.978,
            #       CharacteristicName == "ZINC" & Desig_Use == "AWEDW"  ~ exp(0.8473 * (log(max(0, min(400, hardness)))) + 0.884) * 0.978,
            #       CharacteristicName == "ZINC" & Desig_Use == "AWW"  ~ exp(0.8473 * (log(max(0, min(400, hardness)))) + 0.884) * 0.978
            # )) %>%
            

      return(df_out)
      
}
