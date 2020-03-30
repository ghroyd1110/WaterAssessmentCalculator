
# This function calculates NITROGEN

# Inputs:   data frame 
# Output:   data frame of N results

# Note: requires df_ammonia

# df_in <- df_N
# df_ammonia_in <- df_ammonia

f_nitrogen <- function(df_in = df_N, df_ammonia_in = df_ammonia){


# Nitrogen ====
      
      # Prioritization Logic
      # 1. If "Nitrogen" is present and given as total (not dissolved), then use that value and ignore all other forms of nitrogen
      # 2. If "Total Kjeldahl Nitrogen" is given, use the sum of that plus Nitrate and Nitrite (make sure units match).  All 3 must be present to use...
      # 3. If TKN (above) is not given, add "Organic nitrogen", ammonia (see above) to the nitrate/nitrite samples.  All 4 must be present to use...
      # 4. As a last case scenario, use "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)”
      
      # Note: if #2 or #3, use 1/2 sum(detection limits) for non-detect - must return all needed data for non-detect!
      
      # Data for Nitrogen ====
      df_in <- df_in %>% 
            
            # Just relevant columns
            select(WBID, CharacteristicName, Desig_Use,
                   ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue,
                   Condition, ResultDetectionConditionText,
                   ResultSampleFractionText, ResultMeasureValue, ResultMeasure.MeasureUnitCode,
                   DetectionQuantitationLimitMeasure.MeasureValue, DetectionQuantitationLimitMeasure.MeasureUnitCode)

      # 1. If "Nitrogen" is present and given as total (not dissolved), then use that value and ignore all other forms of nitrogen ====
      
            # Get Data - note: keep all repetitions for now - deal with at non-detect
            df_N_total <- df_in %>% 
                  filter(CharacteristicName == "NITROGEN") %>% 
                  filter(ResultSampleFractionText == "TOTAL") 
            
            # Non Detect Logic
            df_N_total <- f_non_detect_simple(df_N_total) 
            
            # Aggregate
            df_N_total <- df_N_total %>% 
                  group_by(WBID, CharacteristicName, Desig_Use, ResultSampleFractionText, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
                  summarise(ResultMeasureValue = mean(ResultMeasureValue))
            
            # ID Samples (to Remove)
            df_N_total_id <- df_N_total %>% ungroup() %>% 
                  select(WBID, Desig_Use, ResultSampleFractionText, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) 

            # Remove From Data Set (Regardless of Qualifier)
            df_in <- df_in %>% anti_join(df_N_total_id)
            
      # 2. If "Total Kjeldahl Nitrogen" is given, use the sum of that plus Nitrate and Nitrite ==== 
      # Make sure units match - this happens at unit conversions
      # All three (TKN, Nitrate, Nitrite) must be present to use; however they don't all need to be detected!
          
            # ID TKN Samples  
            df_TKN_id <- df_in %>% 
                  filter(grepl("KJELDAHL", CharacteristicName)) %>% 
                  select(WBID, CharacteristicName, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
                  unique() # OK for Unique Here, Because Just IDENTIFYING Which Samples Have TKN
            
            # Extract Relevant Samples/Data 
            df_TKN <- df_in %>%
                  inner_join(df_TKN_id %>% select(-CharacteristicName)) %>% 
                  filter(CharacteristicName %in% c("KJELDAHL NITROGEN", "NITRATE", "NITRITE")) 
            
            # Non Detect
            df_TKN <- f_non_detect_simple(df_TKN)
            
            # Aggregate Repetitions within CharacteristicName
            df_TKN <- df_TKN %>% 
                  group_by(WBID, CharacteristicName, Desig_Use, ResultSampleFractionText, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
                  summarise(ResultMeasureValue = mean(ResultMeasureValue))
            
            # Determine if All Three Present
            df_TKN_3 <- df_TKN %>% 
                  
                  # Make Single Row for Each CharacteristicParameter
                  group_by(WBID, Desig_Use, CharacteristicName, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
                  summarise(N = 1) %>%  
                  
                  # Determine How Many of the Three Parameters for Each Sample
                  group_by(WBID, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
                  summarise(N = sum(N)) %>% 
                  
                  # Pull Sample Only if All Three
                  filter(N == 3)
            
            # Keep only if All Three Present
            df_TKN <- df_TKN %>% inner_join(df_TKN_3 %>% select(-N))
            
            # Sum Up Parameters 
            # Q: Sum Up Even if Dissolved, Total?  Or Include ResultSampleFractionTest?  All Nitrogen standards are Total...
            df_TKN <- df_TKN %>% 
                  group_by(WBID, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
                  summarise(ResultMeasureValue = sum(ResultMeasureValue)) %>% 
                  mutate(CharacteristicName = "NITROGEN")
            
            # Remove From Data Set
            df_in <- df_in %>% anti_join(df_TKN_3 %>% select(-N))

      # 3. If TKN (above) is not given, add "Organic nitrogen", ammonia, and nitrate/nitrite ====
      # Need all 4 present to use...
            
            # CHECK UNITS
            # df8 %>% 
            #       filter(grepl("NIT|AMM", CharacteristicName)) %>% 
            #       group_by(CharacteristicName, ResultMeasure.MeasureUnitCode) %>% 
            #       summarise(N = n()) %>% print.data.frame()

  
            # Combine Left Over N Metrics with Previously Calculated Ammonia
            df_N_org_plus <- 
            
                  bind_rows(
                  
                        df_in %>% filter(CharacteristicName %in% c("ORGANIC NITROGEN", "NITRATE", "NITRITE")),
                        
                        # Ammonia Results in for Multiple Conditions - Only Need Once for Each Sample (In for Both Acute and Chronic, ...)
                        df_ammonia_in %>% 
                              ungroup() %>% 
                              select(-Condition) %>% 
                              group_by(WBID, CharacteristicName, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
                              summarise(ResultMeasureValue = mean(ResultMeasureValue))
                        
                  )
            
            # Non Detect
            df_N_org_plus <- f_non_detect_simple(df_N_org_plus)
            
            # Aggregate within Characteristic Name - Aggregate Repetitions
            df_N_org_plus <- df_N_org_plus %>% 
                  group_by(WBID, CharacteristicName, Desig_Use, ResultSampleFractionText, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
                  summarise(ResultMeasureValue = mean(ResultMeasureValue))
            
            # Determine if All Four Present
            df_N_org_plus_4 <- df_N_org_plus %>% 
                 
                  # Make Single Row for Each CharacteristicParameter
                  group_by(WBID, Desig_Use, CharacteristicName, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>%
                  summarise(N = 1) %>%
                  
                  # Determine Which Have all Four Present
                  group_by(WBID, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
                  summarise(N = sum(N)) %>% 
                  filter(N == 4)
            
            # Keep only if All Four Present
            df_N_org_plus <- df_N_org_plus %>% inner_join(df_N_org_plus_4 %>% select(-N))
            
            # Sum Up Parameters 
            df_N_org_plus <- df_N_org_plus %>% 
                  group_by(WBID, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
                  summarise(ResultMeasureValue = sum(ResultMeasureValue)) %>% 
                  mutate(CharacteristicName = "NITROGEN")
            
            # Remove From Data
            df_in <- df_in %>% anti_join(df_N_org_plus_4 %>% select(-N))
            
            
      # 4. As a last case scenario, use "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)” ====
            
            df_N_mixed <- df_in %>% 
                  filter(grepl("MIXED FORMS", CharacteristicName)) %>% mutate(CharacteristicName = "NITROGEN")
            
            # Non Detect
            df_N_mixed <- f_non_detect_simple(df_N_mixed)
            
            # Aggregate (Repetitions)
            df_N_mixed <- df_N_mixed %>% 
                  group_by(WBID, CharacteristicName, Desig_Use, ResultSampleFractionText, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
                  summarise(ResultMeasureValue = mean(ResultMeasureValue))
            
            

      # Combine ====
            
            df_out <- bind_rows(df_N_total, df_TKN, df_N_org_plus, df_N_mixed)
            
      # Aggregate ====
            
            df_out <- df_out %>% 
                  
                  # Add Year, Week of Year to Sample Test Results Data
                  mutate(my_year = year(ActivityStartDate)) %>% 
                  mutate(week_of_year = week(ActivityStartDate)) %>%
                  
                  # Aggregate
                  group_by(WBID, CharacteristicName, Desig_Use, ResultSampleFractionText, my_year, week_of_year) %>% 
                  summarise(ResultMeasureValue = mean(ResultMeasureValue))
                  
                  
            return(df_out)
            
}
            
            
# EOF ====
            
      
