# This function calculates AMMONIA from AMMONIA-NITROGEN and AMMONIA AND AMMONIUM

# Inputs:   data frame 
# Output:   data frame without excluded Ammonia and Ammonium samples

# To calculate ammonia, use "Ammonia-nitrogen" first
# If not present then use "Ammonia and ammonium"
# Do not add the two together

# Non-Detect logic:  (Note: no standards yet because Ammonia has a result dependent standard)
      # If Detection Limit Present, Use 1/2 Detection Limit
      # If Detection Limit NOT Present, Use 0
      # If all repetitions are non-detect, then use max detection limit (and associated rule)

# df_in <- df_ammonia

f_calculate_ammonia <- function(df_in){
     
      # Data ====
      
      df_in <- df_in %>% 
            
            # Key Var to ID Samples
            select(WBID,
                   Desig_Use, 
                   CharacteristicName,
                   Condition, 
                   ResultDetectionConditionText,
                   ActivityStartDate,
                   ActivityStartTime.Time,
                   ActivityDepthHeightMeasure.MeasureValue,
                   ResultMeasureValue,
                   DetectionQuantitationLimitMeasure.MeasureValue # Need to Keep for Nitrogen Calculations (Future Step)
            ) 
      
      # Non Detect ====
      df_in <- f_non_detect_simple(df_in)
      
      # Split by Parameter, Then Join
      df_an <- df_in %>% 
            filter(CharacteristicName == "AMMONIA-NITROGEN") %>% 
            select(-ResultMeasureValue, -DetectionQuantitationLimitMeasure.MeasureValue) %>% 
            rename(`AMMONIA-NITROGEN` = CharacteristicName) %>% 
            unique() # To Take Care of Repeated Measures
      
      df_aa <- df_in %>% 
            filter(CharacteristicName == "AMMONIA AND AMMONIUM") %>% 
            select(-ResultMeasureValue, -DetectionQuantitationLimitMeasure.MeasureValue) %>% 
            rename(`AMMONIA AND AMMONIUM` = CharacteristicName) %>% 
            unique() # To Take Care of Repeated Measures
      
      # Join
      df_join <- full_join(df_an, df_aa)
      
      # df exclude
      df_exclude <- df_join %>% 
            
            # If Data for Both, Do Not Include Ammonia and Ammonium (Use Ammonia Nitrogen) - i.e. exclude
            mutate(exclude_me = ifelse(!is.na(`AMMONIA-NITROGEN`) & !is.na(`AMMONIA AND AMMONIUM`), 1, 0)) %>% 
            
            filter(exclude_me == 1) %>% 
            
            select(WBID,
                   Desig_Use, 
                   CharacteristicName = `AMMONIA AND AMMONIUM`,
                   Condition, 
                   # ActivityIdentifier, 
                   ActivityStartDate,
                   ActivityStartTime.Time,
                   ActivityDepthHeightMeasure.MeasureValue,
                   exclude_me)
      
      # Keep Only Prioritized Ammonia - Retain Name as Ammonia-Nitrogen (to Compare to Standards) 
      df_out <- left_join(df_in, df_exclude) %>% 
            mutate(exclude_me = ifelse(is.na(exclude_me), 0, exclude_me)) %>% 
            filter(exclude_me != 1) %>% 
            select(-exclude_me) %>% 
            mutate(CharacteristicName = "AMMONIA-NITROGEN") 
            
      # Aggregate Repetitions
      df_out <- df_out %>% group_by(WBID,
                        Desig_Use,
                        CharacteristicName,
                        Condition,
                        # ActivityIdentifier,
                        ActivityStartDate,
                        ActivityStartTime.Time,
                        ActivityDepthHeightMeasure.MeasureValue) %>%
            summarise(ResultMeasureValue = mean(ResultMeasureValue))     
                    
      
      return(df_out)
      
}
