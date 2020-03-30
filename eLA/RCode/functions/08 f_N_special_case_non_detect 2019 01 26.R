
# This function removes non-detect for TKN and organic NITROGEN aggregates

# Inputs:   data frame 
# Output:   data frame of N results


# df_in <- df_N4[2][[1]]

# Use 1/2 sum(detection limits) for non-detect

f_N_special_case_non_detect <- function(df_in){

      # Calculate Detection Limits:  1/2 sum of detection limits
      df_new_detect_limits <- df_in %>% 
            
            # Get Unique Value for Each Parameter - Otherwise it is a function of the number of rows!
            group_by(WBID, CharacteristicName, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>%
            summarise(DetectionQuantitationLimitMeasure.MeasureValue = mean(DetectionQuantitationLimitMeasure.MeasureValue, na.rm = TRUE)) %>% # Because multiple observations
            
            # Calculate Detection Limit
            group_by(WBID, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
            summarise(DetectionQuantitationLimitMeasure.MeasureValue = 0.5 * sum(DetectionQuantitationLimitMeasure.MeasureValue)) %>% 
            mutate(CharacteristicName = "NITROGEN")
      
      # Compare Detection Limit to Standard - ID removals
      df_remove_me <- left_join(
                  
                  df_new_detect_limits,
                  df_standards,
                      
                      by = c("CharacteristicName" = "substance_name",
                             "Desig_Use" = "desig_use"
                      )              
                      
            ) %>% 
            mutate(remove_me = ifelse(DetectionQuantitationLimitMeasure.MeasureValue > standard, 1, 0)) %>% 
            filter(remove_me == 1) %>% 
            select(WBID, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue, remove_me)
      
      # Remove if Detect Limit > Standard
      df_in <- df_in %>% anti_join(df_remove_me)
      
      # Adjust Result Measures
      df_in <- df_in %>% 
            
            # Non Detect with NO Detect Limit --> 0
            mutate(ResultMeasureValue = ifelse(
                  ResultDetectionConditionText == "NOT DETECTED" & is.na(DetectionQuantitationLimitMeasure.MeasureValue), 
                  0, ResultMeasureValue)) %>%
            
            # Non Detect with Detect Limit --> 0.5 * Detect Limit
            mutate(ResultMeasureValue = ifelse(
                  ResultDetectionConditionText == "NOT DETECTED" & !is.na(DetectionQuantitationLimitMeasure.MeasureValue), 
                  0.5 * DetectionQuantitationLimitMeasure.MeasureValue, 
                  ResultMeasureValue)) %>% 
            
            # If Above Detection Limit --> 1.5 * Detect Limit (Assumes Detection Limit Value Available)
            mutate(ResultMeasureValue = ifelse(
                  ResultDetectionConditionText == "PRESENT ABOVE QUANTIFICATION LIMIT", 
                  1.5 * DetectionQuantitationLimitMeasure.MeasureValue & is.na(ResultMeasureValue), 
                  ResultMeasureValue)) 
            
      # Aggregate
      df_out <- df_in %>%
            
            # Get Unique Value for Each Parameter - Otherwise it is a function of the number of rows!
            group_by(WBID, CharacteristicName, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>%
            summarise(ResultMeasureValue = mean(ResultMeasureValue, na.rm = TRUE)) %>% # Because multiple observations
            
            # Calculate Detection Limit
            group_by(WBID, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
            summarise(ResultMeasureValue = sum(ResultMeasureValue))
           
       
      return(df_out)
      
}


# EOF ====
            
      
