
# This function deals with non-detect when standards aren't yet available

# Inputs:   data frame 
# Output:   data frame with adjusted ResultMeasureValues

f_non_detect_simple <- function(df_in){
      
      # Non Detect Logic
      df_out <- df_in %>% 
            
            mutate(
                  
                  ResultMeasureValue = case_when(
                        
                        # If Detection Limit Present, Use 1/2 Detection Limit
                        # ResultDetectionConditionText %in% c("NOT DETECTED", "PRESENT BELOW QUANTIFICATION LIMIT") &
                        #       DetectionQuantitationLimitMeasure.MeasureValue >= 0 ~ 0.5 * DetectionQuantitationLimitMeasure.MeasureValue,
                        
                        # If Detection Limit NOT Present, Use 0
                        # ResultDetectionConditionText %in% c("NOT DETECTED", "PRESENT BELOW QUANTIFICATION LIMIT") &
                        #       is.na(DetectionQuantitationLimitMeasure.MeasureValue) ~ 0,
                        
                        # Changed 2019 04 11: possible to get impairment on result dependent standards if high
                        # detection limits lead to result > standard when using 1/2 detection limit... use all 0's...
                        
                        # If NON DETECT Use 0
                        ResultDetectionConditionText %in% c("NOT DETECTED", "PRESENT BELOW QUANTIFICATION LIMIT") ~ 0,
                        
                        TRUE ~ ResultMeasureValue
                  )) 
      
      return(df_out)
      
}