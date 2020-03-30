
# This function converts handles non detect transformations

# Inputs:   data frame 
# Output:   data frame with transformed non detect values

# df_in <- df9

f_non_detect <- function(df_in, df_standards = df_standards){

      
      # Join Data with Standards
      df_out <- left_join(
            
            df_in,
            
            df_standards %>% select(substance_name, 
                                    # cas_qualifier_name,
                                    desig_use, 
                                    acute_chronic,
                                    standard, 
                                    stnd_unit_name = unit_name,
                                    Method # NEED Method - there are different standards for different methods (phosphorus)
            ) %>% 
                  # If no standard, no sense in keeping it (can't compare to NA standard for non-detect)
                  filter(!is.na(standard)) %>% 
                  unique(),
            
            by = c("CharacteristicName" = "substance_name",
                   # "ResultSampleFractionText" = "cas_qualifier_name", # (Don't Need: Curt 2019 03 05)
                   "Desig_Use" = "desig_use",
                   "Condition" = "acute_chronic"
                   
            )
      ) 
      
      
      # Remove if Detection Limit Units Don't Match Result Measurement Units
      df_remove <- df_out %>% filter(ResultMeasure.MeasureUnitCode != DetectionQuantitationLimitMeasure.MeasureUnitCode) 
      df_out <- df_out %>% anti_join(df_remove)
      
      # Consolidate not detected and below limit
      df_out <- df_out %>% 
            mutate(ResultDetectionConditionText = ifelse(toupper(ResultDetectionConditionText) %in% c("NOT DETECTED", "PRESENT BELOW QUANTIFICATION LIMIT"),
                                                         "NOT DETECTED",
                                                         toupper(ResultDetectionConditionText)))
      
      df_out <- df_out %>% 
            
            # Any rows with "Not Reported" can be omitted - NA after Todd addressed in Dec 23 harmonization?
            # filter(!ResultDetectionConditionText %in% c("Not Reported")) %>% 
            
            # This adjusts for non-detect
            mutate(ResultMeasureValue = case_when(
                
                    
                  # Present Above Quantification Limit is treated as 1.5X the detection limit - NA after Dec 23 harmonization
                  # I think this should happen here for simplicity in managing code in future
                  ResultDetectionConditionText == "PRESENT ABOVE QUANTIFICATION LIMIT" ~ 
                        DetectionQuantitationLimitMeasure.MeasureValue * 1.5,
                  
                  # Not Detected, Limit < Standard; # Start simple - X/2 in all cases
                  ResultDetectionConditionText == "NOT DETECTED" & DetectionQuantitationLimitMeasure.MeasureValue <= standard ~
                        DetectionQuantitationLimitMeasure.MeasureValue/2,
                  
                  # Not Detected, Limit > Standard - Remove
                  ResultDetectionConditionText == "NOT DETECTED" & DetectionQuantitationLimitMeasure.MeasureValue > standard ~
                        -exp(10),
                  
                  # Not Detected and No Detection Limit - Remove
                  ResultDetectionConditionText == "NOT DETECTED" & is.na(DetectionQuantitationLimitMeasure.MeasureValue) ~
                        -exp(10),
                  
                  # All Others
                  TRUE ~ ResultMeasureValue
                  
            ))
      
            # Remove Non-Detect
            df_out <- df_out %>% filter(ResultMeasureValue != -exp(10))
           
      
      return(df_out)
      
}
