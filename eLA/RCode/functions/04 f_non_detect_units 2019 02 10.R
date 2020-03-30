
# This function adds units to non detect results

# Inputs:   data frame and columns to ID parameter by name, units of measure to check/convert, and result value to check/convert
# Output:   data frame with standardized units of measure

# df_in <- df
# df_stnd_in <- df_standards
# name0 = "CharacteristicName"
# units0 = "ResultMeasure.MeasureUnitCode"




f_non_detect_units <- function(df_in, 
                               df_stnd_in,
                              name0 = "CharacteristicName", 
                              units0 = "ResultMeasure.MeasureUnitCode"){
      
      # Change column names to generic names
      df_in <- df_in %>% rename(my_name = name0, my_units = units0)
      
      
      # If sample is na units and results are detected, toss it
      df_in <- df_in %>% 
            
            # ID Rows with Value but NO Units
            mutate(exclude_me = case_when(
                  
                  # We have results
                  ResultMeasureValue >= 0 & 
                        
                        # No Detection Limits Text (i.e. Not > or < Detection Limit)
                        !ResultDetectionConditionText %in% c("NOT DETECTED", "PRESENT ABOVE QUANTIFICATION LIMIT", "PRESENT BELOW QUANTIFICATION LIMIT") &
                        
                        # No Units
                        is.na(my_units) ~ 1,
                  
                  TRUE ~ 0
            )) %>% 
            
            # Remove Those Rows
            filter(exclude_me == 0) %>% 
            select(-exclude_me)
      
    
     
      
      
      
      # Add Standards Units to Data Frame if Non Detect and No Result Units, But Has Standards Units
      df_in <- left_join(
            
            df_in, 
            
            # Get standard units
            df_stnd_in %>% 
                  group_by(substance_name, unit_name) %>% 
                  summarise(N=n()) %>% 
                  # Note: E COLI in CFU/100ML, Do NOT Use MPN/100ML
                  filter(unit_name != "MPN/100ML") %>% 
                  select(-N),
            
            by = c("my_name" = "substance_name")
            
      ) 
      
      
      # If sample is not na, but the detection limit is na:
            # If non detect – toss
            # If detected – keep
      
      df_in <- df_in %>% 
            
            # ID Exclustions
            mutate(exclude_me = case_when(
                  
                  !is.na(ResultMeasureValue) & 
                        ResultDetectionConditionText %in% c("NOT DETECTED", "PRESENT ABOVE QUANTIFICATION LIMIT", "PRESENT BELOW QUANTIFICATION LIMIT") &
                        is.na(DetectionQuantitationLimitMeasure.MeasureValue) ~ 1,
                  TRUE ~ 0
                  
            )) %>% 
            
            # Remove
            filter(exclude_me == 0) %>% 
            select(-exclude_me)
      
      # Apply Units to non detect - If Not Detected and No Units, Then Use Standard Units
      df_units <- df_in %>% 
            
            mutate(my_units = case_when(
                  
                  # If Not Detected and No Units, Then Use Standard Units
                  ResultDetectionConditionText %in% c("NOT DETECTED", 
                                                      "PRESENT BELOW QUANTIFICATION LIMIT",
                                                      # Added above for E Coli - assume true for all
                                                      "PRESENT ABOVE QUANTIFICATION LIMIT") & is.na(my_units) ~ unit_name,
                  
                  
                  # Everything Else Stays as Is
                  TRUE ~ my_units
                  
            )) %>% 
            
            select(-unit_name)
     
      # Change Names Back to Original Names
      names(df_units)[names(df_units) == "my_name"] <- name0
      names(df_units)[names(df_units) == "my_units"] <- units0
      
      return(df_units)
      
}