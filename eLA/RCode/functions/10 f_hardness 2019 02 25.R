# Hardness

# This function extracts and prioritizes/calculates hardness values on a weekly basis; serves as prep for result dependent metal standards

# Inputs:   data frame of test results
# Output:   data frame for hardness results


# Hardness is Simply First in Order of List

# Dissolved Hardness, ca, mg
# Dissolved Total Hardness
# Dissolved Hardness, non-carbonate
# Total total hardness
# Total hardness ca, mg
# Total hardness non-carbonate
# Any other hardness

# df_in <- df_hardness

f_hardness <- function(df_in){
      
    
      # Non Detect
      df_out <-  f_non_detect_simple(df_in)
      
      df_out <- df_out %>% 
            
            # To prioritize Hardness, modify to ordered factor
            # Note: guessing at text strings since not all hardness measures found in data at this point...
            mutate(hardness_priority = case_when(
                  
                  ResultSampleFractionText == "DISSOLVED" & CharacteristicName == "HARDNESS, CA, MG" ~ 1,
                  ResultSampleFractionText == "DISSOLVED" & CharacteristicName == "TOTAL HARDNESS" ~ 2,
                  ResultSampleFractionText == "DISSOLVED" & CharacteristicName == "HARDNESS, NON-CARBONATE" ~ 3,
                  
                  ResultSampleFractionText == "TOTAL" & CharacteristicName == "TOTAL HARDNESS" ~ 4,                  
                  ResultSampleFractionText == "TOTAL" & CharacteristicName == "HARDNESS, CA, MG" ~ 5,
                  ResultSampleFractionText == "TOTAL" & CharacteristicName == "HARDNESS, NON-CARBONATE" ~ 6,
                  
                  # Not Sure About This Order ...
                  is.na(ResultSampleFractionText) & CharacteristicName == "HARDNESS, CA, MG" ~ 7,
                  is.na(ResultSampleFractionText) & CharacteristicName == "TOTAL HARDNESS" ~ 8,
                  is.na(ResultSampleFractionText) & CharacteristicName == "HARDNESS, NON-CARBONATE" ~ 9,
                  
                  TRUE ~ 10
                  
            ))
      
      # Select Top Priority Hardness
      df_out <- df_out %>% 
            arrange(WBID, Desig_Use, Condition, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue, hardness_priority) %>% 
            group_by(WBID, Desig_Use, Condition, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
            mutate(min_priority = min(hardness_priority)) %>% 
            filter(hardness_priority == min_priority) %>% 
            mutate(CharacteristicName = "HARDNESS") %>% 
            select(WBID, Desig_Use, Condition, ResultSampleFractionText, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue, ResultMeasureValue)
      
      # Aggregate 
      df_out <- df_out %>% 
            group_by(WBID, Desig_Use, Condition, 
                     ResultSampleFractionText, # Do We Need to Keep This Variable?
                     ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
            summarise(ResultMeasureValue = mean(ResultMeasureValue)) %>% 
            mutate(CharacteristicName = "HARDNESS")
     
      # Convert to UG/L Which is What Metals Standards Need to Be...
      # df_out <- df_out %>% mutate(ResultMeasureValue = 1000 * ResultMeasureValue)
      
       
      return(df_out)
      
}



