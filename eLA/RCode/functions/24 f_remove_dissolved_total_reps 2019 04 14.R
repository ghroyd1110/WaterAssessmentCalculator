# If Repeated TOTAL and DISSOLVED and Standard is TOTAL 
# (And there is never a time when there are both total and dissolved standards for the same designated use for a single paramter)
# Then just Use Total, Drop Dissolved... (if no total, keep dissolved)

# Inputs:   data frame 
# Output:   data frame with duplicates (Dissolved, Total) removed

# df_in <- df16

f_remove_dissolved_total_reps <- function(df_in){
      
      df_remove_me <- df_in %>% 
            
            select(
                  WBID,
                  CharacteristicName,
                  Desig_Use,
                  ActivityStartDate,
                  ActivityStartTime.Time,
                  ActivityDepthHeightMeasure.MeasureValue,
                  Condition,
                  ResultSampleFractionText,
                  ResultMeasureValue
            ) %>% 
            
            filter(ResultSampleFractionText %in% c("DISSOLVED", "TOTAL")) %>% 
            
            spread(key = ResultSampleFractionText, value = ResultMeasureValue) %>% 
            
            # Must Both Be Present
            filter(!is.na(DISSOLVED) & !is.na(TOTAL)) %>% 
            
            # Assign ResultSampleFractionText
            mutate(ResultSampleFractionText = "DISSOLVED") %>% 
            
            # Remove Extra Columns
            select(-DISSOLVED, - TOTAL)
            
 
      # Remove ====
      
      df_out <- df_in %>% anti_join(df_remove_me) 
      
      return(df_out)
      
}
