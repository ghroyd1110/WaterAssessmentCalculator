# This function removes samples if dissolved > 100% total

# Inputs:   data frame 
# Output:   data frame with contradictions removed

# df_in <- df15

f_dissolved_gt_110_percent_total <- function(df_in){
      
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
            
            # Tag if Dissolved > 110% Total
            mutate(remove_me = ifelse(DISSOLVED > 1.1 * TOTAL, 1, 0)) %>% 
            
            # Remove if Tagged
            filter(remove_me == 1) %>% 
            
            # Clean Intermediate Column
            select(-DISSOLVED, -TOTAL)
      
      
      # Remove ====
      
      df_out <- df_in %>% 
            left_join(df_remove_me) %>% 
            replace_na(replace = list(remove_me = 0)) %>% 
            filter(remove_me != 1)
      
      df_out <- df_out %>% select(-remove_me)
      
      
      # If Repeated TOTAL and DISSOLVED and Standard is TOTAL 
      # (And there is never a time when there are both total and dissolved standards for the same designated use for a single paramter)
      # Then just Use Total, Drop Dissolved... (if no total, keep dissolved)
      
      # # What has Total
      # df_total <- df_DT %>% 
      #       filter(ResultSampleFractionText == "TOTAL") %>% 
      #       select(WBID, CharacteristicName, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue)
      # 
      # # What has Dissolved 
      # df_dissolved <- df_DT %>% 
      #       filter(ResultSampleFractionText == "DISSOLVED") %>% 
      #       select(WBID, CharacteristicName, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue, ResultSampleFractionText)
      # 
      # # What has Both
      # df_both <- inner_join(df_total, df_dissolved)
      # 
      # # Remove Dissolved Extras
      # df_DT <- df_DT %>% anti_join(df_both)
      
      
      return(df_out)
      
}
