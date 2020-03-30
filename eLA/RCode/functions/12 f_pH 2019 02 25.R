# pH

# If one or more repetitions have "fld" or "field", drop all repetitions that don't have "fld" or "field"
# If no repetitions have "fld" or "field", keep all repetitions 
# Then aggregate w/ the negative log mean.

# Inputs:   data frame of test results
# Output:   data frame for temperature results

# df_in <- df_pH

f_pH <- function(df_in){
      
      # Find field entries (note: there are no non-detect pH)
      df_field_id <- df_in %>% 
            filter(grepl("FLD|FIELD", toupper(ResultAnalyticalMethod.MethodName))) %>% 
            select(WBID, CharacteristicName, Desig_Use, Condition,
                   ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% unique()
      
      # Find Non Field Reps Associated with Samples That Do Have Field Reps - These are guys to exclude (cuz they FLD)
      df_x <- df_in %>% 
            inner_join(df_field_id) %>% 
            filter(!grepl("FLD|FIELD", toupper(ResultAnalyticalMethod.MethodName)))
            
      # Remove From Data
      df_in <- df_in %>% anti_join(df_x)
      
      # Aggregate 
      df_out <- df_in %>% 
            group_by(WBID, CharacteristicName, Desig_Use, Condition, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
            summarise(ResultMeasureValue = f_neg_log_10_mean(ResultMeasureValue))
      
      return(df_out)
      
}



