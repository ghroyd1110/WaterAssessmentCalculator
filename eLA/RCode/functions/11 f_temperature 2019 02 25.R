# Temperature

# This function extracts temperature, checks units, and aggregates

# Inputs:   data frame of test results
# Output:   data frame for temperature results



# df_in <- df_temperature

f_temperature <- function(df_in){
      
      # A Few Checks...
      # df_temperature$ResultMeasure.MeasureUnitCode %>% unique()
      # df_temperature$CharacteristicName %>% unique()
      # df_temperature$Condition %>% unique()
      # df_temperature %>% filter(is.na(ResultMeasureValue))
      
      
      # Aggregate 
      df_out <- df_in %>% 
            group_by(WBID, CharacteristicName, Desig_Use, Condition, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue) %>% 
            summarise(ResultMeasureValue = mean(ResultMeasureValue))
      
      return(df_out)
      
}



