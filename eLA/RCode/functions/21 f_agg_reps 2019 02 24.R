# This function aggregates repeated measures

# Inputs:   data frame 
# Output:   data frame with aggregated reps

# df_in <- df10

f_agg_reps <- function(df_in){
      
      # Reduced Subset for Aggregation
      df_out <- df_in %>% group_by(
            
            # Defines Sample
            WBID, CharacteristicName, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue,
            
            # Qualifiers
            ResultSampleFractionText,
            Condition
            
      ) %>% summarise(ResultMeasureValue = mean(ResultMeasureValue))
     
      return(df_out)
      
}
