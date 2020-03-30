# This function removes Dissolved Oxygen samples from lakes when measures at depth > 1 M

# Inputs:   data frame 
# Output:   data frame without excluded dissolved oxygen samples

# df_in <- df

f_remove_DO_gt_1m <- function(df_in){
     
      df <- df_in %>% 
            mutate(exclude_me = ifelse(grepl("DISSOLVED OXYGEN", CharacteristicName) & 
                                             toupper(MonitoringLocationTypeName) == "LAKE" &
                                             ActivityDepthHeightMeasure.MeasureValue > 1,
                                       
                                       1,
                                       0
            )) %>% 
            
            filter(exclude_me == 0 | is.na(exclude_me)) %>% 
            select(-exclude_me)
      
      return(df)
      
}
