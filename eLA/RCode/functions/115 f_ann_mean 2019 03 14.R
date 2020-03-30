# Script to Run Impairment Logic i (Nitrogen and Phosphorus Ann Mean)

# df_in <- df_i

f_ann_mean <- function(df_in){
      
      # Add Month
      df_in <- df_in %>% 
            mutate(my_date = ymd(paste0(my_year,"-01-01")) + weeks(week_of_year - 1)) %>% 
            mutate(my_month = month(my_date)) 
      
      # Years with >= 3 Months Data
      df_years <- df_in %>% 
            
            # Find Unique Occurances
            select(WBID, CharacteristicName, Desig_Use, my_year, my_month) %>% 
            unique() %>% 
      
            # Count N Months in Year
            group_by(WBID, CharacteristicName, Desig_Use, my_year) %>% 
            summarise(n_in_year = n()) %>% 
            
            # Require at Least 3 Months
            filter(n_in_year >= 3) %>% 
            select(-n_in_year)
      
      # Filter and Aggregate 
      df_out <- df_in %>% 
            
            # Only Keep If 3 Months
            inner_join(df_years) %>% 
            
            # Average within Month
            group_by(WBID, CharacteristicName, Desig_Use, ResultSampleFractionText, Condition, Method, my_year, my_month, standard) %>% 
            summarise(ResultMeasureValue = mean(ResultMeasureValue)) %>% 
            
            # Average Within the Year
            group_by(WBID, CharacteristicName, Desig_Use, ResultSampleFractionText, Condition, Method, my_year, standard) %>% 
            summarise(ResultMeasureValue = mean(ResultMeasureValue))
            
            return(df_out)
      
}



