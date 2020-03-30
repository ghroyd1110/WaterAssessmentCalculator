# Function to Calculate and Store 90th Percentile Samples from Date Frame

# Input: data frame 
# Output: data frame with 90th P

# df_in <- df_j

f_90_percentile <- function(df_in){
      
      l_df <- df_in %>% 
            
            # Subset to Critical Cols      
            select(WBID, CharacteristicName, Desig_Use, my_year, week_of_year, Method, ResultMeasureValue, standard) %>% 
            
            # Arrange Chronogically
            arrange(WBID, Desig_Use, my_year, week_of_year) %>% 
            
            # Create Date as First Day of Week
            mutate(my_date = ymd(paste0(my_year,"-01-01")) + weeks(week_of_year - 1)) %>% 
            
            # Split by Parameter x Desig Use
            # split(list(.$CharacteristicName, .$Desig_Use))
            # Need to Add WBID to Split ??? 2019 04 06
            split(list(.$WBID, .$CharacteristicName, .$Desig_Use))
      
      # Map Function to All Tibbles in List of Data Frames
      l_df_out <- l_df %>% map(~f_calc_90_percentile(.x))
      
      df_out <- l_df_out %>% bind_rows()

            return(df_out)
      
}



