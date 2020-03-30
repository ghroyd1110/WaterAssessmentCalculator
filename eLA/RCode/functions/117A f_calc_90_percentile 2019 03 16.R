# Function to Calculate and Store 90th Percentile Samples from Date Frame

# Input: data frame 
# Output: data frame with 90th Percentile Result

# df_in <- l_df[[1]]

f_calc_90_percentile <- function(df_in){
      
      df_temp <- df_in
      
      # Create NULL Output 
      df_out <- data_frame(
            
            WBID = as.character(), CharacteristicName = as.character(), Desig_Use = as.character(), Method = as.character(), 
            ResultMeasureValue = as.numeric(), standard = as.numeric(), my_date = as.Date(character()) , end_date = as.Date(character())
            
      )
      
      repeat{
            
            # Get Starting Date
            start_date <- min(df_temp$my_date)
            end_date <- start_date + 100
            
            # Pull Data (Based on 100 Days)
            df_sub <- df_temp %>% filter(my_date >= start_date & my_date <= end_date)
            
            # Test Min 10 Samples
            n_samples <- nrow(df_sub)
            
            # If < 10 Samples, Adjust: Pull to Get 10 Samples (or < 10 Samples left, NULL)
            if(n_samples < 10 & nrow(df_temp) >= 10){
                  
                  df_sub <- df_temp %>% slice(1:10)
                  
            } else if(n_samples < 10 & nrow(df_temp) > 10){
                  
                  df_sub <- NULL
                  
            }
            
            # Find 90th Percentile
            p90 <- quantile(x = df_sub$ResultMeasureValue, probs = 0.9)
            
            # Extract 1st Row and Overwrite with Result; Add End Date (In Case Need to Track)
            df_out1 <- df_sub %>% 
                  slice(1) %>% 
                  select(-my_year, -week_of_year) %>% 
                  mutate(ResultMeasureValue = p90) %>% 
                  bind_cols(
                        df_sub %>% slice(nrow(df_sub)) %>% select(end_date = my_date)
                  )
            
            # Add to Output
            df_out <- bind_rows(df_out, df_out1)
            
            # Remove Data from Main Table
            df_temp <- df_temp %>% anti_join(df_sub %>% select(-ResultMeasureValue))
      
            # print(df_out)
            
            # Stop When No More Data
            if(nrow(df_temp) < 10){break}
            
      }
      
            return(df_out)
      
}

