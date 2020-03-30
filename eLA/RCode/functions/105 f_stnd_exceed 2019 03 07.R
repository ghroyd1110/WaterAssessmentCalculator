# This function compares weekly samples to standards and tags exceedence

# Inputs:   data frame 
# Output:   data frames exceedences

# df_in <- df_a

f_stnd_exceed <- function(df_in){
      
      # Max and Min 
      df_out <- df_in %>% 
            
            mutate(stnd_exceed = case_when(
                  
                  ResultMeasureValue > standard & Method %in% c("max", "ssmax", "90p", "annmean") ~ 1,
                  ResultMeasureValue < standard & Method == "min" ~ 1,
                  
                  TRUE ~ 0
                  
            ))
      

      return(df_out)
      
}
