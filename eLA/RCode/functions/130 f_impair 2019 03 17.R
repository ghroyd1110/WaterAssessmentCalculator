# Function to determine impairment

# Input: data frame with exceedences
# Output: data frame with impairments

# df_in <- df_b_exceed
# df_in <- df_test

f_impair <- function(df_in, n_exceed_limit = 2, past_x_yrs = 3, exceed_col = "stnd_exceed", my_end_date = input_end_date){
      
      df_out <- df_in %>% 
            
            # Create Date as First Day of Week
            mutate(my_date = ymd(paste0(my_year,"-01-01")) + weeks(week_of_year - 1)) %>%
            
            # Take Only Past X Years
            filter(my_date >= my_end_date - 365.25 * past_x_yrs & my_date <= my_end_date)
      
      df_out <- df_out %>% 
            rename("exceed_col" = exceed_col) %>% 
            group_by(WBID, CharacteristicName, Desig_Use, Condition, Method, impair_logic) %>% 
            summarise(n_exceedences = sum(exceed_col)) %>% 
            mutate(impaired = ifelse(n_exceedences >= n_exceed_limit, 1, 0))
      
      return(df_out)
      
}