# Function to Calculate Core Seasonals

# df_in <- df_b_exceed

f_core_season <- function(df_in){
      
      # Filter b, c, f 3 yrs from user end date
      
      df_out <- df_in %>%
            
            # Just the Required Columns
            dplyr::select(WBID, CharacteristicName, week_of_year) %>%
            
            # ID Quarter
            mutate(Qtr = case_when(
                  
                  week_of_year <= 13 ~ 1,
                  week_of_year <= 26 ~ 2,
                  week_of_year <= 39 ~ 3,
                  week_of_year > 39 ~ 4
                  
            )) %>% 
            
            # Prep for Spread
            dplyr::mutate(Tag = 1) %>% 
            dplyr::select(-week_of_year) %>% 
            unique() %>% 
            
            # Make Wide (to Track Each Quarter)
            spread(key = Qtr, value = Tag, fill = 0) %>%
            dplyr::rename(Q1 = 3, Q2 = 4, Q3 = 5, Q4 = 6) %>%
            dplyr::mutate(Core_Seas_Cnt = Q1+Q2+Q3+Q4)
      
      return(df_out)
      
}

