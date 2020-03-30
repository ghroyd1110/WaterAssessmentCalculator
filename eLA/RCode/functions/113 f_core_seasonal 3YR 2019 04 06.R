# Function to Calculate Core Seasonals

# df_in <- df_b_exceed

f_core_season_3YR <- function(df_in, end_date = input_end_date){
      
      df_out <- df_in %>%
            
            # Just the Required Columns
            dplyr::select(WBID, CharacteristicName, week_of_year, my_year) %>%
            
            # Create Date as First Day of Week
            mutate(my_date = ymd(paste0(my_year,"-01-01")) + weeks(week_of_year - 1)) %>%  
            
            # Subset Last 3 Years
            filter(my_date > end_date - 365.25*3) %>% 
            select(-my_date, -my_year) %>% 
            
            
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

