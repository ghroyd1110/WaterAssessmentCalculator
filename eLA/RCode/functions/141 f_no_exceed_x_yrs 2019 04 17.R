# Function to ensure attain compliance - no exceedences in past X years

# Inputs: 

      # data frame with exceedences


# Output: data frame with no exceed attainment tags

# df_in <- df_b_exceed
# my_end_date <- as.Date("2017-06-30")
# n_yrs = 3
# exceed_col = "stnd_exceed"

# df_in <- df_b_exceed

f_no_exceed_x_yrs <- function(df_in, my_end_date, n_yrs = 3, exceed_col = "stnd_exceed"){
     
      # Standardize Exceed Column
      names(df_in)[names(df_in) == exceed_col] <- "exceed_col"
      
      # Get Start Date
      start_date <- my_end_date - 365.25 * n_yrs 
     
      # Pull and Check
      df_out <- df_in %>% 
            
            # Create Date as First Day of Week
            mutate(my_date = ymd(paste0(my_year,"-01-01")) + weeks(week_of_year - 1)) %>%  
            
            # Filter n_yrs
            filter(my_date >= start_date & my_date <= my_end_date) %>% 
            
            # Determine Attainment
            group_by(WBID, CharacteristicName, Desig_Use) %>% 
            summarise(n_samples = n(), n_exceeds = sum(exceed_col)) %>% 
            
            # Reverse: 1 = attain, 0 = not attain
            # Net Net: attain_exceeds = 1 if attaining, 0 if not attaining
            mutate(attain_exceeds = ifelse(n_exceeds == 0, 1, 0)) %>% 
            select(-n_exceeds)
           
      
      return(df_out)
      
}