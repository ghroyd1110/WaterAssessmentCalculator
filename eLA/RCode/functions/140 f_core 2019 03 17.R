# Function to ensure compliance with core seasonal (for attainment)

# Inputs: 

      # data frame with exceedences
      # data frame summarizing # of seasons
      # data frame identifying core seasonal parameters


# Output: data frame with core seasonal attainment tags

# df_in <- df_c_exceed

f_core <- function(df_in, df_core_indicator_in = df_core_indicator, df_core_season_in = df_core_season){
      
      df_out <- df_in %>% 
            
            # Get WBID and Parameters
            select(WBID, CharacteristicName, Desig_Use) %>% 
            unique() %>% 
            
            # Is it a core parameter
            left_join(df_core_indicator_in) %>% 
            mutate(is_core = ifelse(is.na(Core), 0, 1)) %>% 
            
            # Does it have samples to meet core seasonal requirement
            left_join(df_core_season_in) %>% 
            mutate(meets_core = ifelse(Core_Seas_Cnt >= 3, 1, 0)) %>% 
            
            # Attains Core
            mutate(attains_core = case_when(
                  
                  is_core == 1 & meets_core == 1 ~ 1,
                  is_core == 1 & meets_core == 0 ~ 0,
                  is_core == 0 ~ NA_real_

            ))
            
      
      return(df_out)
      
}