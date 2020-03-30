# Function to deal with special case E COLI attainment

# Inputs: 

      # data frame with E Coli attain


# Output: data frame with adjusted E COLI Attain

# df_in <- df_attain

f_attain_e_coli_special_case <- function(df_in, df_f_impair_GT3YR = df_f_impair_GT3YR, df_core_season_3YR = df_core_season_3YR){
     
      # Claim to Attain... (No Exceeds in Past 3 Years)
      df_attain_special_case_e_coli <- df_in %>% 
            filter(grepl("COLI", CharacteristicName)) %>% 
            filter(attain == 1)
      
      # But Did You Exceed Greater than 3 Years Ago?
      
      df_attain_special_case_e_coli <- 
            df_attain_special_case_e_coli %>% 
            left_join(
                  
                  df_f_impair_GT3YR %>% ungroup() %>% 
                        select(WBID, CharacteristicName, Desig_Use, impaired) %>% 
                        filter(impaired == 1) %>% 
                        rename(impaired_gt3yr = impaired)
                  
            )
      
      # Observations where claim to attain, but exceed greater than 3 years ago...
      df_attain_special_case_e_coli <- df_attain_special_case_e_coli %>% 
            filter(attain == 1 & impaired_gt3yr == 1) %>% 
            select(WBID, CharacteristicName, Desig_Use)
      
      # Check these guys to see if they meet core past 3 years
      df_attain_special_case_e_coli <- df_attain_special_case_e_coli %>% 
            left_join(df_core_season_3YR %>% filter(grepl("COLI", CharacteristicName))) %>% 
            filter(Core_Seas_Cnt < 3) %>% 
            select(WBID, CharacteristicName, Desig_Use) %>% 
            mutate(fail = 1)
      
      # Change Attain to Fail
      df_out <- df_in %>% 
            left_join(df_attain_special_case_e_coli) %>% 
            mutate(attain = case_when(fail == 1 ~ 0, TRUE ~attain)) %>% 
            select(-fail)
      
      return(df_out)
      
}