# Function to deal with special dissolved oxygen impairment logic

# Inputs: 

      # data frame with impairments


# Output: data frame DO impairments

# df_in <- df_impair_do

f_impair_do_special_case <- function(df_in){
     
      # Logic
      df_out <- df_in %>% 
            select(WBID, CharacteristicName, Desig_Use, impaired) %>% 
            spread(key = CharacteristicName, value = impaired) %>% 
            
            # Oxygen Must Be Impaired on Both to Be Impairment
            mutate(do_impaired = 
                         
                         case_when(
                               
                               `DISSOLVED OXYGEN (DO)` == 1 & `DISSOLVED OXYGEN SATURATION` == 1 ~ 1,
                               TRUE ~ 0
                               
                         )) %>% 
            
            select(WBID, Desig_Use, do_impaired)
      
      # Rejoin with Original
      df_out <- df_out %>% 
            inner_join(df_in) %>% 
            
            # Keep DO (Unless Sat is Only Sample Available)
            arrange(WBID, Desig_Use, CharacteristicName) %>% 
            group_by(WBID, Desig_Use) %>% 
            slice(1) %>% 
            
            # Overwrite DO Impairment
            mutate(impaired = do_impaired) %>% 
            select(-do_impaired) %>% 
            
            # Change Name to Single DO CharaacteristicName
            mutate(CharacteristicName = "DISSOLVED OXYGEN (DO)")
      
      
      return(df_out)
      
}