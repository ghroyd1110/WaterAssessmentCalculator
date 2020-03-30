# Function to pull worst case impairments

# Inputs: 

      # data frame with impairments


# Output: data frame worst case impairments

# df_in <- df_impair
# df_in %>% group_by(WBID, CharacteristicName, Desig_Use) %>% tally() %>% filter(n > 1) %>%
#       group_by(CharacteristicName, Desig_Use) %>% tally() %>% print.data.frame()

f_impair_worst_case <- function(df_in){
     
      df_out <- df_in %>% 
            
            # Find Worst Case
            group_by(WBID, CharacteristicName, Desig_Use) %>% 
            mutate(max_impaired = max(impaired)) %>% 
            
            # Arrange (Worst Case on Top)
            arrange(WBID, CharacteristicName, Desig_Use, desc(max_impaired), desc(n_exceedences)) %>% 
            
            # Pull Worst Case
            group_by(WBID, CharacteristicName, Desig_Use) %>% 
            slice(1) %>% 
            select(-max_impaired)
            
          
      return(df_out)
      
}