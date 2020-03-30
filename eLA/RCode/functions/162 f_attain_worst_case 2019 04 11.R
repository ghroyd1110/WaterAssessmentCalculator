# Function to pull worst case attain

# Inputs: 

      # data frame with attain


# Output: data frame worst case attain

# df_in <- df_attain
# df_in %>% group_by(WBID, CharacteristicName, Desig_Use) %>% tally() %>% filter(n > 1) %>%
#       group_by(CharacteristicName, Desig_Use) %>% tally() %>% print.data.frame()

f_attain_worst_case <- function(df_in){
     
      df_out <- df_in %>% 
            
            # Find Worst Case
            group_by(WBID, CharacteristicName, Desig_Use) %>% 
            mutate(min_attain = min(attain)) %>% 
            
            # Remove is Not Worst Case
            filter(attain == min_attain) %>%
            
            
            # Arrange (Worst Case on Top)
            arrange(WBID, CharacteristicName, Desig_Use, attain) %>% 
            
            # Pull Worst Case
            group_by(WBID, CharacteristicName, Desig_Use) %>% 
            slice(1) %>% 
            select(-min_attain) 
            
          
      return(df_out)
      
}