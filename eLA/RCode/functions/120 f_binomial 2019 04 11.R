# This function computes binomial impairments

# Inputs:   data frame 
# Output:   data frames with impairments

# df_in <- df_d_exceed
# binom_var = "stnd_exceed"

f_binomial <- function(df_in, binom_var, my_min_N = 20, my_prob = 0.1, my_conf = 0.9){
      
      # Name Games
      df_in <- df_in %>% select(WBID, CharacteristicName, Desig_Use, binom_var)
      names0 <- names(df_in)
      names(df_in) <- c("WBID", "CharacteristicName", "Desig_Use", "binom_var")
      
      # Calculate
      df_out <- df_in %>% 
            
            group_by(WBID, CharacteristicName, Desig_Use) %>% 
            mutate(Total = n()) %>% 
            
            group_by(WBID, CharacteristicName, Desig_Use, Total) %>%
            summarise(n_exceedences = sum(binom_var)) %>% 
            
            mutate(pbinom = pbinom(q = n_exceedences - 1, size = Total, prob = my_prob)) %>% 

            mutate(impaired = ifelse(Total >= my_min_N & pbinom > my_conf, 1, 0))
            
            

      return(df_out)
      
}
