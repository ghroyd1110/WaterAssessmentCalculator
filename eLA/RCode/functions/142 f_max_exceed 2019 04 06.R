# Function to ensure attain compliance - max number of exceedences based on sample size

# Inputs: 

      # data frame with exceedences


# Output: data frame with max exceed attainment tags

# df_in <- df_a_exceed


f_max_exceed <- function(df_in, exceed_col = "stnd_exceed"){
     
      # Standardize Exceed Column
      names(df_in)[names(df_in) == exceed_col] <- "exceed_col"
      
     
      # Pull and Check
      df_out <- df_in %>% 

            # Find Number of Samples
            group_by(WBID, CharacteristicName, Desig_Use) %>% 
            summarise(n_samples = n(), n_exceed = sum(exceed_col)) %>% 
            
            # Min # for Binomial Exceed Impairment
            # The quantile is defined as the smallest value x such that F(x) ≥ p, where F is the distribution function.
            # Curt is going to stastical validity...
            # Add + 1 because fractional probabilities
            mutate(min_binomial = max(0, qbinom(p = 0.9, size = n_samples, prob = 0.10 ) - 2 + 1)) %>% 
            
            # Determine Attain
            mutate(attain_exceeds = case_when(
                  
                  n_samples >= 20 & n_exceed <= min_binomial ~ 1,
                  n_samples >= 20 & n_exceed > min_binomial ~ 0,
            
                  n_samples >= 16 & n_samples <= 19 & n_exceed <= 3 ~ 1,
                  n_samples >= 16 & n_samples <= 19 & n_exceed > 3 ~ 0,
                  
                  n_samples >= 10 & n_samples <= 15 & n_exceed <= 2 ~ 1,
                  n_samples >= 10 & n_samples <= 15 & n_exceed > 2 ~ 0,
                  
                  n_samples >= 3 & n_samples <= 9 & n_exceed == 0 ~ 1,
                  n_samples >= 3 & n_samples <= 9 & n_exceed > 0 ~ 0,

                  n_samples <= 2 ~ 0, # Less Than 3 Samples and 0 Exceed, Then You Don't Attain
                  
                  TRUE ~ 0
                  
            ))

      
      return(df_out)
      
}