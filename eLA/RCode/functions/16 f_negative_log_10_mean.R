# Function to calculate negative log 10 "mean"...


# Inputs:   vector of pH values
# Output:   negative log 10 "mean"


f_neg_log_10_mean <- function(my_vals){
      
      my_aggregate <- -log10(sum(10^(-1*my_vals))/length(my_vals))
      
      return(my_aggregate)
      
}


