# This function extracts coefficients for result dependent standards

# Inputs:   data frame 
# Output:   coefficients

# df_in <- df_crit_stnds

f_result_dependent_coefficients <- function(df_in){
      
# Constants ====
      
      # These are the parameters with required coefficients
      my_parameters <- c(
            "Ammonia and ammonium", "Ammonia-nitrogen", "Arsenic", 
            "Beryllium", "Boron", 
            "Cadmium", "Chlorine", "Copper", 
            "Escherichia coli", 
            "Hardness, Ca, Mg", "Hardness, non-carbonate", "Total hardness",
            "Iron", 
            "Lead", 
            "Manganese", 
            "Nitrate", "Nitrite", "Nitrogen", "Kjeldahl nitrogen", "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)", "Organic Nitrogen", 
            "Oxygen", "Dissolved oxygen (DO)", "Dissolved oxygen saturation",
            "pH", "Phosphorus", 
            "Selenium", "Suspended Sediment Concentration (SSC)", 
            "Temperature, water", "Temperature, water, deg F", "Temperature, water, deg C", 
            "Zinc")
    
  
# Data Wrangle ====
      
      # Input Data
      
      df_out <- df_in %>% 
            filter(substance_name %in% toupper(my_parameters)) %>% 
            select(substance_name, awc_chronic_a:awedw_acute_d) %>% 
            gather(key = my_id, value = value, -substance_name) %>% 
            na.omit() %>% 
            separate(col = my_id, into = c("desig_use", "Condition", "coeff"), sep = "_") %>% 
            spread(key = coeff, value = value)
            
      return(df_out)
      
}
