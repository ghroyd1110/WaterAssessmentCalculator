
# This function converts units to match units in the standards

# Inputs:   data frame and columns to ID parameter by name, units of measure to check/convert, and result value to check/convert
# Output:   data frame with standardized units of measure

# df_in <- df2
# name0 = "CharacteristicName"
# units0 = "ResultMeasure.MeasureUnitCode"
# value0 = "ResultMeasureValue"

#-------
f_unit_conversion <- function(df_in, 
                              name0 = "CharacteristicName", 
                              units0 = "ResultMeasure.MeasureUnitCode", 
                              value0 = "ResultMeasureValue", 
                              ug_per_l, 
                              mg_per_l){
      
      # Change column names to generic names
      df_in <- df_in %>% rename(my_name = name0, my_units = units0, my_value = value0)
      
      # Ignore samples that are mg/kg for this phase - may come into play later
      df_in <- df_in %>% filter(!my_units %in% c("MG/KG")) 
      
      # Remove SSC if Units as %
      df_remove <- df_in %>% filter(my_units == "%" & grepl("SSC", my_name))
      df_in <- df_in %>% anti_join(df_remove)
      
      # Ignore Nitrogen if CM3/G @STP
      df_in <- df_in %>% filter(!my_units %in% c("CM3/G @STP"))
      
      # Conversion Factors for Units with Qualifiers
      df_in <- df_in %>% 
            
            mutate(my_value = case_when(
                  
                  grepl("AMMONIA", my_name) & grepl("NH3", my_units) ~ my_value * 0.822,
                  grepl("AMMONIA", my_name) & grepl("NH4", my_units) ~ my_value * 0.776,
                  grepl("NITR", my_name) & grepl("NO2", my_units) ~ my_value * 0.304,
                  grepl("NITR", my_name) & grepl("NO3", my_units) ~ my_value * 0.225,
                  grepl("PHOSPHATE", my_name) & grepl("PO4", my_units) ~ my_value * 0.326,
                  TRUE ~ my_value
                  
            ))
      
      # Remove Qualifiers from Unit Codes
      df_in <- df_in %>% 
            mutate(my_units = gsub(pattern = " AS.*$", replacement = "", x = my_units)) %>% 
            mutate(my_units = gsub(pattern = " NH4| CACO3| NO3", replacement = "", x = my_units))

     
      # Transformations to Standard Units
      df_units <- df_in %>% 
            
            # This changes the measurement values
            mutate(my_value = case_when(
                  
                  my_name %in% ug_per_l & my_units %in% c("MG/L") ~ 1000 * my_value,
                  my_name %in% ug_per_l & my_units %in% c("MG/KG") ~ my_value, # Currently obsolete - may need in future
                  
                  my_name %in% mg_per_l & my_units %in% c("MG/KG") ~ my_value, # Currently obsolete - may need in future
                  my_name %in% mg_per_l & my_units %in% c("UG/L") ~ my_value/1000,
                  
                  # Note: Treat MPN and CFU separate
                  grepl("COLI", my_name) & my_units %in% c("MPN/100ML","CFU/100ML","MPN/100 ML") ~ my_value,
                  grepl("COLI", my_name) & my_units %in% c("CFU/ML") ~ 100*my_value,
                  
                  my_name %in% c("PH") & my_units %in% c("NONE", "", "SU", "STD UNITS") ~ my_value, # Explicit
                  
                  # Not Sure About This One (See http://www.watertreatmentguide.com/conversion_tables.htm)
                  my_name %in% c("SUSPENDED SEDIMENT CONCENTRATION (SSC)") & my_units %in% c("%") ~ 100 * my_value,
                  
                  grepl("DISSOLVED OXY", my_name) & grepl("%", my_units) ~ my_value,
                  
                  my_name %in% c("TEMPERATURE, WATER") & my_units %in% c("DEG F") ~ (5/9) * (my_value - 32),
                  my_name %in% c("TEMPERATURE, WATER DEG F") ~ (5/9) * (my_value - 32),
                  
                  TRUE ~ my_value
                  
            )) %>%
            
            # This changes the associated units
            mutate(my_units = case_when(
                  
                  my_name %in% ug_per_l & my_units %in% c("MG/L") ~ "UG/L",
                  my_name %in% ug_per_l & my_units %in% c("MG/KG") ~ "UG/L",
                  
                  my_name %in% mg_per_l & my_units %in% c("UG/L") ~ "MG/L",
                  my_name %in% mg_per_l & my_units %in% c("MG/KG") ~ "MG/L",
                  
                  grepl("COLI", my_name) & my_units %in% c("MPN/100ML","CFU/100ML","MPN/100 ML") ~ "CFU/100ML",
                  grepl("COLI", my_name) & my_units %in% c("CFU/ML") ~ "CFU/100ML",
                  
                  my_name %in% c("PH") & my_units %in% c("NONE", "", "SU", "STD UNITS") ~ "SU",
                  
                  my_name %in% c("SUSPENDED SEDIMENT CONCENTRATION (SSC)") & my_units %in% c("%") ~ "MG/L",
                  
                  grepl("DISSOLVED OXY", my_name) & grepl("%", my_units) ~ "PERCENT",
                  
                  my_name %in% c("TEMPERATURE, WATER") & my_units %in% c("DEG F") ~ "DEG C",
                  my_name %in% c("TEMPERATURE, WATER DEG F") ~ "DEG C",
                  
                  TRUE ~ my_units
                  
            ))
      
      # Change Names Back to Original Names
      names(df_units)[names(df_units) == "my_name"] <- name0
      names(df_units)[names(df_units) == "my_units"] <- units0
      names(df_units)[names(df_units) == "my_value"] <- value0
      
      return(df_units)
      
}