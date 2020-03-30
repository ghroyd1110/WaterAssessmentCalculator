# Function to Adjust for Equipment Tolerances

# df_in = df105
# df_tol_in = df_equip_tol

f_adjust_tolerances <- function(df_in, df_tol_in){
      
      # Equipment Tolerances
      df_out <- left_join(df_in, df_tol_in %>% filter(Tolerance > 0) %>% select(-Method, -Condition))
      
      df_out <- df_out %>% 
            replace_na(list(Tolerance = 0)) %>% 
            mutate(standard =
                         case_when(
                               
                               Tolerance > 0 & Method == "max" ~ standard + Tolerance,
                               Tolerance > 0 & Method == "min" ~ standard - Tolerance,
                               
                               TRUE ~ standard
                               
                         )
            )
      
}