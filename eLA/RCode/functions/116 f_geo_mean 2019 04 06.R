# Script to Run Impairment Logic g (Geo Mean for E Coli)

# Input: data frame 
# Output: data frame ready for stnd exceedence

# df_in <- df_g
# df_out %>% filter(grepl("COLI", CharacteristicName)) %>% filter(WBID %in% c("15060202-059A"))
# df_date %>% filter(WBID %in% c("15060202-059A"))

f_geo_mean <- function(df_in){
      
      # Subset to Critical Cols
      df_out <- df_in %>% select(WBID, CharacteristicName, Desig_Use, my_year, week_of_year, Method, ResultMeasureValue, standard)
      
      # Tag every four samples as a group
      df_out <- df_out %>% 
            arrange(WBID, Desig_Use, my_year, week_of_year) %>% 
            group_by(WBID, Desig_Use) %>% 
            mutate(my_group = ceiling(row_number()/4))
      
      # Gotta Have Four or Else Your Out
      df_out <- df_out %>% 
            group_by(WBID, Desig_Use, my_group) %>% 
            mutate(n_my_group = n()) %>% 
            filter(n_my_group == 4) %>% 
            select(-n_my_group)
            
      # Save Year, Week for Each Group
      # df_date <- df_out %>%
      #       group_by(my_group) %>%
      #       slice(1) %>%
      #       select(my_group, my_year, week_of_year)
      
      df_date <- df_out %>%
            # group_by(my_group) %>%
            group_by(WBID, Desig_Use, my_group) %>%
            slice(1) %>%
            ungroup() %>%
            select(WBID, Desig_Use, my_group, my_year, week_of_year)
            
      
      # Geo Mean
      df_out <- df_out %>% 
            group_by(WBID, CharacteristicName, Desig_Use, Method, my_group, standard) %>% 
            summarise(ResultMeasureValue = geometric.mean(ResultMeasureValue)) %>% 
            ungroup() %>% 
            
            # Add Date
            left_join(df_date) %>% 
            
            # Clean up
            select(-my_group)
      
      
      
            return(df_out)
      
}



