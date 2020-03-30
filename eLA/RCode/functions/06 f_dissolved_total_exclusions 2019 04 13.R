
# This function converts handles non detect transformations

# Inputs:   data frame of test results, data frame of standards
# Output:   data frame with transformed dissolved stnds x total sample excluded

# df_in <- df6
# df_stnd_in <- df_standards

f_dissolved_vs_total_exclusion <- function(df_in, df_stnd_in){
      
      # Clean up "Total" - Consolidate
      df_in <- df_in %>%
            
            mutate(ResultSampleFractionText = toupper(ResultSampleFractionText)) %>% 
            
            mutate(ResultSampleFractionText = case_when(
                  
                  ResultSampleFractionText %in% c("TOTAL", "TOTAL RECOVERABLE", "RECOVERABLE") ~ "TOTAL",
                  ResultSampleFractionText %in% c("DISSOLVED") ~ "DISSOLVED",
                  TRUE ~ ResultSampleFractionText
                  
            ))
            
      # Standard = D & Sample = T Remove
      # Join - Samples and Standards - Only Need Dissolved Stnds
      df_DT <- left_join(
            
            df_in,
            
            df_stnd_in %>% 
                  select(substance_name, desig_use, cas_qualifier_name, acute_chronic) %>% 
                  filter(cas_qualifier_name == "DISSOLVED") %>%
                  unique(),
            
            by = c("CharacteristicName" = "substance_name", 
                   "Desig_Use" = "desig_use", 
                   "Condition" = "acute_chronic"
            )
            
      )
      
      # Compare and remove dissolved stnds x total samples
      # Can't use if total results vs dissolved standard
      df_DT <- df_DT %>% 
            mutate(remove_me = ifelse(ResultSampleFractionText == "TOTAL" & cas_qualifier_name == "DISSOLVED", 1, 0)) %>% 
            replace_na(list(remove_me = 0))
           
      # Keep if Hardness (Special Case)
      df_DT <- df_DT %>% mutate(remove_me = ifelse(grepl("HARD", CharacteristicName), 0, remove_me))

      # Actually remove rows
      df_DT <- df_DT %>% filter(remove_me != 1)
      
      # Remove added columns
      df_DT <- df_DT %>% select(-cas_qualifier_name, -remove_me)
      
      # If Repeated TOTAL and DISSOLVED and Standard is TOTAL 
      # (And there is never a time when there are both total and dissolved standards for the same designated use for a single paramter)
      # Then just Use Total, Drop Dissolved... (if no total, keep dissolved)
            
      # # What has Total
      # df_total <- df_DT %>% 
      #       filter(ResultSampleFractionText == "TOTAL") %>% 
      #       select(WBID, CharacteristicName, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue)
      # 
      # # What has Dissolved 
      # df_dissolved <- df_DT %>% 
      #       filter(ResultSampleFractionText == "DISSOLVED") %>% 
      #       select(WBID, CharacteristicName, Desig_Use, ActivityStartDate, ActivityStartTime.Time, ActivityDepthHeightMeasure.MeasureValue, ResultSampleFractionText)
      # 
      # # What has Both
      # df_both <- inner_join(df_total, df_dissolved)
      # 
      # # Remove Dissolved Extras
      # df_DT <- df_DT %>% anti_join(df_both)
      
      
      return(df_DT)
      
}
