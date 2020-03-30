# Script to Get Table for Core Parameter Indicators


# Read in Curts Doc - Save This in App and Load (Invisible to Database)
df_core_indicator <- read_excel("water/Input_Data/Standards Summary 3.13.19.xlsx", sheet = 1) %>% 
# df_core_indicator <- read_excel("./djs_playground/Standards Summary 3.13.19.xlsx", sheet = 1) %>%      
      
      
      
      select(CharacteristicName = Parameter, Desig_Use = `R DU`, 
             # Method, Condition, # Unnecessary
             Core = `Core Parameter Indicator`) %>% 
      mutate(CharacteristicName = toupper(CharacteristicName)) %>% 

      
      # Change Names to Match
      mutate(CharacteristicName = case_when(
            
            CharacteristicName == "AMMONIA" ~ "AMMONIA-NITROGEN",
            CharacteristicName == "NITROGEN (TOTAL)" ~ "NITROGEN",
            CharacteristicName == "SUSPENDED SEDIMENT CONCENTRATION" ~ "SUSPENDED SEDIMENT CONCENTRATION (SSC)",
            CharacteristicName == "OXYGEN, DISSOLVED" ~ "DISSOLVED OXYGEN (DO)",
            
            TRUE ~ CharacteristicName
            
      )) %>% 
      
      # Keep only if yes...
      filter(Core == "Y") %>% 
      unique() %>% 
      
      # Change to Indicator Variable
      mutate(Core = 1)

# write_csv(x = df_core_indicator, path = "/work/R-projects/Water Project/djs_playground/df_core_indicator.csv")
write_csv(x = df_core_indicator, path = "water/Input_Data/df_core_indicator.csv")

