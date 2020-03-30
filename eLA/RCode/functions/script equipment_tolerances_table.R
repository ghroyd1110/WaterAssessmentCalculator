# Script to Get Table for Equipment Tolerances


# Read in Curts Doc - Save This in App and Load (Invisible to Database)
# df_equip_tol <- read_excel("./djs_playground/Standards Summary 3.13.19.xlsx", sheet = 1) %>% 
df_equip_tol <- read_excel("water/Input_Data/Standards Summary 3.13.19.xlsx", sheet = 1) %>% 
            
      select(CharacteristicName = Parameter, Desig_Use = `R DU`, Method, Condition, Tolerance = `Field Equipment Tolerance`) %>% 
      mutate(CharacteristicName = toupper(CharacteristicName)) %>% 

      
      # Change Names to Match
      mutate(CharacteristicName = case_when(
            
            CharacteristicName == "AMMONIA" ~ "AMMONIA-NITROGEN",
            CharacteristicName == "NITROGEN (TOTAL)" ~ "NITROGEN",
            CharacteristicName == "SUSPENDED SEDIMENT CONCENTRATION" ~ "SUSPENDED SEDIMENT CONCENTRATION (SSC)",
            CharacteristicName == "OXYGEN, DISSOLVED" ~ "DISSOLVED OXYGEN (DO)",
            
            TRUE ~ CharacteristicName
            
      ))

# Add 2nd Defn of Dissolved Oxygen
# df_equip_tol <- bind_rows(
#       
#       df_equip_tol,
#       
#       df_equip_tol %>% 
#             filter(grepl("OXY", CharacteristicName)) %>% 
#             mutate(CharacteristicName = "DISSOLVED OXYGEN SATURATION")
# )

# write_csv(x = df_equip_tol, path = "/work/R-projects/Water Project/djs_playground/df_equip_tol.csv")
# write_csv(x = df_equip_tol, path = "water/Input_Data/df_equip_tol.csv")




