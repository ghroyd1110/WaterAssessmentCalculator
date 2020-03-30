# Script to Create Impairment Logic Table
# This table is paired to the wrangled data to ID logic flow


# Read in Curts Doc - Save This in App and Load (Invisible to Database)
# df_logic <- read_excel("./djs_playground/Standards Summary 3.13.19.xlsx", sheet = 1) %>% 
df_logic <- read_excel("water/Input_Data/Standards Summary 3.13.19.xlsx", sheet = 1) %>% 
      
      select(CharacteristicName = Parameter, Desig_Use = `R DU`, impair_logic = `Impairment Logic`, Method, Condition) %>% 
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
df_logic <- bind_rows(
      
      df_logic,
      
      df_logic %>% 
            filter(grepl("OXY", CharacteristicName)) %>% 
            mutate(CharacteristicName = "DISSOLVED OXYGEN SATURATION")
)

# Dissolved O2 is read in under an ACUTE column (even though it is not acute)
df_logic <- df_logic %>% 
      mutate(Condition = ifelse(grepl("OXYGEN", CharacteristicName), "ACUTE", Condition)) %>% 
      unique()

# Add Missing (Nitrite, Nitrate)
# df_missing <- data_frame(
#       
#       CharacteristicName = c(rep(c("NITRITE", "NITRATE"), 3)),
#       Desig_Use = c(rep(c("DWS", "FBC", "PBC"), 2)),
#       Method = c(rep("max", 6)),
#       impair_logic = c(rep("e",6))
#       
# )

# df_logic <- bind_rows(df_logic, df_missing)

# write_csv(x = df_logic, path = "/work/R-projects/Water Project/djs_playground/df_logic.csv")
write_csv(x = df_logic, path = "water/Input_Data/df_logic.csv")




