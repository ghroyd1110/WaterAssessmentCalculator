# Script to Create Aggregation ID Logic Table
# This table is paired to the wrangled data for temporal aggregation


# Read in Curts Doc - Save This in App and Load (Invisible to Database)
# df_agg_id <- read_excel("./djs_playground/Standards Summary9.16.2018 copy.xlsx", sheet = 1) %>% 

# df_agg_id <- read_excel("./djs_playground/Standards Summary 3.13.19.xlsx", sheet = 1) %>% 
df_agg_id <- read_excel("water/Input_Data/Standards Summary 3.13.19.xlsx", sheet = 1) %>% 

      select(CharacteristicName = Parameter, Desig_Use = `R DU`, Aggregation, `Impairment Logic`) %>% 
      mutate(CharacteristicName = toupper(CharacteristicName)) %>% 
      
      # Keep Aggregation ####

      # Change Names to Match
      mutate(CharacteristicName = case_when(
            
            CharacteristicName == "AMMONIA" ~ "AMMONIA-NITROGEN",
            CharacteristicName == "NITROGEN (TOTAL)" ~ "NITROGEN",
            CharacteristicName == "SUSPENDED SEDIMENT CONCENTRATION" ~ "SUSPENDED SEDIMENT CONCENTRATION (SSC)",
            CharacteristicName == "OXYGEN, DISSOLVED" ~ "DISSOLVED OXYGEN (DO)",
            
            TRUE ~ CharacteristicName
            
      ))

# Add 2nd Defn of Dissolved Oxygen
df_agg_id <- bind_rows(
      
      df_agg_id,
      
      df_agg_id %>% 
            filter(grepl("OXY", CharacteristicName)) %>% 
            mutate(CharacteristicName = "DISSOLVED OXYGEN SATURATION")
)

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

# write_csv(x = df_agg_id, path = "/work/R-projects/Water Project/djs_playground/df_agg_id.csv")
write_csv(x = df_agg_id, path = "water/Input_Data/df_agg_id.csv")

