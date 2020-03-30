df_k


# Calculate numbers of samples per WBID over our time period.  
# Then assign the median identifier; we have to take the median for every 4 samples
# If there are any samples left over after the last multiple of 4 (1, 2, or 3) then drop them. 
# For ex if we have 7 samples, we calculate 1 median and drop the last 3 since they do not meet the min requirement of 4 samples per median.  

df_k1 <- df_k %>%
      dplyr::select(WBID, CharacteristicName, Desig_Use, my_year, week_of_year, ResultMeasureValue, standard) %>%
      dplyr::group_by(WBID, CharacteristicName, Desig_Use, standard) %>%
      dplyr::mutate(Sample_ID =  row_number()) %>% 
      
# median_id = (-(((1:length(df_k1$Sample_ID) - 1) %% 4) - 1:length(df_k1$Sample_ID)) -1 )/4 + 1
      
      dplyr::mutate(Median_ID = (-(((1:length(Sample_ID) - 1) %% 4) - 1:length(Sample_ID)) -1 )/4 + 1)

# Find all sample count results per WBID.  
# We will join this information onto our original data so we can then filter out any sample groups that do not have the minimum requirement of 4.  
df_k2 <- df_k1 %>%
      dplyr::select(WBID, CharacteristicName, Desig_Use, Median_ID, standard, ResultMeasureValue) %>%
      dplyr::group_by(WBID, CharacteristicName, Desig_Use, standard, Median_ID) %>% 
      dplyr::tally() %>%
      dplyr::rename(Median_Cnt = n) %>%
      dplyr::ungroup() %>%
      dplyr::select(-standard)


#@@@ Exceedances

# Add the median count to our data frame and filter out any sample groups that do not have 4 full samples for median calculation.  Then calculate the exceedance results.  Exceedance = one instance of the median of at least 4 consecutive samples (at least 7 days apart) exceeding the standard.  
df_k3 <- left_join(df_k1, df_k2) %>%
      dplyr::filter(Median_Cnt == 4) %>%
      dplyr::select(-my_year, -week_of_year, -Sample_ID, -Median_Cnt) %>%
      dplyr::group_by(WBID, CharacteristicName, Desig_Use, standard, Median_ID) %>%
      dplyr::mutate(Median_Result = median(ResultMeasureValue)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-ResultMeasureValue, -Median_ID) %>%
      unique() %>%
      dplyr::mutate(Exceedance = ifelse(Median_Result > standard, 1, 0))

# For any WBIDs with exceedances, calculate Impairment = Two or more exceedances during the assessment period; join these results back on our master data frame
df_k4 <- df_k3 %>%
      dplyr::select(WBID, CharacteristicName, Desig_Use, Exceedance) %>%
      dplyr::group_by(WBID, CharacteristicName, Desig_Use) %>%
      dplyr::mutate(Num_Exceedances = sum(Exceedance)) %>%
      dplyr::mutate(Impairment = ifelse(Num_Exceedances >= 2, "Y", "N")) %>%
      dplyr::select(-Exceedance) %>%
      unique()


#@@@ Impairment

df_k5 <- left_join(df_k1, df_k4) %>%
      dplyr::mutate(Num_Exceedances = ifelse(is.na(Num_Exceedances), 0, Num_Exceedances)) %>%
      dplyr::mutate(Impairment = ifelse(is.na(Impairment), "N", Impairment))    



#@@@ Attaining

# Max Date will be defined by the user in the flexdashboard; this will be reactive  =====> Need test_results loaded for this
max_date <- max(test_results$ActivityStartDate)
attain_date <- max_date - 3*365
attain_week <- week(attain_date)
attain_year <- year(attain_date)

df_k6 <- df_k5 %>%
      
      dplyr::arrange(my_year, week_of_year) %>%
      
      # Keep only the previous 3 years from user selection
      dplyr::filter(my_year >= attain_year) %>%
      dplyr::filter(!(my_year == attain_year & week_of_year <= attain_week))

df_k7 <- df_k5 %>%
      ungroup() %>%
      dplyr::select(WBID, CharacteristicName, Desig_Use, Num_Exceedances) %>%
      unique() %>%
      dplyr::mutate(Attaining = ifelse(Num_Exceedances > 0, "N", "Y"))


#@@@ Inconclusive
     
df_k8 <- left_join(df_k5, df_k7) %>%
      dplyr::mutate(Inconclusive = ifelse(Impairment == "N" & Attaining == "N", "Y", "N"))



#@@@ Delisting Eligibility

df_k9 <- df_k8 %>%
      #To delist if impaired, we need at least 2 medians calculated (min of 8 samples) with no exceedances AND Attainment criteria met
      dplyr::filter(Median_ID >= 2 & max(Sample_ID) >= 8) %>%
      dplyr::ungroup() %>%
      dplyr::select(WBID, CharacteristicName, Desig_Use, Num_Exceedances, Attaining) %>%
      unique() %>%
      dplyr::mutate(Delist = ifelse(Num_Exceedances == 0 & Attaining == "Y", "Y", "N"))

df_k10 <- left_join(df_k8, df_k9) %>%
      # NAs resulting from the join indicate not enough samples to test Delisting; hence, they get assigned "N"
      dplyr::mutate(Delist = ifelse(is.na(Delist), "N", Delist))  



#@@@ Summarize results by WBID

df_k_results <- df_k10 %>%
      dplyr::ungroup() %>%
      dplyr::select(WBID, CharacteristicName, Desig_Use, Num_Exceedances, Impairment, Attaining, Inconclusive, Delist) %>% 
      unique() %>% 
      View()





