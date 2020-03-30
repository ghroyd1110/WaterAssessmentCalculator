# This function aggregated temporally dependent samples for typcial parameters

# Inputs:   data frame 
# Output:   data frames for mean, geo mean, median, max, min

# df_in <- df16 %>% filter(grepl("0590", WBID)) %>% filter(grepl("SELE", CharacteristicName)) %>% filter(Desig_Use == "AWW")

f_temp_ind_agg_typical_parameters <- function(df_in){
      
      # Add Year, Week of Year to Sample Test Results Data
      df_in <- df_in %>% 
            
            mutate(my_year = year(ActivityStartDate)) %>% 
            mutate(week_of_year = week(ActivityStartDate)) 
      
      
      # print("week('2015-09-01')")
      # print(week("2015-09-01"))
      # print("week('2015-09-02')")
      # print(week("2015-09-02"))
      # 
      # test <- c("2015-09-01","2015-09-02","2015-09-03","2015-09-04",
      #           "2015-09-05","2015-09-06","2015-09-07","2015-09-08",
      #           "2015-09-09","2015-09-10","2015-09-11","2015-09-12")
      # 
      # print(test)
      # print(week(test))
      # 
      # print("Test within f_temp_ind_agg_typical_param")
      # print.data.frame(df_in %>% filter(WBID == "15030101-0590" & CharacteristicName == "SELENIUM" & Desig_Use == "AWW") %>% 
      #                        ungroup() %>% 
      #                        select(ActivityStartDate, ActivityStartTime.Time, my_year, week_of_year))
      # 
      
      
      # Simplify Data to Just Required Columns ====
      
      df_in <- df_in %>% 
            
            ungroup %>% 
            
            select(
                  
                  WBID, 
                  Desig_Use, 
                  CharacteristicName,
                  # ActivityStartDate,
                  # ActivityStartTime.Time
                  # ActivityDepthHeightMeasure.MeasureValue,
                  ResultSampleFractionText,
                  Condition,
                  # Method,
                  my_year,
                  week_of_year,
                  ResultMeasureValue,
                  Aggregation
            )
      
      
      # Typical Parameter, Mean ====
      
      df_agg_mean <- df_in %>% 
            
            # Get Just the Relevant Parameters
            filter(Aggregation == "Mean") %>% 
            
            # Find Mean
            group_by_at(vars(-ResultMeasureValue)) %>% 
            summarise(ResultMeasureValue = mean(ResultMeasureValue))
      
      
      # Typical Parameter, Geometric Mean ====
      
      df_agg_geo_mean <- df_in %>% 
            
            # Get Just the Relevant Parameters
            filter(Aggregation == "Geometric Mean") %>% 
            
            # Find Geo Mean
            group_by_at(vars(-ResultMeasureValue)) %>% 
            summarise(ResultMeasureValue = geometric.mean(ResultMeasureValue))
      
      
      
      # Typical Parameter, Median ====
      
      df_agg_median <- df_in %>% 
            
            # Get Just the Relevant Parameters
            filter(Aggregation == "Median") %>% 
            
            # Find Median
            group_by_at(vars(-ResultMeasureValue)) %>% 
            summarise(ResultMeasureValue = median(ResultMeasureValue))
      
      
      
      # Typical Parameters, Max ====
      
      df_agg_max <- df_in %>% 
            
            # Get Just the Relevant Parameters
            filter(Aggregation == "Max") %>% 
            
            # Find Max
            group_by_at(vars(-ResultMeasureValue)) %>% 
            summarise(ResultMeasureValue = max(ResultMeasureValue))
      
      
      # Typical Parameters, Min ====
      
      df_agg_min <- df_in %>% 
            
            # Get Just the Relevant Parameters
            filter(Aggregation == "Min") %>% 
            
            # Find Min
            group_by_at(vars(-ResultMeasureValue)) %>% 
            summarise(ResultMeasureValue = min(ResultMeasureValue))
      
      
      return(list(
            df_agg_mean = df_agg_mean, 
            df_agg_geo_mean = df_agg_geo_mean, 
            df_agg_median = df_agg_median, 
            df_agg_max = df_agg_max, 
            df_agg_min = df_agg_min))
      
}



