# adeq_water
bash /wcalc/work/app/scripts/cron/weekly_csv_to_rds.sh
/wcalc/work/app /wcalc/work/app/scripts /wcalc/work/app/data
downloading csv files from epa
downloading from 04-22-2019 to 04-29-2019
Downloading Sample_results_chemical
download successful. Written to:  /wcalc/work/app/data/epa/Sample_results_chemical_from_04-22-2019_to_04-29-2019.csv
Downloading Sampling_Activity
download successful. Written to:  /wcalc/work/app/data/epa/Sampling_Activity_from_04-22-2019_to_04-29-2019.csv
Downloading Sampling_Activity_Metrics
download successful. Written to:  /wcalc/work/app/data/epa/Sampling_Activity_Metrics_from_04-22-2019_to_04-29-2019.csv
Downloading Project_data
download successful. Written to:  /wcalc/work/app/data/epa/Project_data_from_04-22-2019_to_04-29-2019.csv
Downloading Site_data_only
download successful. Written to:  /wcalc/work/app/data/epa/Site_data_only_from_04-22-2019_to_04-29-2019.csv
Downloading Result_Detection_Quantitation_Limit_Data
download successful. Written to:  /wcalc/work/app/data/epa/Result_Detection_Quantitation_Limit_Data_from_04-22-2019_to_04-29-2019.csv
converting csv files to rds
Warning message:
In read.table(file = file, header = header, sep = sep, quote = quote,  :
  incomplete final line found by readTableHeader on '/wcalc/work/app/data/epa/Project_data_from_04-22-2019_to_04-29-2019.csv'
/wcalc/work/app/data/epa/Project_data_from_04-22-2019_to_04-29-2019.csv
/wcalc/work/app/data/epa/Result_Detection_Quantitation_Limit_Data_from_04-22-2019_to_04-29-2019.csv
/wcalc/work/app/data/epa/Sample_results_chemical_from_04-22-2019_to_04-29-2019.csv
/wcalc/work/app/data/epa/Sampling_Activity_from_04-22-2019_to_04-29-2019.csv
Warning message:
In read.table(file = file, header = header, sep = sep, quote = quote,  :
  incomplete final line found by readTableHeader on '/wcalc/work/app/data/epa/Sampling_Activity_Metrics_from_04-22-2019_to_04-29-2019.csv'
[1] "empty csv file. Not producing rds file"
/wcalc/work/app/data/epa/Sampling_Activity_Metrics_from_04-22-2019_to_04-29-2019.csv
/wcalc/work/app/data/epa/Site_data_only_from_04-22-2019_to_04-29-2019.csv
mv: cannot stat ‘/wcalc/work/app/data/epa/Sampling_Activity_Metrics*rds’: No such file or directory
moving csv files to archive
Deploying shiny app
Preparing to deploy document...DONE
Uploading bundle for document: 924374...DONE
Deploying bundle: 2081324 for document: 924374 ...
Waiting for task: 613236744
  building: Parsing manifest
  building: Building image: 2193563
  building: Fetching packages
  building: Installing packages
  building: Installing files
  building: Pushing image: 2193563
  deploying: Starting instances
  rollforward: Activating new instances
  terminating: Stopping old instances
Document successfully deployed to https://azdeq.shinyapps.io/waterapp/


