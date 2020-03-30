#!/bin/bash -e

curr_dir=$(dirname "$0")
source $curr_dir/environment.sh

# clean running jobs
rm -rf $curr_dir/*cron_job_running


if test -f $curr_dir/weekly_cron_job_running; then
  echo "cron running";
  exit 1;
fi
touch $curr_dir/weekly_cron_job_running
echo "downloading csv files from epa"
python $scripts/download_epa_data.py $data/epa
echo "converting csv files to rds"
for each in $data/epa/*csv; do Rscript $scripts/rds.R -f $each; echo $each; done;
# Sampling_Activity Sampling_Activity_Metrics Sample_results_chemical Project_data Site_data_only Result_Detection_Quantitation_Limit_Data
mv -f $epa/Sampling_Activity_Metrics*rds $rds/Sampling_Activity_Metrics/;
mv -f $epa/Sample_results_chemical*rds $rds/Sample_results_chemical/;
mv -f $epa/Sampling_Activity*rds $rds/Sampling_Activity/;
mv -f $epa/Project_data*rds $rds/Project_data/;
mv -f $epa/Site_data_only*rds $rds/Site_data_only/;
mv -f $epa/Result_Detection_Quantitation_Limit_Data*rds $rds/Limit_Data/;
echo "moving csv files to archive"
mv -f $epa/*csv $data/epa_archive/

echo "Deploying shiny app"
Rscript $scripts/deploy_to_shiny.R

rm -f $curr_dir/weekly_cron_job_running
