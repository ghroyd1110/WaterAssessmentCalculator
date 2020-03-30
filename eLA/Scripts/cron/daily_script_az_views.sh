#!/bin/bash -e
curr_dir=$(dirname "$0")
source $curr_dir/environment.sh

# quickbase check
if test -f $curr_dir/az_cron_job_running; then
  echo "cron running";
  exit 1;
fi
mkdir -p $az_temp
touch $curr_dir/az_cron_job_running
sleep 1
# az views check
echo "downloading az views "
python $scripts/download_adeq_views.py $az_temp
echo "diff az directories"
diff $az_temp $az_data
if [ $? -ne 0 ]; then
    echo "There are az views edits"; #&> $tempdir/cron.log
    mv -f $az_temp/*csv $az_data
    echo "R script start "; #&> $tempdir/cron.log
    Rscript $scripts/deploy_to_shiny.R  #&> $tempdir/cron.log
    echo "Done"
fi
rm -rf $az_temp
rm -f $curr_dir/az_cron_job_running
