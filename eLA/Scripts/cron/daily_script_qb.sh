#!/bin/bash -e

curr_dir=$(dirname "$0")
source $curr_dir/environment.sh

# quickbase check
mkdir -p $qb_temp
if test -f $curr_dir/qb_cron_job_running; then
  echo "cron running";
  exit 1;
fi
touch $curr_dir/qb_cron_job_running
echo "downloading qb files from quickbase"
python $scripts/download_qb.py $qb_temp
echo "checking for updates in qb directories"
diff $qb_temp $qb_data
if [ $? -ne 0 ]; then
    echo "There are quickbase edits"; #&> $tempdir/cron.log
    mv -f $qb_temp/*csv $qb_data
    echo "R script start "; #&> $tempdir/cron.log
    Rscript $scripts/deploy_to_shiny.R  #&> $tempdir/cron.log
    echo "Done"
fi
rm -rf $qb_temp
rm -f $curr_dir/qb_cron_job_running
