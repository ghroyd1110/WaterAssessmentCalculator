#!/bin/bash
# shopt -s expand_aliases
export PATH="/wcalc/work/.packages/conda3/bin:$PATH"
export LD_LIBRARY_PATH=/usr/lib/oracle/12.2/client64/lib/${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
export ORACLE_HOME=/usr/lib/oracle/12.2/client64
export PATH=$PATH:$ORACLE_HOME/bin

app="/wcalc/work/app"
scripts=$app/scripts
data=$app/data
epa=$data/epa
rds=$data/rds
az_data=$data/az_views
az_temp=$data/az_temp
qb_data=$data/qb
qb_temp=$data/qb_temp

echo "$app $scripts $data"


#curr_dir=$(dirname "$0")
#source $curr_dir/environment.sh
