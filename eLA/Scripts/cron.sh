#!/bin/bash
shopt -s expand_aliases
export PATH="/wcalc/work/.packages/conda3/bin:$PATH"
export LD_LIBRARY_PATH=/usr/lib/oracle/12.2/client64/lib/${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
export ORACLE_HOME=/usr/lib/oracle/12.2/client64
export PATH=$PATH:$ORACLE_HOME/bin
alias runscripts='cd /wcalc/work/demo/adeq_water/; snakemake --snakefile .make_adeq -j 4'
alias runclean='cd /wcalc/work/demo/adeq_water/; snakemake --snakefile .make_adeq --delete-all-output'
alias

DATE=`date '+%Y-%m-%d %H:%M:%S'`
echo -e "$DATE\n\n" > /wcalc/work/elatestcron.out
runscripts | tee >> /wcalc/work/elatestcron.out 
#runscripts 2>&1 | tee /wcalc/work/elatestcron.out 
#runclean >> /wcalc/work/elatestcron.out
