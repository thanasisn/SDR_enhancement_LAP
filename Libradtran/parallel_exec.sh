#!/bin/bash

## Parallel files
SSH_LOGIN="/home/athan/BASH/PARAMS/parallel/sagan_blue_kostas"
SLURP_HOSTS="/home/athan/BASH/PARAMS/parallel/pssh_host_files"

## Project files
ARGS_LIST="$HOME/MANUSCRIPTS/02_enhancement/Libradtran/run.list"
JOB_RESUM="$HOME/MANUSCRIPTS/02_enhancement/Libradtran/resume.file"
HOST_OUTP="$HOME/MANUSCRIPTS/02_enhancement/Libradtran/io_repo"
JOBS_THRO="$HOME/MANUSCRIPTS/02_enhancement/Libradtran/job_throtle.par"
GATHER_DR="$HOME/MANUSCRIPTS/02_enhancement/Libradtran/par_out"



echo " *********************************** "
echo " ** starting with 50% of the cores!! ** "
echo " *********************************** "
echo "change file $JOBS_THRO to change throttling"


## set 50% of the cores as default
echo "50%" > "$JOBS_THRO"

## tic
SEC1=$(date +%s)

## remove resume-failed when starting a new job
##TODO try use an empty file
parallel                            \
    --jobs           "$JOBS_THRO"   \
    --progress                      \
    --eta                           \
    --resume-failed                 \
    --joblog         "$JOB_RESUM"   \
    --arg-file       "$ARGS_LIST"


    #--sshloginfile   "$SSH_LOGIN"   \
    # --results        "/dev/null"   \
# --timeout 1000%
# --jobs 1


## tac
SEC2=$(date +%s)
DIFFSEC="$((SEC2 - SEC1))"
echo "Took $(date +%H:%M:%S -ud @${DIFFSEC})"



# ## get files from host to master
# parallel-slurp -r             \
#     -h    "$SLURP_HOSTS"      \
#     -L    "$GATHER_DR"        \
#           "$HOST_OUTP" "./"
# 
# 
# ## move data
# # parallel-rsync -h ~/.pssh_host_files -ra /home/athan/Improved_Aerosols_O3/DATA/ /home/athan/Improved_Aerosols_O3/DATA/GET

SEC3=$(date +%s)
DIFFSEC=$((SEC3 - SEC2))
echo "Took $(date +%H:%M:%S -ud @${DIFFSEC})"

exit 0
