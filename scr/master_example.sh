#!/bin/bash

#This part is only for runnig in SLURP at ecmwf
#SBATCH --error=sc_autumn.err
#SBATCH --output=sc_autumn.out
#SBATCH --job-name=sc_autumn
#SBATCH --mem-per-cpu=32GB
#set -x
source $HOME/harphub/accord-verif-scripts/config/config_MetCoOp.sh

######## remove this
export RUN_VOBS2SQL=no
export RUN_VFLD2SQL=no
export RUN_POINT_VERF=no
export RUN_POINT_VERF_LOCAL=no
export SCORECARDS=yes
######## remove this

if [ "$RUN_VOBS2SQL" == "yes" ]; then
    echo "Running vobs2sql"
   $RS_DIR/point_pre_processing/vobs2sql.R
fi

if [ "$RUN_VFLD2SQL" == "yes" ]; then
     echo "Running vfld2sql"
    $RS_DIR/point_pre_processing/vfld2sql.R
fi


if [ "$RUN_POINT_VERF" == "yes" ]; then
   $RS_DIR/point_verif/point_verif.R
fi

if [ "$RUN_POINT_VERF_LOCAL" == "yes" ]; then
   echo "Running complete verification set"
   $RS_DIR/point_verif/point_verif_local.R
fi

if [ "$SCORECARDS" == "yes" ]; then
   $RS_DIR/point_verif/create_scorecards.R
fi
