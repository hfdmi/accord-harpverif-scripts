#!/bin/bash 
 
#This part is only for running in SLURP at ecmwf
#SBATCH --output=aude.out
#SBATCH --job-name=aude
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=16000
#SBATCH --ntasks=1
#SBATCH --qos=nf
#SBATCH --time=23:30:00

#source /home/sp3c/.bashrc
#set -x  

source config/config_atos.sh

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


if [ "$SHOW_WEB_STATIC" == "yes" ]; then
	$RS_DIR/visualization/shiny_launch_static.R
fi

if [ "$SHOW_WEB_DYNAMIC" == "yes" ]; then
        $RS_DIR/visualization/shiny_launch_dynamic.R
fi
