#!/bin/bash 
 
#This part is only for running in SLURP at ecmwf
#SBATCH --error=faro21.err
#SBATCH --output=faro21.out
#SBATCH --job-name=faro21
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=16000
#SBATCH --ntasks=1
#SBATCH --qos=nf
#SBATCH --time=23:30:00

set -x  

export CASE_STUDY=CY46_RSL
cd /perm/sp3c/deode_verif/
source config/config_atos.sh

######## remove this 
export RUN_POINT_VERF=no
export RUN_POINT_VERF_LOCAL=no
export RUN_VOBS2SQL=no
export RUN_VFLD2SQL=no
export SCORECARDS=no
export SHOW_LOCAL_WEB=no
export UPDATE_SHINYAPPSIO=yes
######## remove this 

export SHINY_PORT=3543 # Change this number if port is busy when launching web

if [ "$RUN_VOBS2SQL" == "yes" ]; then 
    echo "Running vobs2sql"
   $RS_DIR/point_pre_processing/vobs2sql.R  
fi 

if [ "$RUN_VFLD2SQL" == "yes" ]; then 
     echo "Running vfld2sql"
    $RS_DIR/point_pre_processing/vfld2sql.R 
fi 
 

if [ "$RUN_POINT_VERF" == "yes" ]; then 
   echo "Running verification to get rds files"
   $RS_DIR/point_verif/point_verif.R 
   mkdir -p $RS_DIR/../plot_point_verif/cases/$CASE_STUDY/
   cp -R $RS_DIR/../cases/$CASE_STUDY/output/verif_results/*.rds $RS_DIR/../plot_point_verif/cases/$CASE_STUDY/
fi 

if [ "$RUN_POINT_VERF_LOCAL" == "yes" ]; then 
   echo "Running complete graphic verification set"
   $RS_DIR/point_verif/point_verif_local.R   
   mkdir -p $RS_DIR/../plot_point_verif_local3/cases/$CASE_STUDY/
   cp -R $RS_DIR/../cases/$CASE_STUDY/output/*/*.png $RS_DIR/../plot_point_verif_local3/cases/$CASE_STUDY/
fi 

if [ "$SCORECARDS" == "yes" ]; then 
   echo "Running scorecards generation"
   $RS_DIR/point_verif/create_scorecards.R   
fi 

if [ "$SHOW_WEB" == "yes" ]; then
	$RS_DIR/visualization/shiny_launch.R
fi

if [ "$UPDATE_SHINYAPPSIO" == "yes" ]; then
        $RS_DIR/visualization/update_shinyappsio.R
fi


