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
set -x  
export VERIF_DIR=/perm/sp3c/DE_330-verif-scripts/ #The location of your copy of DE_330's HARP point verification repo
export CASE_STUDY=Spain_202205

cd $VERIF_DIR
source config/config_atos.sh

########  
export RUN_POINT_VERF=no
export RUN_POINT_VERF_LOCAL=yes
export RUN_VOBS2SQL=no
export RUN_INTERPOL2SQL=no
export RUN_VFLD2SQL=no
export SCORECARDS=no
export SHOW_WEB_STATIC=no
export SHOW_WEB_DYNAMIC=no
export SHINY_PORT=3699 # Change this number if port is busy when launching web
######

if [ "$RUN_VOBS2SQL" == "yes" ]; then 
    echo "Running vobs2sql"
   $RS_DIR/point_pre_processing/vobs2sql.R  
fi 

if [ "$RUN_VFLD2SQL" == "yes" ]; then 
     echo "Running vfld2sql"
    $RS_DIR/point_pre_processing/vfld2sql.R 
fi 
if [ "$RUN_INTERPOL2SQL" == "yes" ]; then 
     echo "Running interpol2sql"
    $RS_DIR/point_pre_processing/interpol2sql.R 
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
   mkdir -p $RS_DIR/../plot_point_verif_local/cases/$CASE_STUDY/
   cp -R $RS_DIR/../cases/$CASE_STUDY/output/*/*.png $RS_DIR/../plot_point_verif_local/cases/$CASE_STUDY/
fi 

if [ "$SCORECARDS" == "yes" ]; then 
   echo "Running scorecards generation"
   $RS_DIR/point_verif/create_scorecards.R   
fi 

if [ "$SHOW_WEB_STATIC" == "yes" ]; then
	$RS_DIR/visualization/shiny_launch_static.R
fi
if [ "$SHOW_WEB_DYNAMIC" == "yes" ]; then
        $RS_DIR/visualization/shiny_launch_dynamic.R
fi
