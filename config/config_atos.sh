#/bin/bash 
## load modules
module load R

CONFIG_INITIAL=config_atos
 
#R Env
USING_RENV=${USING_RENV-no}

export VERIF_DIR=/home/nhd/R/dev_accord/accord-verif-scripts #The location of this repo
export CASE_STUDY=Spain_202205

# What to run
export RUN_POINT_VERF=${RUN_VERF-yes}
export RUN_POINT_VERF_LOCAL=${RUN_POINT_VERF_LOCAL-no}

export SCORECARDS=${SCORECARDS-no}
export RUN_VOBS2SQL=${RUN_VOBS2SQL-no}
export RUN_VFLD2SQL=${RUN_VFLD2SQL-no}

export SHOW_WEB_STATIC=no
export SHOW_WEB_DYNAMIC=no
export SHINY_PORT=3699 # Change this number if port is busy when launching web


## DNOT for USER
MAIN_DIR=$(pwd)
CONFIG_DIR=$MAIN_DIR/config
RS_DIR=$MAIN_DIR/R
CONFIG_YAML=$CONFIG_DIR/$CONFIG_INITIAL.yml
CONFIG_R=$CONFIG_DIR/$CONFIG_INITIAL.R

##
export MAIN_DIR CONFIG_INITIAL  
export USING_RENV
export CONFIG_DIR RS_DIR CONFIG_YAML CONFIG_R
