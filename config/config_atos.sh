#/bin/bash 


## load modules
module load R


#THESE TWO ARE NOT IN USE!
#INI_DATE=2022050100
#END_DATE=2022051523

#
CONFIG_INITIAL=config_atos
 
#R Env
USING_RENV=${USING_RENV-no}


# What to run
RUN_POINT_VERF=${RUN_VERF-no}
RUN_POINT_VERF_LOCAL=${RUN_POINT_VERF_LOCAL-no}

SCORECARDS=${SCORECARDS-no}
RUN_VOBS2SQL=${RUN_VOBS2SQL-no}
RUN_VFLD2SQL=${RUN_VFLD2SQL-yes}


## DNOT for USER
MAIN_DIR=$(pwd)
CONFIG_DIR=$MAIN_DIR/config
RS_DIR=$MAIN_DIR/R
CONFIG_YAML=$CONFIG_DIR/$CONFIG_INITIAL.yml
CONFIG_R=$CONFIG_DIR/$CONFIG_INITIAL.R

##
#export INI_DATE END_DATE
export MAIN_DIR CONFIG_INITIAL  
export USING_RENV
export RUN_POINT_VERF  RUN_VOBS2SQL RUN_VFLD2SQL
export SCORECARDS


export CONFIG_DIR RS_DIR CONFIG_YAML CONFIG_R
