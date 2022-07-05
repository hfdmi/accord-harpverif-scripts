#!/usr/bin/env Rscript
# Read vobs data and save it in sqlite format
renv::load(getwd())

library(harp)
library(argparse)
library(here)

### 
source(Sys.getenv('CONFIG_R'))


###
parser <- ArgumentParser()

parser$add_argument("-start_date", type="character",
    default=NULL, 
    help="First date to process [default %(default)s]",
    metavar="Date in format YYYYMMDDHH")

parser$add_argument("-end_date", type="character",
    default=NULL,
    help="Final date to process [default %(default)s]",
    metavar="Date in format YYYYMMDDHH")

args <- parser$parse_args()	


###
CONFIG <- conf_get_config()

 
 
start_date <- ifelse(is.null(args$start_date),CONFIG$shared$start_date,args$start_date)
end_date   <- ifelse(is.null(args$end_date),CONFIG$shared$end_date,args$end_date)
vobs_path   <- CONFIG$pre$vobs_path
obs_path   <- CONFIG$verif$obs_path
by_step         <- CONFIG$verif$by_step  #Read from config file
fcst_model <- CONFIG$verif$fcst_model
lead_time  <- CONFIG$verif$lead_time


cat("Collecting vobs data  from ",start_date," to ",end_date)

obs_data <- read_obs_convert(
  start_date  = start_date,
  end_date    = end_date,
  obs_path    = vobs_path,
  sqlite_path = obs_path
  )


