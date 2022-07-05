#!/usr/bin/env Rscript
# Read vfld data and save it in sqlite format
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

 

### 
start_date <- ifelse(is.null(args$start_date),CONFIG$shared$start_date,args$start_date)
end_date   <- ifelse(is.null(args$end_date),CONFIG$shared$end_date,args$end_date)
fclen <- CONFIG$pre$fclen
vfld_path <- CONFIG$pre$vfld_path
file_template <- CONFIG$pre$vfld_template
fcst_path <- CONFIG$verif$fcst_path
fcst_model <- CONFIG$verif$fcst_model
params <- CONFIG$pre$params
lead_time_str <- CONFIG$verif$lead_time
lead_time  <- eval(parse(text = lead_time_str))


for (param in params)
{
    cat("Processing ",param,"\n")
    read_forecast(
      start_date    = start_date,
      end_date      = end_date,
      fcst_model     = fcst_model,
      parameter = param,
      lead_time = lead_time,
      file_path = vfld_path,
      file_template = file_template,
      output_file_opts =  sqlite_opts(path = fcst_path),
      return_data = TRUE
    )

}
