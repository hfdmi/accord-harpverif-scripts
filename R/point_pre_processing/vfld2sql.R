#!/usr/bin/env Rscript
# Read vfld data and save it in sqlite format
library(harp)
library(argparse)
library(here)
### 
renv::load(getwd())

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
do_all <- CONFIG$pre$do_all


#read_forecast(
#  dttm                = seq_dttm(2022021200, 2022021300, "1d"),
#  fcst_model          = "arome_arctic",
#  parameter           = c("RH2m", "T2m"),
#  lead_time           = seq(0, 24, 3),
#  transformation      = "interpolate", 
#  file_path           = here("data/netcdf"),
#  file_template       = template,
#  file_format_opts    = my_opts,
#  output_file_opts    = sqlite_opts(path = file.path(tempdir(), "FCTABLE")) 
#)

if (do_all) {
   print("Processing ALL parameters in vfld files!")
        read_forecast(
          dttm           = seq_dttm(start_date,end_date,"1h"),
          fcst_model     = fcst_model,
          parameter = NULL,
          lead_time = lead_time,
          file_path = vfld_path,
          file_template = file_template,
          output_file_opts =  sqlite_opts(path = fcst_path),
          return_data = TRUE
        )
} else {
    for (param in params)
    {
        cat("Processing ",param,"\n")
        read_forecast(
          dttm           = seq_dttm(start_date,end_date,"1h"),
          fcst_model     = fcst_model,
          parameter = param,
          lead_time = lead_time,
          file_path = vfld_path,
          file_template = file_template,
          output_file_opts =  sqlite_opts(path = fcst_path),
          return_data = TRUE
        )
    
    }
}
