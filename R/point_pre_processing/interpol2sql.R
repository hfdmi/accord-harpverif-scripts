#!/usr/bin/env Rscript
# Read vfld data and save it in sqlite format
library(harp)
library(argparse)
library(here)
library(Rgrib2)
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
fcst_templates <- CONFIG$pre$fcst_templates
params <- CONFIG$pre$params
clim_file <- CONFIG$pre$clim_file
clim_param <- CONFIG$pre$clim_param
interp_method <- CONFIG$pre$interp_method
lead_time_str <- CONFIG$verif$lead_time
lead_time  <- eval(parse(text = lead_time_str))
by         <- CONFIG$verif$by_step
do_all <- CONFIG$pre$do_all
model_output_path <- CONFIG$pre$model_output_path
cat("fcst_templates are",fcst_templates,"\n")
for (param in params)  #Loop through all params
{
cat("Trying with param",param,"\n")

    for (template in fcst_templates)
    {
    #In this block we check if this param has already been processed in other file template,
    #if not, we process it with this one
    dir_path <- paste(fcst_path,fcst_model,sep="/")
    file_pattern <- paste("FCTABLE_",param,"_",sep = "")
    cat("looking for files in ",dir_path,"\n")
    cat("with file pattern",file_pattern,"\n")
    matching_files <- list.files(path = dir_path, pattern = file_pattern, recursive=TRUE)
    if (length(matching_files) == 0) {
        cat("No existing files found, processing ",param, "in file template: ",template,"\n")
        read_forecast(
          dttm = seq_dttm(start_date,end_date,by_step)
          fcst_model     = fcst_model,
          parameter = param,
          lead_time = lead_time,
	  file_path     = model_output_path,
          file_template = template,
	  transformation = "interpolate",
	  transformation_opts = interpolate_opts(correct_t2m = FALSE,method = interp_method, clim_file = clim_file, clim_param = clim_param),
          output_file_opts =  sqlite_opts(path = fcst_path),
          return_data = TRUE
        )
    }
        else {
    cat("FC already found for",file_pattern," :","\n")
    print(matching_files)
}
    }
}

