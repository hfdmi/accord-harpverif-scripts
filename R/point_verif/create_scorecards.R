#!/usr/bin/env Rscript

# Create the scorecards

library(harp)
library(argparse)
library(here)


###
source(Sys.getenv('CONFIG_R'))
#Function to do the whole calculation
source(here("R","point_verif","fn_scorecard.R"))

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

#NOTE: store_false sets this to TRUE if the argument is NOT called, otherwise to FALSE if it is called
# Hence it is TRUE by default
parser$add_argument("-save_rds", action="store_false",
                   help="Save rds file for verification or not [default %(default)s]")

args <- parser$parse_args()


###
CONFIG <- conf_get_config()
params <- CONFIG$params_details

### 
start_date <- ifelse(is.null(args$start_date),CONFIG$shared$start_date,args$start_date)
end_date   <- ifelse(is.null(args$end_date),CONFIG$shared$end_date,args$end_date)



fcst_model <- CONFIG$scorecards$fcst_model
ref_model <- CONFIG$scorecards$ref_model
parameters <- CONFIG$scorecards$parameters

by_step         <- CONFIG$verif$by_step  #Read from config file
lead_time_str <- CONFIG$verif$lead_time
lead_time  <- eval(parse(text = lead_time_str))
fcst_type  <- CONFIG$verif$fcst_type
fcst_path  <- CONFIG$verif$fcst_path
obs_path   <- CONFIG$verif$obs_path
verif_path <- CONFIG$verif$verif_path
grps       <- CONFIG$verif$grps
plot_output <- CONFIG$post$plot_output

## 
date_path <- paste0(paste(start_date,end_date,sep="-"))
dir.create(file.path(fcst_model,date_path),recursive=TRUE)
pngfile <- paste(paste("scorecards",fcst_model,as.character(start_date),as.character(end_date),sep="_"),".png",sep="")
savepath <- file.path(plot_output,date_path,pngfile)

pooled_by <- "SID"
models_to_compare <- c(ref_model, fcst_model)

#Select stations. Placeholder for the moment. If NULL it will not filter any
selected_stations <- NULL #default



# Calculation starts here

scorecard_data <- lapply(
  parameters,
  scorecard_function,
  start_date = start_date,
  end_date   = end_date,
  by         = by_step,
  fcst_model = models_to_compare,
  fcst_type  = fcst_type,
  fcst_path  = fcst_path,
  obs_path   = obs_path,
  n          = 100,
  pooled_by  = "SID",
  stations   = selected_stations,
  min_cases  = 5
)


# The old version with pooled_bootstrap_score
#DO NOT USE, it is buggy
#scorecard_data <- bind_bootstrap_score(scorecard_data)

#This one uses the new bootstrap_verify function
scorecard_data <- bind_point_verif(scorecard_data)
print(scorecard_data)
#Save the verif data. Will this work here??? Only saving it for the whole domain
#The naming of the output file is set automatically. Not changing it at this point
#to avoid clashes with the shiny app
#save_point_verif(scorecard_data,".") #not saving this, no point

plot <- plot_scorecard(
  scorecard_data,
  fcst_model = fcst_model,
  ref_model  = ref_model,
  scores     = c("rmse", "stde", "bias")
) + theme(legend.text = element_text(size = 6)) #+ ggtitle(figtitle)

ggsave(savepath)

