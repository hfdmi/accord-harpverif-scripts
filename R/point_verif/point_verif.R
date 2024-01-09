#!/usr/bin/env Rscript

# If you are using renv, uncomment these two lines:
#print("Using renv. Loading environment")
#renv::load("/perm/sp3c/deode_verif")

# Basic script to run point verification and generate the corresponding rds files

library(harp)
library(purrr)
library(argparse)
library(here)

# sometimes it is useful to be able to use command-line-arguments.
# Adding here the options most usually need to be changed
# Values that usually remain unchanged are included in the config file
# conf_det_scores.R

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
params <- CONFIG$params_details


###
start_date <- ifelse(is.null(args$start_date),CONFIG$shared$start_date,args$start_date)
end_date   <- ifelse(is.null(args$end_date),CONFIG$shared$end_date,args$end_date)
by_step         <- CONFIG$verif$by_step  #Read from config file
fcst_model <- CONFIG$verif$fcst_model
lead_time_str <- CONFIG$verif$lead_time
lead_time  <- eval(parse(text = lead_time_str))
fcst_type  <- CONFIG$verif$fcst_type
lags       <- "0s" #only for ensembles. Leaving here for the moment
fcst_path  <- CONFIG$verif$fcst_path
obs_path   <- CONFIG$verif$obs_path
verif_path <- CONFIG$verif$verif_path
grps       <- CONFIG$verif$grps


# Some warnings in output
# Warning from recycling prolly comes from
# argument file_template. This does not change
# Default:     file_template = "fctable",

#Andrew's verification function below
# Function that runs the verification
run_verif <- function(prm_info, prm_name) {
  cat("Verifying ",prm_name,"\n")
  if (!is.null(prm_info$vc)) {
    vertical_coordinate <- prm_info$vc
  } else {
    vertical_coordinate <- NA_character_
  }
  
  # Read the forecast
  fcst <- read_point_forecast(
         dttm=seq_dttm(start_date,end_date,by_step),
         fcst_model    = fcst_model,
         fcst_type     = fcst_type,
         parameter     = prm_name,
         lead_time     = lead_time,
         lags          = lags,
         file_path     = fcst_path,
         vertical_coordinate = vertical_coordinate
       )
  # Find the common cases - for upper air parmeters we need to ensure 
  # that the level column  is included in the check
  fcst <- switch(
    vertical_coordinate,
    "pressure" = common_cases(fcst, p),
    "height"   = common_cases(fcst, z),
    common_cases(fcst)
  )
  # optional rescaling of forecasts using the scale_fcst part of the
  # params list. We use do.call to call the scale_point_forecast 
  # function with a named list containing the arguments. ##
  if (!is.null(prm_info$scale_fcst)) {
    fcst <- do.call(
      scale_param,list(fcst, prm_info$scale_obs$scaling, prm_info$scale_obs$new_units, prm_info$scale_obs$mult)
    )
  }
  # Read the observations getting the dates and stations from 
  # the forecast
  obs <- read_point_obs(
    #dttm=seq_dttm(start_date,end_date,by_step),
    dttm=unique_valid_dttm(fcst),
    parameter  = prm_name,
    obs_path   = obs_path,
    stations   = unique_stations(fcst),
    vertical_coordinate = vertical_coordinate
  )  
  # optional rescaling of observations using the scale_obs part of the
  # params list. We use do.call to call the scale_point_forecast 
  # function with a named list containing the arguments.
  if (!is.null(prm_info$scale_obs)) {
    obs <- do.call(
      scale_param, list(obs, prm_info$scale_obs$scaling, prm_info$scale_obs$new_units, prm_info$scale_obs$mult, col = {{prm_name}})
    )
  }
  
  # Join observations to the forecast
  fcst <- join_to_fcst(fcst, obs, force=TRUE)
  #Note by Samuel: Some stop clause is needed here if any of the models 
  #intersects 0 with the observations' SIDS (then remove force=TRUE)

  # Check for errors removing obs that are more than a certain number 
  # of standard deviations from the forecast. You could add a number 
  # of standard deviations to use in the params list 
  fcst <- check_obs_against_fcst(fcst, prm_name)

  # Make sure that grps is a list so that it adds on the vertical 
  # coordinate group correctly
  if (!is.list(grps)) {
    grps <- list(grps)
  }
  
  grps <- switch(
    vertical_coordinate,
    "pressure" = map(grps, ~c(.x, "p")),
    "height"   = map(grps, ~c(.x, "z")),
    grps
  )
  
  # Do the verification
  if (fcst_type == "eps") {
    verif <- ens_verify(
      fcst, prm_name, thresholds = prm_info$thresholds, groupings = grps
    )
  } else {
    verif <- det_verify(
      fcst, prm_name, thresholds = prm_info$thresholds, groupings = grps
    )
  }
  
  # Save the scores
  save_point_verif(verif, verif_path = verif_path)
  
  # Return the data to the calling environment (normally global)
  verif
  
}

print("Parameters to process:")
print(params)

# Use possibly from the purrr package to allow the script to continue
# if it fails for a parameter - it returns NULL if it fails. See
# ?safely and ?quietly if you want to retain the errors.
#possible_run_verif <- possibly(run_verif, otherwise = NULL)
possible_run_verif <- safely(run_verif, otherwise = NULL, quiet= FALSE)
print(possible_run_verif)

# Use imap from the purrr package to map each element of the params list
# to the possible_run_verif function. imap passes the element of the list
# as the first argument and the name of the element as the second.
verif <- imap(params, possible_run_verif)

# This will be added in the visualization part
# You can open the results in a shiny app using 
# shiny_plot_point_verif(verif_path)

