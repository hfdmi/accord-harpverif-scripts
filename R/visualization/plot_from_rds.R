#!/usr/bin/env Rscript
# Plot the rds output

library(harp)
library(purrr)
library(here)
library(argparse)
library(yaml)

parser <- ArgumentParser()

parser$add_argument("-rds_file", type="character",
    default="None",
    help="File to process [default %(default)s]",
    metavar="Path to the rds file")

parser$add_argument("-config_file", type="character",
    default="config_examples/config.yml",
    help="Last date to process [default %(default)s]",
    metavar="String")

parser$add_argument("-score", type="character",
    default="bias",
    help="Score to plot [default %(default)s]",
    metavar="String")

# This is an extra label for the stations
parser$add_argument("-stations_label", type="character",
    default="ALL",
    help="The stations I am plotting [default %(default)s]",
    metavar="String")


args <- parser$parse_args()
config_file <- args$config_file
cat("Using config file ",config_file,"\n")
CONFIG <- yaml.load_file(here(config_file))
rds_file <- args$rds_file

#Split name to get param, date range  and model
filename <- gsub(".harp.","-",basename(rds_file))
filename <- gsub(".rds","",filename)
split_name <- strsplit(filename,"-")
param <- split_name[[1]][2]
start_date <- split_name[[1]][3]
end_date <- split_name[[1]][4]
model <- split_name[[1]][5]
score <- args$score
stations_label <- args$stations_label

#Template for output file (SUBJECT TO CHANGE!)
# PARAM-SCORE-STATIONS_LABEL-START_DATE-END_DATE.png
# Example: T2m-bias-ALL-2020020312-2020021712.png
png_file <- paste(paste(param,score,stations_label,start_date,end_date,sep="-"),".png",sep="")
subDir <- paste(start_date,end_date,sep="-")
dir.create(file.path(model, subDir), showWarnings = FALSE,recursive = TRUE)
read_verif <- readRDS(rds_file)
plot_point_verif(read_verif, {{score}},plot_num_cases=FALSE,x_axis=leadtime)
ggsave(file.path(model,subDir,png_file))
