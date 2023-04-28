#!/usr/bin/env Rscript

library(here)
library(shiny)
runApp(here('plot_point_verif_local1',port=strtoi(Sys.getenv(c("SHINY_PORT")))))
