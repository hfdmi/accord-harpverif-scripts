#!/usr/bin/env Rscript

library(here)
library(shiny)
port <- Sys.getenv(c("SHINY_PORT"))
host <- Sys.getenv(c("HOST"))
print('To display shiny app in Firefox window at ATOS:')
print('1: open a new terminal')
print(paste0('execute this command: ssh -L ', port,':localhost:',port,' ',host))
print(paste0('Open a Firefox window and go to http://127.0.0.1:',port,'/'))
runApp(here('plot_point_verif_local'),port=strtoi(Sys.getenv(c("SHINY_PORT"))))
