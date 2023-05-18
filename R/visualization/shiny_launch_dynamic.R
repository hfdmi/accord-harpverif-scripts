#!/usr/bin/env Rscript

library(harpVis)
library(here)
port <- Sys.getenv(c("SHINY_PORT"))
host <- Sys.getenv(c("HOST"))
print('To display shiny app in Firefox window at ATOS:')
print('1: open a new terminal')
print(paste0('execute this command: ssh -L ', port,':localhost:',port,' ',host))
print(paste0('Open a Firefox window and go to http://127.0.0.1:',port,'/'))
app_dir <- system.file("shiny_apps/plot_point_verif", package= "harpVis")
shiny::shinyOptions(app_start_dir=paste( Sys.getenv(c("PERM")),"/deode_verif/",sep=""))
shiny::runApp(app_dir, port=strtoi(Sys.getenv(c("SHINY_PORT"))))
runApp()
