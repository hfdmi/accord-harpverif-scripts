#!/usr/bin/env Rscript

library(harpVis)
library(here)
#app_dir <- system.file("shiny_apps/plot_point_verif", package= "harpVis")
#shiny::shinyOptions(app_start_dir=paste( Sys.getenv(c("PERM")),"/deode_verif/",sep=""))
#shiny::runApp(app_dir, port=strtoi(Sys.getenv(c("SHINY_PORT"))))
runApp()
