#!/usr/bin/env Rscript

library(shiny)
library(here)
library(rsconnect)

#deployApp(appDir='/perm/sp3c/deode_verif/plot_point_verif_local1/', appName="plot_point_verif_local1",appTitle="DE_330 point verification static 1")
deployApp(appDir='/perm/sp3c/deode_verif/plot_point_verif/', appName="plot_point_verif",appTitle="DE_330 point verification dynamic")
deployApp(appDir='/perm/sp3c/deode_verif/plot_point_verif_local3/', appName="plot_point_verif_local3",appTitle="DE_330 point verification static 3")
#deployApp(appDir='/perm/sp3c/deode_verif/plot_point_verif_local2/', appName="plot_point_verif_local2",appTitle="DE_330 point verification static 2")
#deployApp(appDir='/perm/sp3c/deode_verif/plot_point_verif_local4/', appName="plot_point_verif_local4",appTitle="DE_330 point verification static 4")






