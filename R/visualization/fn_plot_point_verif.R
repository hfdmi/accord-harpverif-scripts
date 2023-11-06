# Carry out all surface/upper-air plotting for det/eps harp verification object or an rds file
# NB:
# 1) Script under development, to be used with caution. 
# 2) Much of the plotting below can just be carried out with harp's plot_point_verif. This script
# was initally designed in order to have full control over the visualisation. 

fn_plot_point_verif <- function(harp_verif_input,png_archive,plot_num_cases = TRUE,cmap = "Set2",
                                     table_SIDS = FALSE,compare_mbr000 = FALSE){
 
  #=================================================#
  # INITIAL CHECKS - USER INTERACTION REQUIRED BELOW
  #=================================================#
  
  # Is the input a file path to an rds file or is it a harp verification object?
  if (is.list(harp_verif_input)){
    print("Point verif plotting: The input file is assumed to be a harp verification object")
    verif       <- harp_verif_input
  } else {
    print("Point verif plotting: Reading rds file input")
    verif       <- readRDS(harp_verif_input)
  }
  scores_tables <- names(verif)

  if (all(grepl("det_",scores_tables))){
    fcst_type <- "det"
    model_names <- unique(verif$det_summary_scores$mname)
  } else if (any(grepl("ens_",scores_tables))){
    fcst_type <- "ens"
    model_names <- unique(verif$ens_summary_scores$mname)
  } else {
    stop("Input does not look like a harpPoint verification object, aborting")
  }
  
  #=================================================#
  # USER INTERACTION REQUIRED HERE TO DEFINE
  # SCORES AND GROUPS TO PLOT 
  #
  # BY DEFAULT THIS WILL PRODUCE A SIMULAR SUITE OF 
  # PLOTS AS IN MONITOR
  #
  # THESE OPTIONS ARE SPLIT BY FCST_TYPE
  #================================================#
  
  # DETERMINISTIC EXP
  if (fcst_type == "det"){
    
    # NB: scores/groups should match the names available in the harp verification object.
    # If plotting multiple scores, use "~" as a seperator e.g. "bias~rmse", but still 
    # stick to the same base names.
    
    # START OF SURFACE SCORES
    
    # Scores as a fn of leadtime
    scores_lt  <- c("bias~rmse")
    # Scores as a fn of validdate
    scores_vd  <- c("bias~stde") 
    # Scores as a fn of threshold
    scores_th  <- c("threat_score","false_alarm_rate","false_alarm_ratio","kuiper_skill_score",
                    "frequency_bias","equitable_threat_score","extreme_dependency_score")
    # Note: Scores typically given in Monitor are below. See harp verif object for all threshold scores
    #scores_th <- c("threat_score","false_alarm_rate","false_alarm_ratio","kuiper_skill_score",
    #               "frequency_bias","equitable_threat_score","extreme_dependency_score",
    #               "symmetric_eds","extreme_dependency_index","symmetric_edi")
    # Map scores (combining scores won't work here)
    scores_mp  <- c("bias","rmse")
    
    # Leadtimes to plot when looking at scores_th
    #tleadtimes <- c("24","48","All")
    tleadtimes  <- c("0","6","12","18","24","30","36","42","48") #c("All")
    
    # Cycles to consider when plotting (summary, threshold scores as fn of leadtime, theshold scores as fn of threshold)
    cycles_summary      <- c("00","12","All")
    cycles_threshold_lt <- c("All")
    cycles_threshold_th <- c("00","12","All")
    
    # END OF SURFACE SCORES
    
    # START OF UA SCORES
    # Scores as a fn of leadtime
    p_scores_lt <- c("bias~rmse")
    
    # Profile scores
    p_scores_pr <- c("bias~stde")
    
    # END OF UA SCORES
    
    # NB: What groups are considered? 
    # This controls what group to look for in the verif object and then do something sensible from there).
    # There are several options available (note that the "p_*" refers to UA plotting):
    # 1) "leadtime", "validdate", "threshold", "p_leadtime" - Here the groups are used for the x-axis
    # 2) "SID" - For map scores
    # 3) "p_prof" - For UA profiles
    # 4) "other" - Some special ens scores which are handled by harp's plot_point_verif
    # In general one can leave xgroups as all of the above; if the data does not exist in the verif object,
    # it will simply be skipped
    xgroups    <- c("leadtime","validdate","threshold","SID","p_leadtime","p_prof")

    # EPS EXPERIMENT
  } else if (fcst_type == "ens"){
    
    # START OF SURFACE SCORES
    
    # Ensemble scores as a fn of leadtime
    scores_lt  <- c("mean_bias~rmse","rmse~spread","crps","fair_crps")
    # Ensemble scores as a fn of validdate
    scores_vd  <- c("mean_bias~stde")
    # Ensemble scores as a fn of thresholds
    scores_th  <- c("brier_score","brier_skill_score","fair_brier_score","roc_area")
    # Some of the most relevant scores:
    #scores_th    <- c("fair_brier_score","brier_score","brier_skill_score","brier_score_reliability",
    #                  "brier_score_resolution","brier_score_uncertainty","roc_area")
    # TODO: include option for brier score decomposition
    # Other scores (this covers rank_histogram, reliability, roc, and economic_value which have to be treated seperately)
    scores_ot  <- c("rank_histogram","roc","reliability")
    # Map scores
    scores_mp  <- c("mean_bias","rmse")
    
    # Finally, add in what member specific scores to consider.
    # The naming convention here is is important: For control member comparison, use prefix "ctrl" e.g. "ctrlbias~rmse" or
    # "ctrlmae" (here scores can be combined with ~).
    # For all members, use prefix "mbr" e.g. "mbrbias" which will plot all members and facet by model (here scores
    # cannot be combined with ~)
    scores_lt  <- c(scores_lt,"mbrbias","mbrrmse")
    if (compare_mbr000){
      scores_lt <- c(scores_lt,"ctrlbias~rmse")
      scores_vd <- c(scores_vd,"bias~stde")
      scores_mp <- c(scores_mp,"ctrlbias","ctrlrmse")
    }
    
    # Leadtimes to plot when looking at scores_th
    #tleadtimes <- c(seq(12,48,12),"All")
    tleadtimes <- c("0","6","12","18","24","30","36","42","48") #c("All")
    
    # Cycle to consider when plotting (summary, threshold scores as fn of leadtime, theshold scores as fn of threshold)
    cycles_summary      <- c("00","12","All")
    cycles_threshold_lt <- c("All")
    cycles_threshold_th <- c("00","12","All")
    
    # END OF SURFACE SCORES
    
    # START OF UA SCORES
    # Scores as a fn of leadtime
    p_scores_lt <- c("mean_bias~rmse")
    
    # Profile scores
    p_scores_pr <- c("mean_bias~stde","rmse~spread","crps","fair_crps")
    
    if (compare_mbr000){
      p_scores_lt <- c(p_scores_lt,"ctrlbias~rmse")
      p_scores_pr <- c(p_scores_pr,"bias~stde")
    }
    
    # ENS OF UA SCORES
    
    # What groups are considered? (see descriptionin "det" section above) 
    xgroups    <- c("leadtime","validdate","threshold","SID","other","p_leadtime","p_prof")

  }
  
  #================================================#
  # END OF USER INTERACTION
  # NORMALLY NO NEED TO EDIT BELOW THIS
  #================================================#
  
  #================================================#
  # SOME USEFUL ATTRIBUTES AND DEFINITIONS
  #================================================#
  
  # Get useful attributes
  num_models   <- length(model_names)
  param        <- attr(verif,"parameter")
  sdate        <- attr(verif,"start_date")
  edate        <- attr(verif,"end_date")
  # Note: The unit of the parameter is not included in the harp verif object by default (it seems?)
  # It should be manually added as an extra attribute upstream. If not, it will be set to empty
  par_unit     <- attr(verif,"unit") 
  if (is.null(par_unit)){
    par_unit <- " "
  }
  #num_stations <- attr() 
  # TODO: How to handle num_stations if the verif data contains station groups?
  # Need to return to this. 
  
  # Check if the png_archive points to sdate-edate? If not, create a subdirectory
  d_end <- paste0(sdate,"-",edate)
  if (basename(png_archive) != d_end){
    png_archive <- file.path(png_archive,d_end)
    dir.create(file.path(png_archive),showWarnings = TRUE,recursive = FALSE)
  }
  
  # Some ens scores to be dealt with seperately (using harp's plot_point_verif)
  ens_spec   <- c("rank_histogram","reliability","roc","economic_value")
  
  # Useful strings
  title_str = paste0(str_to_title(param)," : ",format(str_datetime_to_datetime(sdate),"%Y-%m-%d-%H")," - ",
                     format(str_datetime_to_datetime(edate),"%Y-%m-%d-%H"))
  
  # More useful attributes
  all_summary_scores   <- names(verif[[paste0(fcst_type,"_summary_scores")]])
  all_threshold_scores <- names(verif[[paste0(fcst_type,"_threshold_scores")]])
  threshold_vals       <- unique(verif[[paste0(fcst_type,"_threshold_scores")]][["threshold"]])
  
  #================================================#
  # FIGURE OPTIONS
  #================================================#
  
  # Define some figure widths/heights
  fw <- 7
  fh <- 4.5
  fw_map <- 10
  fh_map <- 4.5
  fig_units <- "in"
  fig_dpi   <- 200
  if (num_models==3){
    fw_map <- 12
  } else if (num_models > 3){
    print("Warning: Width/height for map plotting may not be optimal!")
    fw_map <- 12
    fh_map <- 7.5
  }
  
  # Define the colour scheme used in line plots (using Rcolorbrewer)
  cpal    <- brewer.pal(max(num_models,3),cmap)
  mcolors <- cpal[1:num_models]
  
  # Define the color table
  names(mcolors) <- model_names 
  
  # Line stylces
  line_styles <- c("solid","dashed","dotted","dotdash")
  
  # Some sizes
  line_size   <- 1
  point_size  <- 1.5
  stroke_size <- 0.75
  
  # Define various themes
  ptheme_l <- theme_bw()+ 
    theme(
      plot.title=element_text(size=10),
      plot.subtitle = element_text(size=10),
      axis.text=element_text(size=10),
      axis.title=element_text(size=10),
      strip.text = element_text(size = 10),
      legend.text = element_text(size=10),
      legend.position = "top"
    )
  ptheme_nc <- theme_bw()+ 
    theme(
      axis.text=element_text(size=10),
      axis.title=element_text(size=10),
      legend.position = "none"
    )
  
  #=====================================================#
  # LIST OF FIXED VALUES (USED FOR PASSING TO FUNCTIONS)
  #=====================================================#
  
  fxoption_list <- list("param" = param, "sdate" = sdate, "edate" = edate, "num_models" = num_models, "par_unit" = par_unit,
                        "fw" = fw, "fh" = fh, "fw_map" = fw_map, "fh_map" = fh_map, "mcolors" = mcolors, "ens_spec" = ens_spec,
                        "line_styles" = line_styles, "line_size" = line_size, "stroke_size" = stroke_size, "point_size" = point_size,
                        "ptheme_l" = ptheme_l, "ptheme_nc" = ptheme_nc, "png_archive" = png_archive, "fig_units" = fig_units,
                        "fig_dpi" = fig_dpi)
  
  #=====================================================#
  # NOW DO THE PLOTTING
  # "xgroup" DECIDES WHAT HAPPENS HERE
  # THIS INCLUDES ALL SUMMARY, THREHSOLD, VERTICAL PLOTS
  #=====================================================#
  
  # Now start multiple loops, structure is:
  # xgroup
  #   score
  #     fcst_cycle
  #       station group/domain
  # Something better could be done
  
  # Set verif_input as the original verif object
  verif_input <- verif
  
  for (xgroup in xgroups){
    
    c_ind = TRUE # Just a counter for printing
    
    # Define some options based on the x-axis
    if (xgroup == "leadtime"){
      xg_str <- "lt"
      scores <- scores_lt
    } else if (xgroup == "validdate"){
      xg_str <- "vd"
      scores <- scores_vd
    } else if (xgroup == "valid_hour"){
      xg_str <- "vh"
      scores <- sscores_vh
    } else if (xgroup == "threshold"){
      xg_str <- "th"
      scores <- scores_th
    } else if (xgroup == "other"){
      xg_str <- "NA"
      scores <- scores_ot
    } else if (xgroup == "SID"){
      xg_str <- "mp"
      scores <- scores_mp
    } else if (xgroup == "p_leadtime"){
      xg_str <- "lt"
      scores <- p_scores_lt
    } else if (xgroup == "p_prof"){
      xg_str <- "pr"
      scores <- p_scores_pr
    } else {
      stop(print0("The xgroup ",xgroup," is not considered! Aborting"))
    }

    for (score in scores){
      
      # Summary or threshold table?
      if ((grepl("ctrl",score)) || (grepl("mbr",score))){
        # Looking at member det scores 
        c_typ <- "summary"
        c_ftyp <- "det"
        score_orig <- score
        score <- gsub("mbr","",score)
        score <- gsub("ctrl","",score)
        cycles_oi <- cycles_summary
      } else if (all(strsplit(score,"~")[[1]] %in% all_threshold_scores)){
        c_typ <- "threshold"
        c_ftyp <- fcst_type
        score_orig <- score
        if (xgroup == "threshold"){
          cycles_oi <- cycles_threshold_th
        } else {
          cycles_oi <- cycles_threshold_lt
        }
      } else if (all(strsplit(score,"~")[[1]] %in% all_summary_scores)){
        c_typ <- "summary"
        c_ftyp <- fcst_type
        score_orig <- score
        cycles_oi <- cycles_summary
      } else { 
        c_typ <- "NA" # Score is not in the data frame and will be skipped
        c_ftyp <- fcst_type
        score_orig <- score
        cycles_oi <- cycles_summary
        #print(paste0("Warning: ",score," is not in the verif table and will be skipped"))
      }
    
      # Check for stations/cycles/lts/vhs available in the verif object
      c_ver      <- verif_input[[paste0(c_ftyp,"_",c_typ,"_scores")]]
      cnames     <- names(c_ver)
      if (is.null(cnames)){
        cnames  <- "NA"
      }
      tmp_out    <- fn_check_verif(c_ver,verif_input,cnames)
      verif      <- tmp_out$verif
      stations   <- tmp_out$stations
      cycles     <- tmp_out$cycles; cycles <- intersect(cycles,cycles_oi)
      leadtimes  <- tmp_out$leadtimes; leadtime_vals <- intersect(tleadtimes,leadtimes)
      validhours <- tmp_out$validhours
      station_group_var <- tmp_out$station_group_var
      rm(tmp_out)
      
      for (cycle in cycles){
        
        verif_I <- filter_list(verif,fcst_cycle == cycle)

        if (cycle == "All"){
          cy_str <- "All cycles used"
        } else {
          cy_str <- paste0(cycle,"Z cycle used")
        }
        
        for (station in stations){
          
          # Variable options
          vroption_list <- list("xgroup" = xgroup, "score" = score, "cycle" = cycle, "station" = station,
                                "c_typ" = c_typ, "c_ftyp" = c_ftyp, "xg_str" = xg_str)
            
          # TODO: A temorary subtitle string (the number of stations should be included, will return to this)
          subtitle_str <- paste0(station," stations : ",cy_str)
          
          verif_II <- filter_list(verif_I,get(station_group_var) == station)

          # Does the score+xgroup exist in table? Also split according to xgroup
          # This works for standard threshold and summary scores
          if ((!(xgroup %in% c("SID"))) & (xgroup %in% cnames) & (!("p" %in% cnames)) & (all(strsplit(score,"~")[[1]] %in% cnames))){
            
            if (c_ind){
              print(paste0("Working on xgroup: ",xgroup))
              c_ind = FALSE
            }
            
            # Plot
            if (c_typ == "threshold"){
              
              # Here we are either plotting as a fn of leadtime or threshold
              if (xgroup == "threshold"){
                loop_values <- leadtime_vals; filter_col  <- "leadtime"
                verif_III   <- verif_II
              } else if (xgroup == "leadtime"){
                loop_values <- threshold_vals; filter_col  <- "threshold"
                # Remove case where we have threshold scores over all leadtimes
                verif_III   <- filter_list(verif_II,leadtime != "All") 
              } 
              for (ii in loop_values){
                verif_IIII  <- filter_list(verif_III,get(filter_col) == ii)
                c_subtitle <- paste0(subtitle_str," : ",str_to_title(filter_col)," = ",ii)
                if (xgroup == "threshold"){
                  vlt = ii; vth = "NA"
                } else if (xgroup == "leadtime"){
                  vlt = "NA"
                  if (ii < 0){
                    vth <- paste0("m",abs(ii))
                  } else {
                    vth <- ii
                  }
                }
                p_c  <- fn_plot_point(verif_IIII,title_str,c_subtitle,fxoption_list,vroption_list,vlt=vlt,vth=vth)
                
                # Call numcases plot if desired
                if (plot_num_cases){
                  p_numcases <- fn_plot_numcases(verif_IIII,fxoption_list,vroption_list)
                  p_c <- grid.arrange(p_c,p_numcases,ncol=1,nrow=2,widths = c(4),heights = c(4,1))
                }
                # Save png
                png_fname <- fn_png_name(fcst_type,score_orig,xg_str,param,cycle,sdate,edate,station,vlt=vlt,vth=vth)
                ggsave(p_c,filename=png_fname,path=png_archive,width=fw,height=fh,units=fig_units,dpi=fig_dpi,device='png')
              } # lt
              
            } else if (c_typ == "summary"){ 
              verif_III <- verif_II
              
              if (grepl("ctrl",score_orig)){
                verif_III[[3]] <- filter(verif_III[[3]],member == "mbr000")
                c_title_str <- paste0("Mbr000 : ",title_str)
              } else if (grepl("mbr",score_orig)){
                c_title_str <- paste0("All Members : ",title_str)
                vroption_list$score <- score_orig
              } else {
                c_title_str <- title_str
              }
              
              p_c <- fn_plot_point(verif_III,c_title_str,subtitle_str,fxoption_list,vroption_list)
  
              if ((plot_num_cases) & (!grepl("mbr",score_orig))){
                p_numcases <- fn_plot_numcases(verif_III,fxoption_list,vroption_list)
                p_c <- grid.arrange(p_c,p_numcases,ncol=1,nrow=2,widths = c(4),heights = c(4,1))
              }
              png_fname <- fn_png_name(fcst_type,score_orig,xg_str,param,cycle,sdate,edate,station)
              ggsave(p_c,filename=png_fname,path=png_archive,width=fw,height=fh,units=fig_units,dpi=fig_dpi,device='png')
              
            } # if c_typ
           
          # Now look at the "other" group
          } else if ((xgroup == "other") & (score %in% ens_spec)){
        
            if (c_ind){
              print(paste0("Working on xgroup: ",xgroup))
              c_ind = FALSE
            }
    
            if (score == "rank_histogram"){
              p_out <- fn_plot_point(verif_II,title_str,subtitle_str,fxoption_list,vroption_list)
            } else {
              # Loop over all leadtime+threshold pairs
              for (lt in leadtime_vals){
                for (th in threshold_vals){
                  verif_III <- verif_II
                  verif_III[[2]] <- filter(verif_III[[2]],leadtime == lt,threshold == th)
                  if (th < 0){
                    vth <- paste0("m",abs(th))
                  } else {
                    vth <- th
                  }
                  c_subtitle <- paste0(subtitle_str," : Leadtime = ",lt," : Threshold = ",th)
                  p_out <- fn_plot_point(verif_III,title_str,c_subtitle,fxoption_list,vroption_list,vlt=lt,vth=vth)
                }
              }
            } # score
          
          # Map scores
          } else if ((xgroup == "SID") & (xgroup %in% cnames) & (all(strsplit(score,"~")[[1]] %in% cnames))){
            
            if (c_ind){
              print(paste0("Working on xgroup: ",xgroup))
              c_ind = FALSE
            }
            
            if (validhours[1] == "NA"){
              # Average scores only
              c_subtitle <- paste0(subtitle_str," : Average")
              verif_III <- verif_II
              if (grepl("ctrl",score_orig)){
                verif_III[[3]] <- filter(verif_III[[3]],member == "mbr000")
                c_title_str <- paste0("Mbr000 : ",title_str)
              } else {
                c_title_str <- title_str
              }
              p_c <- fn_plot_map(verif_III,c_title_str,c_subtitle,fxoption_list,vroption_list)
              
              png_fname <- fn_png_name(fcst_type,score_orig,xg_str,param,cycle,sdate,edate,station,vlt="All")
              ggsave(p_c,filename=png_fname,path=png_archive,width=fw_map,height=fh_map,units=fig_units,dpi=fig_dpi,device='png')
              
            } else {
              
              for (vhour in validhours){
                verif_III <- verif_II
                if (vhour != "All"){
                  c_subtitle  <- paste0(subtitle_str," : Valid hour = ",vhour,"Z")
                } else {
                  c_subtitle  <- paste0(subtitle_str," : Average")
                }
                
                if (grepl("ctrl",score_orig)){
                  verif_III[[3]] <- filter(verif_III[[3]],member == "mbr000",valid_hour == vhour)
                  c_title_str <- paste0("Mbr000 : ",title_str)
                } else {
                  verif_III[[1]] <- filter(verif_III[[1]],valid_hour == vhour)
                  c_title_str <- title_str
                }
                p_c <- fn_plot_map(verif_III,c_title_str,c_subtitle,fxoption_list,vroption_list)
                
                if (is.list(p_c)){
                  png_fname <- fn_png_name(fcst_type,score_orig,xg_str,param,cycle,sdate,edate,station,vlt = vhour)
                  ggsave(p_c,filename=png_fname,path=png_archive,width=fw_map,height=fh_map,units=fig_units,dpi=fig_dpi,device='png')
                }
                
                # Optional SID table output (just for debugging/filtering stations)
                # Lists 10 stations with largest RMSE
                if (table_SIDS){
                  fn_sid_issue_table(verif_III,fxoption_list,vroption_list,vlt=vhour)
                }
                
              } # validhour
              
            } # if validhour exists 
              
          # UA scores
          } else if ((xgroup %in% c("p_leadtime","p_prof")) & (all(strsplit(score,"~")[[1]] %in% cnames)) & ("p" %in% cnames)){
           
            if ((xgroup == "p_leadtime") & ("leadtime" %in% cnames)){
              
              if (c_ind){
                print(paste0("Working on xgroup: ",xgroup))
                c_ind = FALSE
              }
              
              vroption_list$xgroup <- "leadtime"
              
              # Get the available pressure levels
              p_levels <- unique(verif_II[[1]][["p"]])
              # Loop over pressure levels and plot summary scores
              for (pl in p_levels){
                
                verif_III    <- filter_list(verif_II,p == pl)
                param_rename <- paste0(str_to_title(param),pl)
                c_title_str  <- gsub(paste0(str_to_title(param)," : "),paste0(param_rename,"hPa : "),title_str)
                
                if (grepl("ctrl",score_orig)){
                  verif_III[[3]] <- filter(verif_III[[3]],member == "mbr000")
                  c_title_str <- paste0("Mbr000 : ",c_title_str)
                }
                p_c <- fn_plot_point(verif_III,c_title_str,subtitle_str,fxoption_list,vroption_list)
                
                if ((plot_num_cases) & (!grepl("mbr",score_orig))){
                  p_numcases <- fn_plot_numcases(verif_III,fxoption_list,vroption_list)
                  p_c <- grid.arrange(p_c,p_numcases,ncol=1,nrow=2,widths = c(4),heights = c(4,1))
                }
                png_fname <- fn_png_name(fcst_type,score_orig,xg_str,param,cycle,sdate,edate,station,vlt=pl)
                ggsave(p_c,filename=png_fname,path=png_archive,width=fw,height=fh,units=fig_units,dpi=fig_dpi,device='png')

              } # pressure levels
              
            } # if p_leadtime
            
            if ((xgroup == "p_prof") & ("valid_hour" %in% cnames)){
              
              if (c_ind){
                print(paste0("Working on xgroup: ",xgroup))
                c_ind = FALSE
              }
              
              vhours <- intersect(validhours,c("00","12"))
              
              for (vh in vhours){
                
                verif_III    <- filter_list(verif_II,valid_hour == vh)

                if (grepl("ctrl",score_orig)){
                  verif_III[[3]] <- filter(verif_III[[3]],member == "mbr000")
                  c_title_str <- paste0("Mbr000 : ",title_str)
                } else {
                  c_title_str <- title_str
                }
              
                if (vh != "All"){
                  c_subtitle  <- paste0(subtitle_str," : Valid hour = ",vh,"Z")
                } else {
                  c_subtitle  <- paste0(subtitle_str," : Average")
                }
                
                p_c <- fn_plot_profile(verif_III,c_title_str,c_subtitle,plot_num_cases,fxoption_list,vroption_list)
                
                png_fname <- fn_png_name(fcst_type,score_orig,xg_str,param,cycle,sdate,edate,station,vlt = vh)
                ggsave(p_c,filename=png_fname,path=png_archive,width=fw,height=fh,units=fig_units,dpi=fig_dpi,device='png')

              } # validhour

            } # if p_prof
            
          } else { # If xgroup+score exists
        
            #print(paste0("Combination xgroup = ",xgroup," and score = ",score," not found, skipping..."))
            
          } # If xgroup exists
        } # station
      } # cycle
    } # Scores
  } # xgroup

} # End of function

#================================================#
# HELPER FUNCTIONS
#================================================#

# Just returns some information from the
# verif object
#================================================#
fn_check_verif <- function(df,verif,dfnames){
  # Check if a "station_group" exists
  station_group_var = "station_group"
  if (station_group_var %in% dfnames){
    stations <- unique(df[[station_group_var]])
  } else {
    stations <- c("All")
    verif <- verif %>% mutate_list("{station_group_var}" := "All") # Add identifier if none exists
    # Note that {} and := are used to assign the value of the variable to a column
  }
  # The same for fcst_cycle
  if ("fcst_cycle" %in% dfnames){
    cycles <- unique(df[["fcst_cycle"]])
  } else {
    cycles <- c("All")
    verif <- verif %>% mutate_list(fcst_cycle = "All")
  }
  # And check what leadtimes exist
  if ("leadtime" %in% dfnames){
    leadtimes <- unique(df[["leadtime"]])
  } else {
    leadtimes <- "NA"
  }
  # And check what validhours exist
  if ("valid_hour" %in% dfnames){
    validhours <- unique(df[["valid_hour"]])
  } else {
    validhours <- "NA"
  }
  return(list("verif" = verif,
              "cycles" = cycles,
              "stations" = stations,
              "leadtimes" = leadtimes,
              "validhours" = validhours,
              "station_group_var" = station_group_var))
}
#================================================#

# Used for plotting maps
#================================================#
fn_plot_map <- function(verif,title_str,subtitle_str,fxoption_list,vroption_list){
  
  # Read options
  c_typ <- vroption_list$c_typ; c_ftyp <- vroption_list$c_ftyp; score <- vroption_list$score
  num_models <- fxoption_list$num_models; par_unit <- fxoption_list$par_unit
  station <- vroption_list$station
  
  df <- verif[[paste0(c_ftyp,"_",c_typ,"_scores")]]
  
  # Split sscore into its individual parts
  all_scores   <- strsplit(score,"~")[[1]]
  num_scores   <- length(all_scores)
  title_scores <- gsub("mbr","",all_scores) # Remove "mbr" from the title
  title_scores <- gsub("_"," ",str_to_title(title_scores)) # Remove underscore from score names
  
  # Plot title
  ptitle <- paste0(paste0(title_scores,collapse = ", ")," : ",title_str)
  
  # In the map plotting it is assumed that the SID lat/lons are included in the verification object
  # Otherwise, nothing can be done
  if ((all(c("lat","lon") %in% names(df))) & (num_scores == 1)){
  
    # Colorbar options
    c_min <- min(df[[score]],na.rm=TRUE); c_max <- max(df[[score]],na.rm=TRUE)
    c_min <- c_min - 0.1; c_max <- c_max + 0.1; c_min <- round(c_min,1); c_max <- round(c_max,1)
    
    min_lon <- min(df[["lon"]]); max_lon <- max(df[["lon"]])
    min_lat <- min(df[["lat"]]); max_lat <- max(df[["lat"]])
    
    p_map <- df %>% ggplot(aes(lon,lat,fill=get(score),size=abs(get(score))))
    p_map_col <- scale_fill_gradient2(
      paste0("(",par_unit,")"), guide=guide_colourbar(title.position="top"),
      low  ="blue4",mid  ="white",high ="red4",n.breaks = 5,limits = c(c_min,c_max))

    p_map <- p_map + geom_polygon(data = map_data("world"),mapping = aes(long, lat, group = group),
                                  fill = "grey100", colour = "black",inherit.aes = FALSE) +  
                     geom_point(colour='grey40',pch=21) + 
                     coord_map(projection = "lambert",lat0=63,lat1=63,xlim=c(min_lon,max_lon),ylim=c(min_lat,max_lat)) +
                     theme(panel.background = element_rect(fill="grey95"),
                          panel.grid       = element_blank(),
                          axis.text        = element_blank(),
                          axis.ticks       = element_blank(),
                          axis.title       = element_blank(),
                          plot.title       = element_text(size=14),
                          legend.text      = element_text(size=10),
                          plot.subtitle    = element_text(size=12), 
                          legend.title     = element_text(size=12),
                          legend.position  = "right", 
                          strip.background = element_rect(fill="white"),
                          strip.text       = element_text(size=14))+
                    facet_wrap(vars(mname),ncol=min(num_models,3))+
                    scale_size_continuous(range = c(0.1, 3))+ # Controls the overall point size
                    labs(title = ptitle,subtitle = subtitle_str,fill = "",size = "")+
                    guides(size="none") # Remove size label from legend
    
    p_map <- p_map + p_map_col
    
    return(p_map)
  } else {
    print("Cannot plot maps as lat/lon missing or multiple scores have been specified, skipping...")
    return(NA)
  }
  
}
#================================================#

# Plotting of UA profiels
#================================================#
fn_plot_profile <- function(verif,title_str,subtitle_str,plot_num_cases,fxoption_list,vroption_list){
  
  # Read options
  c_typ <- vroption_list$c_typ; c_ftyp <-vroption_list$c_ftyp; score <- vroption_list$score; xgroup <- vroption_list$xgroup
  cycle <- vroption_list$cycle; station <- vroption_list$station; xg_str <- vroption_list$xg_str
  line_size <- fxoption_list$line_size; stroke_size <- fxoption_list$stroke_size; ptheme_l <- fxoption_list$ptheme_l
  mcolors <- fxoption_list$mcolors; line_styles <- fxoption_list$line_styles; par_unit <- fxoption_list$par_unit
  png_archive <- fxoption_list$png_archive; sdate <- fxoption_list$sdate;  ptheme_nc <- fxoption_list$ptheme_nc; 
  edate <- fxoption_list$edate; fw <- fxoption_list$fw; fh <- fxoption_list$fh
  param <- fxoption_list$param; num_models <- fxoption_list$num_models; point_size <- fxoption_list$point_size
  
  df <- verif[[paste0(c_ftyp,"_",c_typ,"_scores")]]
  
  # Split sscore into its individual parts
  all_scores   <- strsplit(score,"~")[[1]]
  num_scores   <- length(all_scores)
  title_scores <- gsub("mbr","",all_scores) # Remove "mbr" from the title
  title_scores <- gsub("_"," ",str_to_title(title_scores)) # Remove underscore from score names
  
  # Plot title
  ptitle <- paste0(paste0(title_scores,collapse = ", ")," : ",title_str)
  
  # Note: No member plotting considered here 
  if (num_scores == 1){
    p_out <- df %>% ggplot(aes(y=p,color=fct_inorder(mname)))+
      geom_path(aes(x=get(all_scores)),size=line_size)+
      geom_point(aes(x=get(all_scores)),stroke=stroke_size,size=point_size)
  } else if (num_scores == 2){
    p_out <- df %>% ggplot(aes(y=p,color=fct_inorder(mname)))+
      geom_path(aes(x=get(all_scores[1]),linetype=title_scores[1]),size=line_size)+
      geom_point(aes(x=get(all_scores[1])),stroke=stroke_size,size=point_size)+
      geom_path(aes(x=get(all_scores[2]),linetype=title_scores[2]),size=line_size)+
      geom_point(aes(x=get(all_scores[2])),stroke=stroke_size,size=point_size)
      #scale_linetype_manual(values=line_styles)
  } else {
    stop(paste0("Number of scores = ",num_scores," is not considered in plot_profile"))
  }
  
  p_out <- p_out + scale_y_reverse() + labs(y = "p (hPa)",x = par_unit,color = "",linetype = "") + 
    ptheme_l+scale_color_manual(values=mcolors)
  
  # Add zero line if relevant
  cp_xlim <- layer_scales(p_out)$x$range$range
  if ((cp_xlim[1]<0) && (cp_xlim[2]>0)){
    p_out <- p_out + geom_vline(xintercept=0,size=line_size,color="black",linetype="dashed")
  }
  
  # Combine plot and numcases grobs side-by-side. To do this, we want a common legend over the plot, 
  # which is obtained by usingfn_prof_legend
  mylegend <- fn_prof_legend(p_out)

  if (plot_num_cases){
    ncy    <- "num_cases"
    ylabel <- "Num. cases"
    p_nc <- df %>% ggplot(aes(y=p,color=fct_inorder(mname)))+
      geom_path(aes(x=get(ncy)),size=line_size)+
      geom_point(aes(x=get(ncy)),stroke=stroke_size,size=point_size)+
      labs(y = " ",x = ylabel,color = " ",shape = " ")+ptheme_nc+
      scale_color_manual(values=mcolors)+scale_y_reverse() + 
      scale_x_continuous(breaks=scales::breaks_pretty(n=3))
  
    p_tot <- grid.arrange(mylegend,
                        arrangeGrob(p_out + theme(legend.position="none"),p_nc + theme(legend.position="none"),
                        ncol=2, nrow = 1,widths = c(4,1.0), heights = c(4)),
                        top=textGrob(paste0(ptitle," \n",subtitle_str),x=0.1,hjust=0,gp = gpar(fontsize = 10)),
                        nrow=2,heights=c(1,9))
  } else {
    
    p_tot <- grid.arrange(mylegend,
                          arrangeGrob(p_out + theme(legend.position="none"),
                                      ncol=1, nrow = 1,widths = c(4), heights = c(4)),
                          top=textGrob(paste0(ptitle," \n",subtitle_str),x=0.1,hjust=0,gp = gpar(fontsize = 10)),
                          nrow=2,heights=c(1,9))
  }
  
  return(p_tot)
  
}
#================================================#

#================================================#
fn_prof_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
#================================================#

#================================================#
# Highlight which stations have large rmse
fn_sid_issue_table <- function(df,fxoption_list,vroption_list,vlt="NA",vth="NA"){
  
  # Read options
  c_ftyp <-vroption_list$c_ftyp; score <- vroption_list$score;
  cycle <- vroption_list$cycle; station <- vroption_list$station; xg_str <- vroption_list$xg_str
  png_archive <- fxoption_list$png_archive; sdate <- fxoption_list$sdate;
  edate <- fxoption_list$edate; param <- fxoption_list$param
  
  mnames       <- unique(df[[1]][["mname"]])
  num_stations <- unique(df[[1]][["SID"]])
  stns_lrmse   <- df[[1]] %>% filter(mname == mnames[1]) %>% arrange(-rmse)
  ns_val       <- min(10,num_stations) # Info for 10 stations with largest rmse
  stns_lrmse   <- stns_lrmse[1:ns_val,] %>% select("mname","valid_hour","fcst_cycle","SID","lat","lon","rmse","num_cases")
  stns_lrmse[["rmse"]] <- round(stns_lrmse[["rmse"]],2)
  
  # Get corresponding png name
  png_fname <- fn_png_name(c_ftyp,score,xg_str,param,cycle,sdate,edate,station,vlt,vth)
  tab_fname <- paste0("SIDtable-",png_fname); tab_fname <- gsub(".png",".csv",tab_fname)
  
  # Save as a table
  write.table(stns_lrmse,file=file.path(png_archive,tab_fname),sep=",",row.names = FALSE)
}
#================================================#
