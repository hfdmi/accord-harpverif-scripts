# Construct auxiliary scores from a harp forecast object i.e.
# 1) Daily variation
# 2) Forecast timeseries
# 3) Frequency distribution
# 4) Scatterplot

fn_plot_aux_scores <- function(fcst_input,png_archive,station_group_var = "station_group",
                               plot_num_cases = TRUE,cmap = "Set2",compare_mbr000 = FALSE,mbr_plots = FALSE){
  
  #=================================================#
  # INITIAL CHECKS 
  #=================================================#
  
  print("Working on initial checks for aux scores")
  
  if (is.list(fcst_input)){
    print("Aux scores plotting: The input file is assumed to be a harp forecast object")
  } else {
    stop("Aux scores plotting: A harp forecast object is required, aborting")
  }
  
  models_vec <- names(fcst_input)
  num_models <- length(models_vec)
  
  # DET or ENS?
  fcst_type <- "det"
  for (ii in seq(1,num_models,1)){
    c_names <- names(fcst_input[[ii]])
    c_names <- c_names[!(grepl("_det",c_names))]
    if (any(grepl("_mbr",c_names))){
      fcst_type <- "ens"
      break
    }
  }
  
  # Bind fcst to create one fcst data frame
  if (fcst_type == "ens"){
    # Add in ensemble mean and spread
    fcst <- ens_stats(fcst_input)
    fcst <- bind_fcst(fcst) # Can take a while
    fcst_names <- names(fcst)
    # TODO: Check in latest version of harp if the same behaviour occurs. When you bind_fcst after using ens_stats,
    # the mean and spread are added on as columns ens_mean/spread (these repeat for each member). Thus do some manipulation here
    # to add the mean/spread to the "member" column to tidy things up 
    # TODO: Possibly this is not recommended...may have to revisit
    df_meanspread <- NULL
    for (c_model in models_vec){
      df_tmp    <- fcst %>% filter(mname == c_model); members_tmp <- unique(df_tmp[["member"]])
      df_tmp    <- df_tmp %>% filter(member == members_tmp[1])
      df_mean   <- df_tmp %>% mutate(member = "mean",forecast = ens_mean)
      df_spread <- df_tmp %>% mutate(member = "spread",forecast = ens_spread)
      df_meanspread <- bind_rows(df_meanspread,df_mean,df_spread)
    }
    fcst <- bind_rows(fcst,df_meanspread); fcst <- select(fcst,-ens_mean,-ens_spread)
    rm(df_tmp,members_tmp,df_mean,df_spread,df_meanspread)
  } else if (fcst_type == "det"){
    fcst <- bind_fcst(fcst_input) 
    fcst_names <- names(fcst)
  }

  # Add in validhour and station group if they do not exist
  if (!("valid_hour" %in% fcst_names)){
    #fcst <- mutate(fcst,validhour=substr(YMDh(validdate),9,10))
    fcst <- expand_date(fcst,validdate)
    fcst <- mutate_list(fcst,valid_hour = sprintf("%02d",valid_hour))
  }
  if (!(station_group_var %in% fcst_names)){
    fcst <- mutate(fcst,"{station_group_var}" := "All")
  }
  
  # Useful variables
  param    <- unique(fcst[["parameter"]])
  if  (ifelse(length(grep('AccPcp', names(fcst),value=FALSE)) == 0, FALSE, TRUE) ){param <- grep('AccPcp', names(fcst),value=TRUE)}
  fcst     <- rename(fcst,"OBS"=all_of(param))
  cycles   <- sort(unique(fcst[["fcst_cycle"]]))
  stations <- unique(fcst[[station_group_var]])
  par_unit <- unique(fcst[["units"]])
  sdate    <- YMDh(first(sort(unique(fcst[["fcdate"]]))))
  edate    <- YMDh(last(sort(unique(fcst[["fcdate"]]))))
  
  # Check if the png_archive points to sdate-edate? If not, create a subdirectory
  d_end <- paste0(sdate,"-",edate)
  if (basename(png_archive) != d_end){
    png_archive <- file.path(png_archive,d_end)
    dir.create(file.path(png_archive),showWarnings = TRUE,recursive = FALSE)
  }
  
  #=================================================#
  # USER INTERACTION
  #=================================================#
  
  lt_to_use   <- seq(3,48,3) # What leadtimes to use?
  cycles_oi   <- c("All") # What cycles to plot for?
  stations_oi <- unique(c(stations,"All")) # What stations to plot for?
  fd_adjust   <- 1  # Adjust parameter in freq dist plotting
  
  # Scatterplot colormap
  #cmap_rgb    <- c("213,213,213", "195,195,231", "164,164,220", "143,143,204",
  #              "42,159,255", "42,205,255", "54,249,235", "163,255,126",
  #              "235,255,54", "255,219,42", "255,179,42", "255,137,42",
  #              "251,52,42", "199,42,42", "149,42,42", "118,0,248")
  #cmap_hex    <- sapply(strsplit(cmap_rgb, ","), function(x)
  #  rgb(x[1], x[2], x[3], maxColorValue=255))
  cmap_hex    <- brewer.pal(12,"Paired") # Something better available?
  scat_bins   <- 50
  
  #=================================================#
  # END OF USER INTERACTION 
  #=================================================#
  
  #================================================#
  # FIGURE OPTIONS
  #================================================#
  
  # Define some figure widths/heights
  fw <- 7
  fh <- 4.5
  fig_units <- "in"
  fig_dpi   <- 200

  # Define the colour scheme used in line plots (using Rcolorbrewer)
  cpal         <- brewer.pal(max(num_models+1,3),cmap) # Add +1 for "OBS"
  mcolors      <- cpal[1:(num_models+1)]
  
  # Define the color table
  names(mcolors) <- c(models_vec,"OBS") 
  
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
  
  title_str = paste0(str_to_title(param)," : ",format(str_datetime_to_datetime(sdate),"%Y-%m-%d-%H")," - ",
                     format(str_datetime_to_datetime(edate),"%Y-%m-%d-%H"))
  
  fxoption_list <- list("param" = param, "sdate" = sdate, "edate" = edate, "num_models" = num_models, "par_unit" = par_unit,
                        "fw" = fw, "fh" = fh, "mcolors" = mcolors,"line_styles" = line_styles, "line_size" = line_size, 
                        "stroke_size" = stroke_size, "point_size" = point_size, "ens_spec" = "NA", "c_ftyp" = fcst_type,
                        "ptheme_l" = ptheme_l, "ptheme_nc" = ptheme_nc, "png_archive" = png_archive, "cmap_hex" = cmap_hex,
                        "station_group_var" = station_group_var, "fd_adjust" = fd_adjust, "fig_units" = fig_units,
                        "scat_bins" = scat_bins , "plot_num_cases" = plot_num_cases, "fig_dpi" = fig_dpi)
  
  #=================================================#
  # COMPUTE VARIOUS AUX SCORES
  #=================================================#
  
  fcst      <- filter(fcst,leadtime %in% lt_to_use)
  leadtimes <- sort(unique(fcst[["leadtime"]]))
  
  if (length(leadtimes)>5){
    lt_used_fig <- c(leadtimes[1],leadtimes[2],"... ",leadtimes[length(leadtimes)])
  } else {
    lt_used_fig <- leadtimes
  }
  
  for (cycle in cycles_oi){
    
    if (cycle == "All"){
      cyc_used_fig  <- cycles 
      c_fcst <- fcst 
    } else {
      cyc_used_fig <- cycle
      c_fcst <- fcst %>% filter(fcst_cycle == cycle)
    }
    
    for (station in stations_oi){
    
      if (station == "All"){
        cc_fcst <- c_fcst 
      } else {
        cc_fcst <- c_fcst %>% filter(get(station_group_var) == station)
      }
      
      subtitle_str  <- paste0("Used {",paste0(cyc_used_fig,collapse=","),"} +",
                                 paste0(lt_used_fig,collapse = ", ")," : ",station," stations")
      
      # Define vroption list (with some dummy variables to be filled in later)
      vroption_list <- list("xgroup" = "xgroup", "score" = "score", "cycle" = cycle, "station" = station,
                            "c_typ" = "summary", "c_ftyp" = fxoption_list$c_ftyp, "xg_str" = "xg_str")
      # Split by fcst_type
      if (fcst_type == "det"){
        
        # All aux scores
        fn_aux(cc_fcst,title_str,subtitle_str,fxoption_list,vroption_list)
          
      } else if (fcst_type == "ens"){
        
        # Compute scores for the ensemble mean
        cc_fcst_mean <- cc_fcst %>% filter(member == "mean")
        c_title_str <- paste0("Ensemble mean : ",title_str)
        fn_aux(cc_fcst_mean,c_title_str,subtitle_str,fxoption_list,vroption_list)
        
        # Compute scores for the control members
        if (compare_mbr000){
          cc_fcst_ctrl <- cc_fcst %>% filter(member == "mbr000")
          c_title_str <- paste0("Mbr000 : ",title_str)
          fn_aux(cc_fcst_ctrl,c_title_str,subtitle_str,fxoption_list,vroption_list)
        }
        
        # Member scores (not really required)
        if (mbr_plots){
          group_vars <- c("mname","valid_hour","member")
          vroption_list$score <- "mbrdailyvar"; vroption_list$xgroup <- "valid_hour"; vroption_list$xg_str <- "vh"
          fn_dvar_ts(cc_fcst,group_vars,title_str,subtitle_str,fxoption_list,vroption_list)
          
          group_vars <- c("mname","validdate","member")
          vroption_list$score <- "mbrtimeseries"; vroption_list$xgroup <- "validdate"; vroption_list$xg_str <- "vd"
          fn_dvar_ts(cc_fcst,group_vars,title_str,subtitle_str,fxoption_list,vroption_list)
          
          vroption_list$xgroup <- "NA"; vroption_list$score <- "mbrfreqdist"; vroption_list$xg_str <- "NA";
          fn_freqdist(cc_fcst,title_str,subtitle_str,fxoption_list,vroption_list)
          
        }
        
      } # Det/eps
        
    } # station
    
  } # cycle
  
} # End of function

#================================================#
# HELPER FUNCTIONS
#================================================#

# Just a wrapper
#================================================#
fn_aux <- function(fc,title_str,subtitle_str,fxoption_list,vroption_list){
  
  if (grepl("Mbr000",title_str)){
    cprefix <- "ctrl"
  } else {
    cprefix <- ""
  }
  
  # Daily Var 
  group_vars <- c("mname","valid_hour")
  vroption_list$xgroup <- "valid_hour"; vroption_list$score <- paste0(cprefix,"dailyvar"); vroption_list$xg_str <- "vh"
  fn_dvar_ts(fc,group_vars,title_str,subtitle_str,fxoption_list,vroption_list)
  
  # Timeseries 
  group_vars <- c("mname","validdate")
  vroption_list$xgroup <- "validdate"; vroption_list$score <- paste0(cprefix,"timeseries"); vroption_list$xg_str <- "vd"
  fn_dvar_ts(fc,group_vars,title_str,subtitle_str,fxoption_list,vroption_list)
  
  # Frequency distribution 
  vroption_list$xgroup <- "NA"; vroption_list$score <- paste0(cprefix,"freqdist"); vroption_list$xg_str <- "NA";
  fn_freqdist(fc,title_str,subtitle_str,fxoption_list,vroption_list)
  
  # Scatter plots
  vroption_list$score <- paste0(cprefix,"scatterplot");
  fn_scatterplot(fc,title_str,subtitle_str,fxoption_list,vroption_list)
  
}
#================================================#

# Compute the daily variation/timeseries
#================================================#
fn_dvar_ts <- function(fc,group_vars,title_str,subtitle_str,fxoption_list,vroption_list){
  
  # Read options
  score <- vroption_list$score; station <- vroption_list$station;
  score_orig <- score
  if (grepl("mbr",score)){
    score <- gsub("mbr","",score)
  }

  if (grepl("ctrl",score)){
    score <- gsub("ctrl","",score)
    vroption_list$score <- score
  }
  
  dv <- fc %>% group_by_at(group_vars) %>% summarise(!!score := mean(forecast), mean_obs = mean(OBS),num_cases=n())
  
  # Replace mean_obs by "OBS" in mname (or member)
  if (grepl("mbr",score_orig)){
    dv_tmp <- filter(dv,member == unique(dv[["member"]][1]))
    dv_tmp <- mutate(dv_tmp,member = "OBS","{score}" := mean_obs)
  } else {
    dv_tmp <- filter(dv,mname == unique(dv[["mname"]])[1])
    dv_tmp <- mutate(dv_tmp,mname = "OBS","{score}" := mean_obs)
  }
  dv <- bind_rows(dv,dv_tmp); dv <- select(dv,-mean_obs)

  # Call plotting
  p_c   <- fn_plot_point(dv,title_str,subtitle_str,fxoption_list,vroption_list)
  
  if (fxoption_list$plot_num_cases & (!grepl("mbr",score_orig))){
    p_nc  <- fn_plot_numcases(dv,fxoption_list,vroption_list)
    p_c   <- grid.arrange(p_c,p_nc,ncol=1,nrow=2,widths = c(4),heights = c(4,1))
  }
  # Save png
  png_fname <- fn_png_name(fxoption_list$c_ftyp,score_orig,vroption_list$xg_str,fxoption_list$param,
                           vroption_list$cycle,fxoption_list$sdate,fxoption_list$edate,station)
  ggsave(p_c,filename=png_fname,path=fxoption_list$png_archive,width=fxoption_list$fw,height=fxoption_list$fh,
         units=fxoption_list$fig_units,dpi=fxoption_list$fig_dpi,device='png')
    
}
#================================================#

# Frequency distribution
#================================================#
fn_freqdist <- function(fc,title_str,subtitle_str,fxoption_list,vroption_list){
  
  title_scores <- gsub("mbr","",vroption_list$score) # Remove "mbr" from the title
  title_scores <- gsub("_"," ",str_to_title(title_scores)) # Remove underscore from score names
  ptitle <- paste0(paste0(title_scores,collapse = ", ")," : ",title_str)
  
  if (grepl("mbr",vroption_list$score)){
    fc_obs  <- fc %>% filter(member == unique(fc[["member"]][1]))
    fc_obs  <- fc_obs %>% mutate(member = "OBS",forecast = OBS)
    fc_mean <- fc %>% filter(member == "mean")
    fc_ctrl <- fc %>% filter(member == "mbr000")
    fc      <- fc %>% filter(!(member %in% c("mean","spread","OBS")))
    
    p_out <- ggplot()+
      geom_density(data=fc,aes(x=forecast,group=interaction(mname,member)),color="gray",
                   size=fxoption_list$line_size,adjust=fxoption_list$fd_adjust,alpha=0.5)+
      geom_density(data=fc_ctrl,aes(x=forecast,group=mname,color="mbr000"),
                   size=fxoption_list$line_size,adjust=fxoption_list$fd_adjust,alpha=0.5)+
      geom_density(data=fc_mean,aes(x=forecast,group=mname,color="mean"),
                   size=fxoption_list$line_size,adjust=fxoption_list$fd_adjust,alpha=0.5)+
      geom_density(data=fc_obs,aes(x=forecast,group=mname,color="OBS"),
                   size=fxoption_list$line_size,adjust=fxoption_list$fd_adjust,alpha=0.5)+
      facet_wrap(vars(mname),ncol=min(fxoption_list$num_models,2))+
      labs(x = fxoption_list$par_unit, y = "Relative frequency",color = "",
           title=ptitle,subtitle = subtitle_str)+fxoption_list$ptheme_l
    
  } else {
    fc_obs <- fc %>% filter(mname == unique(fc[["mname"]][1]))
  
    p_out <- ggplot()+
      geom_density(data=fc,aes(x=forecast,color=fct_inorder(mname)),size=fxoption_list$line_size,
                   adjust=fxoption_list$fd_adjust)+
      geom_density(data=fc_obs,aes(x=OBS,color="OBS"),size=fxoption_list$line_size,
                   adjust=fxoption_list$fd_adjust)+
      labs(x = fxoption_list$par_unit, y = "Relative frequency",color = "",
           title=ptitle,subtitle = subtitle_str)+
      fxoption_list$ptheme_l+scale_color_manual(values=fxoption_list$mcolors)
  
  }
  
  cp_xlim <- layer_scales(p_out)$x$range$range
  if ((cp_xlim[1]<0) && (cp_xlim[2]>0) && (!grepl("AccPcp",fxoption_list$param,fixed=TRUE))){
    p_out <- p_out + geom_vline(xintercept=0,size=fxoption_list$line_size,color="black",linetype="dashed")
  }  
  
  png_fname <- fn_png_name(fxoption_list$c_ftyp,vroption_list$score,vroption_list$xg_str,fxoption_list$param,
                           vroption_list$cycle,fxoption_list$sdate,fxoption_list$edate,vroption_list$station)
  ggsave(p_out,filename=png_fname,path=fxoption_list$png_archive,width=fxoption_list$fw,height=fxoption_list$fh,
         units=fxoption_list$fig_units,dpi=fxoption_list$fig_dpi,device='png')
  
}
#================================================#
  
# Scatterplot
# TODO: CAN MOVE OVER TO HARP'S PLOT_SCATTER?
# (HOW TO FACET BY FCST MODEL?)
#================================================#
fn_scatterplot <- function(fc,title_str,subtitle_str,fxoption_list,vroption_list){

  title_scores <- gsub("mbr","",vroption_list$score) 
  title_scores <- gsub("_"," ",str_to_title(title_scores))
  ptitle <- paste0(paste0(title_scores,collapse = ", ")," : ",title_str)
  
  p_scat <- fc %>% ggplot(aes(x=OBS,y=forecast))+
    geom_hex(bins=fxoption_list$scat_bins)+
    facet_wrap(vars((mname)),ncol=min(fxoption_list$num_models,3))
  max_count <- max(ggplot_build(p_scat)$data[[1]]$count)
  br1 <- round(logseq(1,max_count,5),0)
  p_scat <- p_scat +
    geom_abline(intercept = 0, slope = 1,color="black")+
    scale_fill_gradientn(" ",colors=fxoption_list$cmap_hex,
                         labels=br1,breaks=br1,trans="log")+
    labs(x = paste0("OBS (",fxoption_list$par_unit,")"),y = paste0("Forecast (",fxoption_list$par_unit,")"),
         title=ptitle,subtitle = subtitle_str)+fxoption_list$ptheme_l
  
  png_fname <- fn_png_name(fxoption_list$c_ftyp,vroption_list$score,vroption_list$xg_str,fxoption_list$param,
                           vroption_list$cycle,fxoption_list$sdate,fxoption_list$edate,vroption_list$station)
  ggsave(p_scat,filename=png_fname,path=fxoption_list$png_archive,width=fxoption_list$fw,height=fxoption_list$fh,
         units=fxoption_list$fig_units,dpi=fxoption_list$fig_dpi,device='png')
  
}
#================================================#
