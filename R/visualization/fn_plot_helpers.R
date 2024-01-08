# HELPER FUNCTIONS FOR PLOTTING

#================================================#
# HELPER FUNCTION TO DEFINE THE PNG NAME
#================================================#

fn_png_name <- function(fcst_type,score,xg_str,param,cycle,sdate,edate,station,vlt="NA",vth="NA"){
  # Save pngs using the following file template:
  # PARAM-FTYPE-SCORE-XAXIS-FC-DTGSTART-DTGEND-STATION-VALID(LT)-THRESHOLD.png
  SCORE     <- paste0(fcst_type,"-",score,"-",xg_str)
  png_fname <- paste(param,SCORE,cycle,sdate,edate,station,vlt,vth,sep="-")
  png_fname <- paste0(png_fname,".png")
  return(png_fname)
}
#================================================#

#================================================#
# HELPER FUNCTION FOR SIMPLE PLOTS
#================================================#

fn_plot_point <- function(verif,title_str,subtitle_str,fxoption_list,vroption_list,vlt="NA",vth="NA"){
  
  # Read options
  c_typ <- vroption_list$c_typ; c_ftyp <-vroption_list$c_ftyp; score <- vroption_list$score; xgroup <- vroption_list$xgroup
  cycle <- vroption_list$cycle; station <- vroption_list$station; xg_str <- vroption_list$xg_str
  line_size <- fxoption_list$line_size; stroke_size <- fxoption_list$stroke_size; ptheme_l <- fxoption_list$ptheme_l
  mcolors <- fxoption_list$mcolors; line_styles <- fxoption_list$line_styles; par_unit <- fxoption_list$par_unit
  ens_spec <- fxoption_list$ens_spec; png_archive <- fxoption_list$png_archive; sdate <- fxoption_list$sdate;
  edate <- fxoption_list$edate; fw <- fxoption_list$fw; fh <- fxoption_list$fh
  param <- fxoption_list$param; num_models <- fxoption_list$num_models; point_size <- fxoption_list$point_size
  
  if (all(grepl("_scores",names(verif)))){
    df <- verif[[paste0(c_ftyp,"_",c_typ,"_scores")]]
  } else {
    df <- verif
  }
  
  # A check on the type of xgroup (in case of character type for lead_time and validhour)
  if ((xgroup %in% names(df)) & (is.character(df[[xgroup]])) & ((xgroup == "lead_time") || (xgroup == "valid_hour"))){
    df[[xgroup]] <- as.numeric(df[[xgroup]])
  }
  
  # Split score into its individual parts
  all_scores   <- strsplit(score,"~")[[1]]
  num_scores   <- length(all_scores)
  title_scores <- gsub("mbr","",all_scores) # Remove "mbr" from the title
  title_scores <- gsub("_"," ",str_to_title(title_scores)) # Remove underscore from score names
  
  # Plot title
  ptitle <- paste0(paste0(title_scores,collapse = ", ")," : ",title_str)
  
  # Change point size if we are looking at valid_dttm
  if (xgroup == "valid_dttm"){
    stroke_size = 0; 
    point_size = 0
  }
  
  # Plot according to number of scores (and handle some special scores for plot_point_verif)
  if (all(all_scores %in% ens_spec)){
    # Create the colour table for plot_point_verif
    models_tmp  <- names(mcolors)
    colours_tmp <- unname(mcolors)
    # Colour table
    ctab <- data.frame(fcst_model = models_tmp,colour = colours_tmp)
    colnames(ctab) <- c("fcst_model","colour")
    plot_point_verif(verif,{{all_scores}}, rank_is_relative = "TRUE", rank_hist_type = "lollipop", colour_by = "fcst_model",
                     colour_table = ctab, plot_title = ptitle, plot_subtitle = subtitle_str, legend_position = "top",
                     plot_caption = "none")
    # Save 
    png_fname <- fn_png_name(c_ftyp,all_scores,xg_str,param,cycle,sdate,edate,station,vlt,vth)
    ggsave(filename=png_fname,path=png_archive,width=fw,height=fh)
    p_out <- "NA"
    return(p_out)
  } else if (any(grepl("mbr",all_scores))){
    
    # If looking at valid_dttm, then use only one column
    if (xgroup == "valid_dttm"){
      ncols = 1
    } else {
      ncols = min(num_models,2)
    }
    
    # Plot all members and facet by model name
    c_score <- gsub("mbr","",all_scores)
    # If OBS exists as a fcst_model, then add this in
    df_ctrl <- df %>% filter(member == "mbr000")
    # This deals with the case of dailyvar/timeseries
    if ("OBS" %in% unique(df[["member"]])){
      df_obs <- df %>% filter(member == "OBS") 
      df_mean <- df %>% filter(member == "mean")
      df <- df %>% filter(!(member %in% c("mean","spread","OBS")))
      
      p_out <- ggplot()+
        geom_path(data=df,aes(x=get(xgroup),y=get(c_score),group=interaction(fcst_model,member)),
                  alpha=0.5,size=line_size,color="gray")+
        geom_path(data=df_ctrl,aes(x=get(xgroup),y=get(c_score),color="mbr000"),size=line_size)+
        geom_path(data=df_mean,aes(x=get(xgroup),y=get(c_score),color="Mean"),size=line_size)+
        geom_path(data=df_obs,aes(x=get(xgroup),y=get(c_score),color="OBS"),size=line_size)+
        facet_wrap(vars(fcst_model),ncol=ncols)+
        labs(x = str_to_title(xgroup),y = par_unit,title=ptitle,subtitle=subtitle_str,color="")+ptheme_l
      
    } else {
      p_out <- ggplot()+
          geom_path(data=df,aes(x=get(xgroup),y=get(c_score),group=interaction(fcst_model,member)),
                    alpha=0.5,size=line_size,color="gray")+
          geom_path(data=df_ctrl,aes(x=get(xgroup),y=get(c_score)),color="red",size=line_size)+
          facet_wrap(vars(fcst_model),ncol=ncols)+
          labs(x = str_to_title(xgroup),y = par_unit,title=ptitle,subtitle=subtitle_str)+ptheme_l
    }
      
    # Add zero line if relevant
    cp_ylim <- layer_scales(p_out)$y$range$range
    if ((cp_ylim[1]<0) && (cp_ylim[2]>0)){
      p_out <- p_out + geom_hline(yintercept=0,size=line_size,color="black",linetype="dashed")
    }
    return(p_out)
  } else {
    if (num_scores == 1){
      # Remove Inf/-Inf values which may appear for ens skill scores
      df <- df[!is.infinite(df[[all_scores]]),]
      p_out <- df %>% ggplot(aes(x=get(xgroup),color=fct_inorder(fcst_model)))+
        geom_line(aes(y=get(all_scores)),size=line_size)+
        geom_point(aes(y=get(all_scores)),stroke=stroke_size,size=point_size)
    } else if (num_scores == 2){
      # Remove Inf/-Inf values which may appear for ens skill scores
      df <- df[!is.infinite(df[[all_scores[1]]]),]
      df <- df[!is.infinite(df[[all_scores[2]]]),]
      p_out <- df %>% ggplot(aes(x=get(xgroup),color=fct_inorder(fcst_model)))+
        geom_line(aes(y=get(all_scores[1]),linetype=title_scores[1]),size=line_size)+
        geom_point(aes(y=get(all_scores[1])),stroke=stroke_size,size=point_size)+
        geom_line(aes(y=get(all_scores[2]),linetype=title_scores[2]),size=line_size)+
        geom_point(aes(y=get(all_scores[2])),stroke=stroke_size,size=point_size)
      #scale_linetype_manual(values=line_styles[1:2])
    } else {
      stop(paste0("Number of scores = ",num_scores," is not considered in plot_point"))
    }
    
    p_out <- p_out + 
      labs(x = str_to_title(xgroup),y = par_unit,color = "",linetype = "",
           title=ptitle,subtitle=subtitle_str)+ptheme_l+scale_color_manual(values=mcolors)
    
    cp_ylim <- layer_scales(p_out)$y$range$range
    if ((cp_ylim[1]<0) && (cp_ylim[2]>0)){
      p_out <- p_out + geom_hline(yintercept=0,size=line_size,color="black",linetype="dashed")
    }
    
    return(p_out)
  }
}
#================================================#

#================================================#
# HELPER FUNCTION FOR PLOTTING THE NUMBER OF CASES
#================================================#

fn_plot_numcases <- function(verif,fxoption_list,vroption_list){
  
  # Read options
  c_typ <- vroption_list$c_typ; c_ftyp <- vroption_list$c_ftyp; xgroup <- vroption_list$xgroup
  line_size <- fxoption_list$line_size; stroke_size <- fxoption_list$stroke_size; point_size = fxoption_list$point_size
  ptheme_nc <- fxoption_list$ptheme_nc; mcolors <- fxoption_list$mcolors
  
  if (all(grepl("_scores",names(verif)))){
    df <- verif[[paste0(c_ftyp,"_",c_typ,"_scores")]]
  } else {
    df <- verif
  }
  
  # A check on the type of xgroup (in case of character type for lead_time and validhour)
  if ((xgroup %in% names(df)) & (is.character(df[[xgroup]])) & ((xgroup == "lead_time") || (xgroup == "valid_hour"))){
    df[[xgroup]] <- as.numeric(df[[xgroup]])
  }
  
  if (c_typ == "summary"){
    ncy    <- "num_cases"
    ylabel <- "Num. cases"
  } else if (c_typ == "threshold"){
    if (c_ftyp == "det"){
      ncy  <- "num_cases_for_threshold_observed"
    } else if (c_ftyp == "ens"){
      ncy  <- "num_cases_observed"
    }
    ylabel <- "Num. cases obs."
  }
  p_out<- df %>% ggplot(aes(x=get(xgroup),color=fct_inorder(fcst_model)))+
    geom_line(aes(y=get(ncy)),size=line_size)+
    geom_point(aes(y=get(ncy)),stroke=stroke_size,size=point_size)+
    labs(x = " ",y = ylabel,color = " ",shape = " ")+ptheme_nc+
    scale_color_manual(values=mcolors)
  
  # Change axis transformation if applicable
  p_ylim <- layer_scales(p_out)$y$range$range
  if ((p_ylim[2] - p_ylim[1])>1000){
    c_breaks <- round(pracma::logseq(1,p_ylim[2],3),0)
    p_out <- p_out + scale_y_continuous(trans="pseudo_log",breaks=c_breaks)
  } else {
    p_out <- p_out + scale_y_continuous(breaks=scales::breaks_pretty(n=3))
  }
  return(p_out)
}
#================================================#
