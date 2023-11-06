fn_check_obs_against_fcst <- function(
    .fcst, parameter, num_sd_allowed = NULL, stratification = c("SID", "quarter_day") ) {
  
  #parameter <- prm_name
  #.fcst <-  fcst
  #num_sd_allowed <- NULL
  #stratification = c("SID", "quarter_day")
  
  pt <- parameter
  parameter_quo  <- rlang::enquo(pt)
  parameter_expr <- rlang::quo_get_expr(parameter_quo)
  if (is.character(parameter_expr)) {
    parameter_quo <- rlang::sym(parameter_expr)
  }
  parameter_name <- rlang::quo_name(parameter_quo)
  
  #print(parameter)
  #print(pt)
  #print(parameter_quo)
  #print(parameter_expr)
  #print(parameter_name)
  
  if (is.null(num_sd_allowed)) {
    num_sd_allowed <- switch(
      parameter_name,
      "T2m"       = 6,
      "RH2m"      = 6,
      "Q2m"       = 6,
      "Pmsl"      = 6,
      "Ps"        = 6,
      "S10m"      = 6,
      "G10m"      = 6,
      "vis"       = 6,
      "Cbase"     = 6,
      "AccPcp1h"  = 8,
      "AccPcp3h"  = 8,
      "AccPcp6h"  = 8,
      "AccPcp12h" = 8,
      "AccPcp24h" = 8,
      0
    )
  }
  
  print(paste("Doing check against fcst for ", parameter_name, ". num_sd_allowed is: ", num_sd_allowed, sep=""))
  #print(prm_name)
  #print(parameter_name)
  
  if (num_sd_allowed > 0) {
    
    tolerance <- join_models(
      .fcst,
      by = Reduce(
        intersect,
        lapply(
          .fcst,
          function(x) {
            grep(
              "_mbr[[:digit:]]+$|_mbr[[:digit:]]+_lag[[:digit:]]*$|_det$",
              colnames(x),
              value = TRUE, invert = TRUE
            )
          }
        )
      )
    )[[1]]
    
    # Create extra columns for stratifying
    if (!is.element("valid_hour", colnames(tolerance))) {
      
      if (!is.element("validdate", colnames(tolerance))) {
        stop(".fcst must have `validdate` column.")
      }
      
      if (is.numeric(tolerance[["validdate"]])) {
        tolerance <- expand_date(tolerance, .data[["validdate"]])
      } else if (inherits(tolerance[["validdate"]], "POSIXct")) {
        tolerance <- dplyr::mutate(
          tolerance,
          valid_hour = lubridate::hour(.data[["validdate"]]),
          valid_month = lubridate::month(.data[["validdate"]])
        )
      } else {
        stop("Cannot convert validdate column to hour and months")
      }
      
    }
    
    if (is.element("quarter_day", stratification)) {
      tolerance <- dplyr::mutate(
        tolerance,
        quarter_day = cut(.data[["valid_hour"]], seq(0, 24, 6), right = FALSE)
      )
    }
    
    tolerance_df <- tidyr::pivot_longer(
      tolerance,
      tidyselect::matches("_mbr[[:digit:]]+$|_det$")
    ) %>%
      dplyr::group_by(!!!rlang::syms(stratification)) %>%
      dplyr::summarise(
        tolerance_allowed = stats::sd(.data[["value"]]) * .env[["num_sd_allowed"]],
        num_cases         = dplyr::n()
      )
    
    tolerance <- suppressWarnings(suppressMessages(join_to_fcst(
      tolerance,
      tolerance_df,
      by = stratification,
      force_join = TRUE
    )))
    
    tolerance <- dplyr::mutate(
      tolerance,
      dplyr::across(
        dplyr::matches("_mbr[[:digit:]]+$|_det$"),
        ~abs(. - !!parameter_quo)
      )
    )
    
    tolerance <- dplyr::mutate(
      tolerance,
      min_diff = matrixStats::rowMins(
        as.matrix(
          dplyr::select(tolerance, dplyr::matches("_mbr[[:digit:]]+$|_det$"))
        )
      )
    )
    
    # tolerance <- tolerance %>%
    #   dplyr::mutate(
    #     min_diff = matrixStats::rowMins(
    #       as.matrix(
    #         dplyr::select(tolerance, dplyr::contains("_mbr"))
    #       )
    #     )
    #   )
    
    good_obs <- tolerance %>%
      dplyr::filter(.data$min_diff <= .data$tolerance_allowed) %>%
      dplyr::select(.data$SID, .data$validdate, !! parameter_quo) %>%
      dplyr::group_by(.data$SID, .data$validdate) %>%
      dplyr::summarise(!! rlang::sym(parameter_name) := unique(!! parameter_quo)) %>%
      dplyr::ungroup()
    
    bad_obs  <- tolerance %>%
      dplyr::filter(.data$min_diff > .data$tolerance_allowed) %>%
      dplyr::select(.data$SID, .data$validdate, !! parameter_quo) %>%
      dplyr::group_by(.data$SID, .data$validdate) %>%
      dplyr::summarise(!! rlang::sym(parameter_name) := unique(!! parameter_quo)) %>%
      dplyr::ungroup()
    
    .fcst <- suppressWarnings(suppressMessages(join_to_fcst(
      .fcst,
      dplyr::select(
        good_obs, .data$SID, .data$validdate
      ),
      by = c("SID", "validdate"),
      force_join = TRUE
    )))
    
    attr(.fcst, "removed_cases") <- bad_obs
    
  } else {
    
    attr(.fcst, "removed_cases") <- NULL
    
  }
  
  .fcst
  
}
