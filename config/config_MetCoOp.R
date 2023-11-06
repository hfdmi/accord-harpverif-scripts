# Setting up the parameters and their scaling and thresholds here
# Adding this part not so easy using the yml file
# It is possible to create several nested sections like
# variable:
#   - name
#   - threshold
# etc
# but this make look a bit confusing. Hardcoding this part here


# List of parameters
# Add more parameters below using the same format. For upper air parameters,
# don't forget the vc = "pressure"

library(yaml)

conf_get_config <- function(){  
  CONFIG <- yaml.load_file(Sys.getenv('CONFIG_YAML') )
  CONFIG$params_details = conf_get_params_details()
  CONFIG
}


conf_get_params_details <- function(){
thresholds <- list(
 
  "Pmsl"=c(960,970,980,990,1000,1010,1020),
  "Ps"  =c(960,970,980,990,1000,1010,1020),

  "T2m"=c(-20,-10,-5,0,5,10,15,20,25,30),
  "Td2m"=c(-20,-10,-5,0,5,10,15,20,25,30),
  "Tmin"=c(-20,-10,-5,0,5,10,15,20,25,30),
  "Tmax"=c(-20,-10,-5,0,5,10,15,20,25,30),

  "Q2m"=c(2,4,6,8,10,12),
  "RH2m"=c(50,75,90,95),

  "S10m"=c(5,10,15,20,25,30,35),
  "Gmax"=c(5,10,15,20,25,30,35),


  "T"=c(-30,-20,-10,-5,0,5,10,15,20,25,30),
  "Td"=c(-30,-20,-10,-5,0,5,10,15,20,25,30),
  "S"=c(5,10,15,20,25,30,35,50),
  "RH"=c(30,40,50,60,70,80,90),
  "Q"=c(2,4,6,8,10,12),
  "Z"=c(0,1000,5000,10000,20000),
  "D"=c(0,90,180,270),

 "AccPcp1h"=c(0.1,0.3,0.5,1,2,4,7,10,15,20),
 "AccPcp3h"=c(0.1,0.3,0.5,1,2,4,7,10,15,20),
 "AccPcp6h"=c(0.1,0.3,0.5,1,2,4,7,10,15,20),
 "AccPcp12h"=c(0.1,0.3,0.5,1,2,4,7,10,15,20),

 "CCtot"=c(0,1,2,3,4,5,6,7,8),
 "CClow"=c(0,1,2,3,4,5,6,7,8),
 "Cbase"=c(seq(-7400, 0, 200)), #c(25,50,75,100,200,500,1000,2000,3000,4000,5000),

 "vis"=c(seq(-10000, 0, 500)), #c(100,1000,5000,10000,20000,40000,50000),
 
 "DSN"=c(3,10,30,75,100,300)#c(100,1000,5000,10000,20000,40000,50000)

)

    params_all <- list(
        Pmsl  = list (
        	    thresholds = thresholds$Pmsl,
				      num_sd_allowed = 5
          			),
        Ps  = list (
        			thresholds = thresholds$Ps,
        			num_sd_allowed = 5 
          			),
    		T2m = list(
        			thresholds = thresholds$T2m,
	   			    num_sd_allowed = 5,
        			scale_fcst = list(scale_factor = -273.15, new_units = "degC"),
        			scale_obs  = list(scale_factor = -273.15, new_units = "degC")       
    				),
    		Tmax = list(
        			thresholds = thresholds$Tmax,
	   			    num_sd_allowed = 5,
        			scale_fcst = list(scale_factor = -273.15, new_units = "degC"),
        			scale_obs  = list(scale_factor = -273.15, new_units = "degC")       
    				),
    		Tmin = list(
        			thresholds = thresholds$T2min,
	   			    num_sd_allowed = 5,
        			scale_fcst = list(scale_factor = -273.15, new_units = "degC"),
        			scale_obs  = list(scale_factor = -273.15, new_units = "degC")       
    				),
    		Td2m = list(
        			thresholds = thresholds$Td2m,
	   			    num_sd_allowed = 5,
        			scale_fcst = list(scale_factor = -273.15, new_units = "degC"),
        			scale_obs  = list(scale_factor = -273.15, new_units = "degC")       
    				),
    		Q2m = list(
        			thresholds = thresholds$Q2m,
	   			    num_sd_allowed = 5,
      				scale_fcst = list(scale_factor = 1e3, new_units = "gkg-1", multiplicative=TRUE),
      				scale_obs  = list(scale_factor = 1e3, new_units = "gkg-1", multiplicative=TRUE)
    				),
    		RH2m = list(
        			thresholds = thresholds$RH2m,
	   			    num_sd_allowed = 5,
	   			    obsmax_val = 105
    				),
    		S10m = list(
        			thresholds = thresholds$S10m,
	   			    num_sd_allowed = 5
    				),
    		Gmax = list(
        			thresholds = thresholds$Gmax,
	   			    num_sd_allowed = 5
    				),
    		CCtot = list(
        			thresholds = thresholds$CCtot,
	   			    num_sd_allowed = 5
    				),
    		CClow = list(
        			thresholds = thresholds$CClow,
	   			    num_sd_allowed = 5
    				),
    		Cbase = list(
        			thresholds = thresholds$Cbase,
        			scale_fcst = list(scale_factor = -1, new_units = "-m", multiplicative=TRUE),
        			scale_obs = list(scale_factor = -1, new_units = "-m", multiplicative=TRUE),
	   			    num_sd_allowed = 5
    				),
    		vis = list(
        			thresholds = thresholds$vis,
        			scale_fcst = list(scale_factor = -1, new_units = "-m", multiplicative=TRUE),
        			scale_obs = list(scale_factor = -1, new_units = "-m", multiplicative=TRUE),
	   			    num_sd_allowed = 5
    				),
    		DSN = list(
        			thresholds = thresholds$DSN,
	   			    num_sd_allowed = 5
    				),
                AccPcp1h = list (
       				thresholds = thresholds$AccPcp1h,
				      num_sd_allowed = 10
          			),
        AccPcp3h = list (
        			thresholds = thresholds$AccPcp3h,
				      num_sd_allowed = 10
          			),
        AccPcp6h = list (
        			thresholds = thresholds$AccPcp6h,
				      num_sd_allowed = 10
          			),
        AccPcp12h = list (
        			thresholds = thresholds$AccPcp12h,
				      num_sd_allowed = 10
          			),
        AccPcp24h = list (
        			thresholds = thresholds$AccPcp24h,
				      num_sd_allowed = 10
          			),
    		DSN = list(
        			thresholds = thresholds$DSN,
	   			    num_sd_allowed = 5
    				),
    		T = list(
        			thresholds = thresholds$T,
	   			    num_sd_allowed = 5,
        			scale_fcst = list(scale_factor = -273.15, new_units = "degC"),
        			scale_obs  = list(scale_factor = -273.15, new_units = "degC"),       
      				vc         = "pressure"
    				),
    		Td = list(
        			thresholds = thresholds$Td,
	   			    num_sd_allowed = 5,
        			scale_obs  = list(scale_factor = -273.15, new_units = "degC"),       
        			scale_obs  = list(scale_factor = -273.15, new_units = "degC"),       
      				vc         = "pressure"
    				),
    		RH = list(
        			thresholds = thresholds$RH,
	   			    num_sd_allowed = 5,
      				vc         = "pressure"
    				),
    		Q = list(
        			thresholds = thresholds$Q,
	   			    num_sd_allowed = 5,
      				scale_fcst = list(scale_factor = 1e3, new_units = "gkg-1", multiplicative=TRUE),
      				scale_obs  = list(scale_factor = 1e3, new_units = "gkg-1", multiplicative=TRUE),
      				vc         = "pressure"
    				),
    		S = list(
        			thresholds = thresholds$S,
	   			    num_sd_allowed = 5,
      				vc         = "pressure"
    				),
    		D = list(
        			thresholds = thresholds$D,
	   			    num_sd_allowed = 5,
      				vc         = "pressure"
    				),
    		Z = list(
        			thresholds = thresholds$Z,
	   			    num_sd_allowed = 5,
      				vc         = "pressure"
    				)
    )
    		  
    	#params_all[names(params_all) %in% c( "Ps", "Pmsl", "T2m",  "Q2m", "RH2m",  "S10m", 
        #                           "CCtot",  "CClow",  "Cbase",  "vis",
    	#			    "AccPcp6h", "AccPcp12h", "AccPcp24h", "T", "Q", "S", "Z")]
    	#params_all[names(params_all) %in% c( "Gmax")]
    	#params_all[names(params_all) %in% c( "AccPcp3h", "AccPcp12h"  )]
    	#params_all[names(params_all) %in% c( "Q", "S", "Z" )]

# Exclude the following parameters
    	#params_all[!(names(params_all) %in% c( "AccPcp1h", "AccPcp3h", "Tmax", "Tmin", "Td2m", "Gmax", "DSN", "D", "Td"  ))]
    	params_all[!(names(params_all) %in% c( "AccPcp1h", "AccPcp3h", "Tmax", "Tmin", "Td2m", "Gmax", "DSN", "D", "RH", "Td"  ))]
}

