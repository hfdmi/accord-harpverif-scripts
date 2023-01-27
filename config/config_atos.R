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
    T2m_thr <- c(-20, -10, seq(-5, 30, 5))
    Tmax_thr <- c(seq(0, 40, 5))
    Tmin_thr <- c(seq(-15, 25, 5))
    S10m_thr <- c(seq(0, 25, 5))
    RH2m_thr <- c(seq(0, 100, 20))
    Cbase_thr <- c(seq(0, 2000, 200))
    Pcp_thr <- c(0,1,5,10,15,20,30,40,60,80, seq(100, 100, 500))
    vis_thr    <- c(seq(0, 2000, 200))
    Pmsl_thr     <- c(seq(920, 1040, 20))
    Q_thr  <- c(seq(0, 0.15, 0.015))
    SD_thr <- c(0,1,3,6,10, seq(20, 80, 20))
    params <- list(
                AccPcp1h = list (
       thresholds = Pcp_thr
          ),
                  AccPcp3h = list (
        thresholds = Pcp_thr
          ),
                  AccPcp6h = list (
        thresholds = Pcp_thr
          ),
                  AccPcp12h = list (
       thresholds = Pcp_thr
          ),
                AccPcp24h = list (
      thresholds = Pcp_thr
          ),
                Pcp = list (
       thresholds = Pcp_thr
          ),

    		T2m = list(
        thresholds = T2m_thr,
        scale_fcst = list(scale_factor = -273.15, new_units = "degC"),
        scale_obs  = list(scale_factor = -273.15, new_units = "degC")       
    			),
   		Td2m = list(
        thresholds = T2m_thr,
        scale_fcst = list(scale_factor = -273.15, new_units = "degC"),
        scale_obs  = list(scale_factor = -273.15, new_units = "degC")       
    			),
    		Tmax = list(
        thresholds = Tmax_thr,
        scale_fcst = list(scale_factor = -273.15, new_units = "degC"),
       scale_obs  = list(scale_factor = -273.15, new_units = "degC")        
    			),    
    	        Tmin = list(
        thresholds = Tmin_thr,
        scale_fcst = list(scale_factor = -273.15, new_units = "degC"),
        scale_obs  = list(scale_factor = -273.15, new_units = "degC")        
    			),    
    		S10m = list(
        thresholds = S10m_thr      
    			),
    	        Gmax = list(
        thresholds = S10m_thr      
    			),    			
         	RH2m  = list (
        thresholds = RH2m_thr
         ),   
                Q2m  = list (
        thresholds = Q_thr
         ),
                  	CClow  = list (
        thresholds = RH2m_thr
         ),   
                  	CCtot  = list (
        thresholds = RH2m_thr
         ),   
                  	Cbase  = list (
        thresholds = Cbase_thr
         ),   
                        Ps  = list (
        thresholds = Pmsl_thr
         ),
                     Pmsl  = list (
        thresholds = Pmsl_thr 
          ),
                     vis  = list (
        thresholds = vis_thr
         ),   
                     DSN  = list (
        thresholds = SD_thr
         ),
    		T = list(
       scale_fcst = list(scale_factor = -273.15, new_units = "degC"),
        scale_obs  = list(scale_factor = -273.15, new_units = "degC"),
        vc         = "pressure"
                 
    			)    
    	      )
    		  
         params
  
}


