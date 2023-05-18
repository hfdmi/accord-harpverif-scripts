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
    T2m_thr <- c(-20, -10, seq(-5, 25, 5))
    S10m_thr <- seq(0, 25, 5)
    RH2m_thr <- seq(0, 100, 20)
    
    params <- list(
    		T2m = list(
        thresholds = T2m_thr,
        scale_fcst = list(scale_factor = -273.15, new_units = "degC"),
        scale_obs  = list(scale_factor = -273.15, new_units = "degC")
      
    			),
    
    		S10m = list(
        thresholds = S10m_thr
      
    			),
         RH2m  =
         list (
        thresholds = RH2m_thr
         ),
    
    		T = list(
        scale_fcst = list(scale_factor = -273.15, new_units = "degC"),
        scale_obs  = list(scale_factor = -273.15, new_units = "degC"),
        vc         = "pressure"
      
    			)
    
    
    	      )
    		  
    params
  
}

