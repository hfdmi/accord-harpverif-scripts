# Instructions for running a point verification

The instructions below describe how to carry out a deterministic
point verification using the ACCORD verification scripts.

## Modify the configuration files

All configuration parameters are defined in the `config` directory.
For a given configuration, there are 3 configuration files. 
`config_file.sh`
`config_file.R`
`config_file.yml`

### config_file.sh
The initial dates and options for running the verification are defined here,
as well as the options to do pre-processing, verification of visualization
```
#R Env
USING_RENV=${USING_RENV-no}


# What to run
RUN_POINT_VERF=${RUN_VERF-yes}
RUN_POINT_VERF_LOCAL=${RUN_POINT_VERF_LOCAL-yes}

SCORECARDS=${SCORECARDS-yes}
RUN_VOBS2SQL=${RUN_VOBS2SQL-yes}
RUN_VFLD2SQL=${RUN_VFLD2SQL-yes}

```

### config_file.yml

The main config details are defined in the yml file, including the start
and end dates. There is a section for each part of the process.
```
shared:
  start_date: 2022050100
  end_date: 2022051023

pre:
  fclen: 48
  vfld_path: "/home/nhd/R/harpUserScripts/data/sample_data"
  vobs_path: "/home/nhd/R/harpUserScripts/data/sample_data/vobs"
  vfld_template:  #include one for each model if they are different. 
    - "vfld"
  params:
    - S10m
    - T2m
    - RH2m
    - Pmsl
    - T

```


### config_file.R
This config file defines the list of parameters to be 
verified and set some thresholds. 
This file also reads the config file
```
library(yaml)

conf_get_config <- function(){
  CONFIG <- yaml.load_file(Sys.getenv('CONFIG_YAML') )
  CONFIG$params_details = conf_get_params_details()
  CONFIG
}
```

Two examples are included in the `config` directory: `config_example.{yml,R,sh}` and `config_atos.{yml,R,sh}`.

To run the verification for a particular configuration, one can
either call the master script pointing to the configuration files
from `scr`: `./scr/master_example.sh` or create temporary soft link
in the `accord-verif-scripts` directory
`ln -sf scr/master_local.sh main_local.sh`
There is an example link `main.sh` already included in this repository.
