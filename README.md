# accord-verif-scripts
This a standard set of scripts to carry out
a point-verification using [harp](https://github.com/harphub/harp).
The scripts were developed during the [ACCORD MQA working week in Helsinki](https://opensource.umr-cnrm.fr/projects/accord/wiki/MQAWW202206), with contributions from James Fannon, Daniel Yazgi, Florian Weidle, Carl Fortelius, Andrew Singleton and Carlos Peralta.

Currently, the scripts include only deterministic measures, based on what is provided in the [monitor verification package](https://hirlam.github.io/Monitor/dev/) and include: bias, rmse, stdev, equitable threat score, extreme dependency score, false-alarm rate, false-alarm ratio, frequency distribution, frequency bias, Kuiper skill score, as well as timeseries plot of daily variations versus validdate, validhour and scatter plots.

Contributions are welcome. Please see instructions on how to contribute below.

## How to contribute
- Add a directory with a descriptive name - you will make all of your changes in this directory
- Under your new directory\:
  - Make a README.md file briefly describing what is in the directory and the intended use case 
  - Make a directory called "R" and add your R scripts there, e.g. R/harpFun.R
  - Make a directory called "man" and add documenation for each script using markdown, e.g. man/harpFun.md
  - For other languages create directories:

  <br>
  
  |Directory|Language|
  |---|---|
  |py|Python|
  |c|C|
  |cpp|C++|
  |fortran|FORTRAN|
  |scr|shell scripts|
  
- Commit changes to your local repository and push to your fork on github
- From your fork on github, click on Contribute > Open Pull Request and follow the instructions

## Useful reading
[Happy Git With R](https://happygitwithr.com)

