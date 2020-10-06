#' ---
#' title: "Generic R Project Configuration File"
#' author: "Alex F. Bokov, Ph.D."
#' date: "10/18/2018"
#' ---
#' 

#### inputdata ####
#' 
#' The inputdata variable determines which data files will get read into your
#' project. The values are the file locations and the names are the variables
#' to which they will be assigned after they are read into R
#' 
#' In the `config.R` file there should only be simulations of your actual data
#' or datasets that you are _certain_ you have permission to redistribute 
#' publicly.
#' 
#' If there is also a `local.config.R` file, that one will override `config.R`
#' and that one can contain paths to actual data, presumably on each 
#' collaborator's local computer.
#' 
#' WARNING: currently there is no valid simulated data for this project and
#' therefore you have to have a copy of the real data locally and a 
#' `local.config.R` file that overrides the below `inputdata` variable.
inputdata <- c(
  # DOESN'T EXIST
  dat01 = 'data/kcsample.zip'
  # the file within the zip that we use
  ,dat02 = 'DF_kc_v5_dbb4a700.csv'
  # the pre-processed data file (not part of repository, must be already 
  # in your posession
  ,dat03 = '2010061114_e3c84_DF_kc_v5_dbb4a700_dev.tsv'
  # the working copy of the data dictionary
  ,map0 = 'data/DF_kc_v5_dbb4a700_dict.csv'
  # manual mappings intended to persist from one refresh to the next
  ,map1 = 'data/persistent_dict.tsv'
  # a table of derived variables which will get created later
  ,map2 = 'data/derived_var_dictionary.tsv'
  # mappings from previous attempt
  ,map3 = 'data/datadictionary_static.csv'
  # mappings of ICD10 codes to patient safety indicators
  # from Southern et al., Medical Care 2017 doi: 10.1097/MLR.0000000000000649
  ,psi = 'data/southern_icd10patientsafetycodes_medicalcare17.csv'
  # factor levels
  ,levels_map = 'data/levels_map.tsv'
);

#### project options ####
#' In this project's data dictionaries, the default column name is stored in
#' `colname`
projectoptions <- list(tb.retcol='colname');
project_seed <- '20200929';

#### footer ####
c()


