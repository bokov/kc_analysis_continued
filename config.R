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
  dat01 = 'data/kcsample.csv'
  # the working copy of the data dictionary
  ,map0 = 'data/DF_kc_v5_dbb4a700_dict.csv'
  # manual mappings intended to persist from one refresh to the next
  ,map1 = 'data/persistent_dict.tsv'
  # a table of derived variables which will get created later
  ,map2 = 'data/derived_var_dictionary.tsv'
  # mappings of ICD10 codes to patient safety indicators
  # from Southern et al., Medical Care 2017 doi: 10.1097/MLR.0000000000000649
  ,psi = 'data/southern_icd10patientsafetycodes_medicalcare17.csv'
);

#### project options ####
#' In this project's data dictionaries, the default column name is stored in
#' `colname`
projectoptions <- list(tb.retcol='colname');
project_seed <- '20200929';

#### footer ####
c()


