#' ---
#' title: "Export abbreviated tables"
#' author:
#' - "Alex F. Bokov^[UT Health, Department of Epidemiology and Biostatistics]"
#+ load_deps, echo=FALSE, message=FALSE, warning=FALSE,results='hide'

#' The purpose of this script is to export abbreviated tables that are somewhat
#' easier, hopefully to understand than the raw tables actually used for 
#' analysis. This is done by removing currently unused columns and 
#' informational columns, which may be needed to extract future variables.
#' 
# Init ----
#
# In the below two lines are the minimum script-level settings you need.
# The `.projpackages` object has the names of the packages you need installed
# if necessary and then loaded for this scriport. The `.deps` object contains
# other scriports on which this one _directly_ depends (you don't need to worry
# about the indirect ones-- each scriport manages its own dependencies for
# packages and scriports). The recommended value to start with is the one shown
# here. You can add more if the need arises later. For more information, please
# see the [overview](overview.html) scriport.
.projpackages <- c('GGally','pander','dplyr','ggplot2','data.table'
                   ,'survival','broom','forcats','table1');
.deps <- c( '' );
.debug <- 0;
.junk<-capture.output(source('./scripts/global.R',chdir=TRUE,echo=FALSE
                             ,local=TRUE));
# Settings ----
# Set some formatting options for this document

.optionalcols <- c('static','units_cd','mxinsts','nval_num','patvis_null'
                   ,'confidence_num','mxconmod','pats_null','colid'
                   ,'quantity_num','cid','concept_path','rule','colcd'
                   ,'location_cd','valtype_cd','varname','chartname','col_url');

pander::panderOptions('table.alignment.default','right');
pander::panderOptions('table.alignment.rownames','right');
pander::panderOptions('table.split.table',Inf);
pander::panderOptions('p.wrap','');
pander::panderOptions('p.copula',', and ');
theme_set(theme_bw(base_family = 'serif',base_size=14) +
            theme(strip.background = element_rect(fill=NA,color=NA)
                  ,strip.text = element_text(size=15)));
knitr::opts_chunk$set(echo=.debug>0, warning=.debug>0, message=.debug>0);


.currentscript <- current_scriptname('export_tables.R');

# data ----
.tempenv <- new.env();
.loadedobjects <- load_deps2('data.R',cachedir = .workdir,debug=.debug
                             ,envir = .tempenv);
for(ii in c('dat01','dat01devel','dat01test')){
  dat01 <- copy(.tempenv[[ii]]);
  dct0 <- sync_dictionary(dat01,.tempenv$dct0);
  dat01[,unique(c(v(c_info,dat01)
                  ,v(c_coderange,dat01)
                  ,v(c_coderange_info,dat01))) := NULL];
  .emptycols <- sapply(dat01,function(xx) length(unique(xx))==1);
  dat01[,names(.emptycols)[.emptycols] := NULL];
  .outfile <- export(dat01,tempfile(),format='tsv');
  .outfinalfile <- with(tidbits:::git_status(print=FALSE)
                        ,paste0(gsub('dat01','_',ii),'_',branch,'_',hash
                                ,'_shortened.tsv')) %>%
    c('\\.[^.]*$',.) %>% rbind(c('^[0-9]{11,13}_',''),c('__','_')) %>% 
    submulti(basename(inputdata['dat01']),.);
  file.rename(.outfile,.outfinalfile);
  message(.outfinalfile);
  if(ii=='dat01'){
    dct0 <- select(dct0,-intersect(names(dct0),.optionalcols));
    dct0 <- sync_dictionary(dat01,dct0,reduce=2
                            ,filename=gsub('.tsv$','_dictionary.tsv'
                                           ,.outfinalfile))};
}

