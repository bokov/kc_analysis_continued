#' ---
#' title: "Read in Data"
#' author:
#' - "Alex F. Bokov^[UT Health, Department of Epidemiology and Biostatistics]"
#' date: "09/14/2018"
#' ---
#'
#+ message=F,echo=F
# init ----
debug <- 0;
# note: the `icdcoder` library might be needed in the future, and it has to be
# manually downloaded and installed from https://github.com/wtcooper/icdcoder/
# (package hasn't been updated in 5 years, but only existing way to map ICD9 to
# ICD10
.projpackages <- c('dplyr','data.table','forcats','pander','icd'
                   ,'TraMineRextras','cluster');
.globalpath <- c(list.files(patt='^global.R$',full=T)
                 ,list.files(path='scripts',patt='^global.R$',full=T)
                 ,list.files(rec=T,pattern='^global.R$',full=T)
                 ,list.files(path='..',patt='^global.R$',full=T)
                 ,list.files(path='..',rec=T,pattern='^global.R$',full=T))[1];
if(debug>0) source(.globalpath,chdir = TRUE, local=TRUE) else {
  .junk<-capture.output(source(.globalpath,chdir=TRUE, echo=FALSE, local=TRUE))};
#.currentscript <- parent.frame(2)$ofile;
.currentscript <- current_scriptname('data.R');
#' Saving original file-list so we don't keep exporting functions and
#' environment variables to other scripts
.origfiles <- ls();
#' regexps
.re_neph <- "CPT:50240|CPT:1014157|CPT:50220|CPT:50225|CPT:50230|CPT:1008091|CPT:50234|CPT:50236|CPT:50548|CPT:50546|CPT:50545|CPT:50543|CPT:50593|CPT:50592";
#+ echo=FALSE,message=FALSE

# read data dictionaries ----
#' Reading data dictionary
#'
#' If `varmap.csv` doesn't exist, run `dictionary.R`
.srcenv <- new.env();
if(!file.exists('varmap.csv')) source('dictionary.R',local=.srcenv);
#' Then load `varmap.csv`
if(debug) message('Importing varmap.csv');
dct0 <- import('varmap.csv');
#' load levels map
levels_map <- import(inputdata['levels_map']);
# read data ----
#' # Read data
if(debug) message('About to read');
# filtering out patients with two or fewer visits with `if(.N>2)0` and
set.seed(project_seed);
dat01 <- unzip(inputdata['dat01']) %>% basename %>% 
  intersect(basename(inputdata['dat02'])) %>% fread;
if(debug) dat01.00 <- copy(dat01);

dat01 <- dat01[age_at_visit_days >= 18*362.25,][
  ,if(.N>2) .SD, by=patient_num][
    # z_ixvis was originally a randomly selected index visit for each patient
    # but for now we want all visits, yet cannot simply remove z_ixvis because
    # other stuff depends on it so instead we set it to the first observed visit
    ,z_ixvis:=head(age_at_visit_days,1),by=patient_num][
      ,a_t1:=age_at_visit_days-z_ixvis,by=patient_num][
        ,a_t0:=shift(a_t1),by=patient_num][
          # converting `start_date` to a proper date column for subsequent join
          ,start_date := as.Date(start_date)];
if(basename(inputdata['dat02'])=='DF_kc_v5_dbb4a700.csv'){
  dat01[,c('v002_Plvs_ptnts_cd', 'v002_Plvs_ptnts') := NULL]
}

# transform data ----
#' # Transform data
#'
#' ## assign random subsets
if(debug) message('About to subsample');
set.seed(project_seed);
.sample <- dat01[,.(subsample=sample(c('devel','test'),1)),by=patient_num];
dat01[.sample,on = 'patient_num',z_subsample:=subsample];

# aggregate ----
#' 
#' Indicator variable for whether or not a visit is directly included in NAACCR
dat01$a_n_visit <- apply(dat01[,v(c_naaccr,dat01),with=FALSE],1
                         ,function(xx) any(!is.na(xx)));

#' ## Aggregate the outcomes indicators

#' ## Missingness
#'
#' ### Any diagnoses at all during visit?
dat01$a_anydiagtf <- c(v(c_icd10),v(c_icd9)) %>% paste0('_tf') %>% 
  intersect(names(dat01)) %>% `[`(dat01,,.SD,.SDcols=.) %>% apply(1,any);

#' ### Any procedures at all during visit?
dat01$a_anyproctf <- c(v(c_icd10pcs),v(c_cpt)) %>% paste0('_tf') %>% 
  intersect(names(dat01)) %>% `[`(dat01,,.SD,.SDcols=.) %>% apply(1,any);

#' ### Any labs at all during visit?
dat01$a_anylabstf <- v(c_loinc) %>% paste0('_mn') %>% 
  intersect(names(dat01)) %>% `[`(dat01,,.SD,.SDcols=.) %>% apply(1,function(xx) any(!is.na(xx)));

dat01$a_anythingtf <- with(dat01,a_anydiagtf|a_anyproctf|a_anylabstf);

# remove unused ----
#' ## Remove unused columns
#'
#' Blow away the info-only columns, labs, and PSI components except those named
#' in c_override_keep
# if(debug) message('Removing columns');
# dat01[,setdiff(c(v(c_info),v(c_loinc),v(c_psi)),v(c_override_keep)) := NULL];

#' ## Rename columns
#'
#' Rename columns to more easily recognizable names along with dct0 entries
#+ rename_columns
setnames(dat01,function(xx){
  submulti(xx,dct0[,c('colname','rename')] %>% na.omit,method='startsends')
  },skip_absent = TRUE);
# names(dat01) <- dct0[,c('colname','rename')] %>% na.omit %>%
#   # catch the case where there is no 'rename' column
#   {if(nrow(.)==0) names(dat01) else {
#     submulti(names(dat01),.,method='startsends')}};

#' Update the dictionary to match renamed columns
if(debug) message('Syncing dictionary');
dct0 <- sync_dictionary(dat01);

if(debug) dat01.01 <- copy(dat01);

#' Mass relabel/reorder factor variables.
for(ii in v(c_sortlabels)){
  #if(ii == 'n_rectype') browser();
  dat01[[ii]] <- factorclean(dat01[[ii]]
                             ,remove=c('NAACCR\\|[0-9]{3,4}:',';00','00;',';0$'
                                       ,'^0')
                             ,otherfun = function(xx) gsub(';',',',xx)
                             ,spec_mapper = levels_map,var=ii,droplevels = T)};
for(ii in setdiff(v(c_naaccr_race),v(c_info))){
  dat01[[ii]] <- factorclean(dat01[[ii]]
                             ,remove=c('NAACCR\\|[0-9]{3,4}:',';00','00;',';0$'
                                       ,'^0')
                             ,otherfun = function(xx) gsub(';',',',xx)
                             ,spec_mapper = levels_map,var='_rc'
                             ,droplevels = T)};
#' Convert NAACCR race codes
dat01[
  ,a_n_race := interaction(.SD),.SDcols = setdiff(v(c_naaccr_race),v(c_info))];
levels(dat01$a_n_race) <- gsub('\\.(88|Unknown)','',levels(dat01$a_n_race));

#' Simplified recurrence type
#' RECURRENCE VARIABLE
dat01$a_n_recur <- factor(dat01$n_rectype);
levels(dat01$a_n_recur)[!levels(dat01$a_n_recur) %in% 
                          c('Unknown if recurred or was ever gone'
                            ,'Never disease-free','Disease-free'
                            ,grep('Ambig_',levels(dat01$a_n_recur)
                                  ,val=T))]<-'Recurred';
#' Fix any natf values (but can't use c_natf until it's updated from previous 
#' definitions)
#' 
for(ii in v(c_kcfact)) if(!is.logical(dat01[[ii]])){
  dat01[[ii]] <- !is.na(dat01[[ii]])};

#' indicator variable for nephrectomy
dat01$a_e_neph_tf <- grepl(.re_neph,dat01$z_e_surg);

#' Unified NAACCR diabetes comorbidity
#' 
#' TODO: verify that this is catching diabetes correctly
dat01[,a_n_dm := apply(.SD,1,function(xx) any(grepl('250[0-9]{2}$',xx)))
      ,.SDcols = setdiff(v(c_naaccr_comorb),v(c_info))];



#' Find the patients which had kidney cancer according to EMR and to NAACCR
# kcpatients.emr <- subset(dat01,e_kc_i10|e_kc_i9)$patient_num %>% unique;
# the above is equivalent to the below, but less efficient
# state variables ----
dat01[
  # kc diagnoses, emr and naaccr
  ,`:=`(z_haskc_emr_state=.I>match(T,e_kc_i10,nomatch=1e6)+min(.I)-1
        ,z_haskc_naaccr_state=.I>match(T,n_ddiag,nomatch=1e6)+min(.I)-1
        # surgery, emr and naaccr
        ,z_surg_emr_state=.I>match(T,a_e_neph_tf,nomatch=1e6)+min(.I)-1
        ,z_surg_naaccr_state=.I>match(T,n_dsurg|n_rx3170,nomatch=1e6)+min(.I)-1)
  ,by=patient_num][
    ,`:=`(z_haskc_emr=any(z_haskc_emr_state)
          ,z_haskc_naaccr=any(z_haskc_naaccr_state))
    ,by=patient_num];
## not using n_seer_kcancer for now because that is kidney _or_ renal pelvis
#dat01[,z_haskc_naaccr := any(n_kcancer,na.rm=TRUE),by=patient_num];

# sequence exploration (temporary home) ----
#' ## sequence exploration
#' 

dat01tse <- rbind(
  dat01[e_kc_i10==TRUE,.SD[1],by=patient_num,.SDcols='age_at_visit_days'][
    ,event:='DiagEMR']
  ,dat01[n_ddiag==TRUE,.SD[1],by=patient_num,.SDcols='age_at_visit_days'][
    ,event:='DiagNAACCR']
  ,dat01[a_e_neph_tf==TRUE,.SD,by=patient_num,.SDcols='age_at_visit_days'][
    ,event:='SurgEMR']
  ,dat01[n_dsurg|n_rx3170,.SD,by=patient_num,.SDcols='age_at_visit_days'][
    ,event:='SurgNAACCR']
  ,dat01[(n_dsurg|n_rx3170|a_e_neph_tf)&z_surg_emr_state&z_surg_naaccr_state
         ,.SD[.N],by=patient_num,.SDcols='age_at_visit_days'][
           ,event:='PostSurg']
  ,dat01[z_haskc_emr_state&z_haskc_naaccr_state,.SD[1],by=patient_num
         ,.SDcols='age_at_visit_days'][
           ,event:='PostDiag']
  ,dat01[,.SD[.N],by=patient_num,.SDcols='age_at_visit_days'][
    ,`:=`(event='END')])[
             ,time:=age_at_visit_days/7][,time:=time-min(time),by=patient_num][
               order(patient_num,time)];
# dat01tse <- rbind(dat01tse,dat01tse[,tail(.SD,1),by=patient_num][
#   # add final event one unit of time later to make sure it doesn't disappear
#   # during conversion to sts
#   ,`:=`(time=time+1,event='END')])[order(patient_num,time)];
dat01tse <- rbind(dat01tse,dat01tse[event %in% c('SurgEMR','SurgNAACCR')
                                          ,.SD[.N],by=c('patient_num','time')][
                                            ,`:=`(time=time+1,event='SURGDONE')])[
                                              order(patient_num,time)];

# test subset, just patients who have diagnoses and surgeries in both data 
# sources
# dat01tse00 <- dat01tse[,`:=`(
#   allevents=length(intersect(event,c('DiagEMR','DiagNAACCR','SurgEMR'
#                                        ,'SurgNAACCR'))) == 4
#   ,multisurg=sum(event %in% c('SurgEMR','SurgNAACCR'))>2),by=patient_num][
#     allevents & multisurg]#[!event %in% c('PostDiag','PostSurg'),1:4];
# # just diags
# dat01tse01 <- dat01tse00[!event %in% c('SurgEMR','SurgNAACCR')];
# 
# dat01tse02 <- rbind(dat01tse00,dat01tse00[event %in% c('SurgEMR','SurgNAACCR')
#                                       ,.SD[.N],by=c('patient_num','time')][
#                                         ,`:=`(time=time+1,event='SURGDONE')])[
#                                           order(patient_num,time)];


stm <-seqe2stm(#unique(dat01tse00$event)
  c('DiagEMR','DiagNAACCR','PostDiag','SurgEMR','SurgNAACCR','PostSurg'
    ,'SURGDONE','END')
  ,dropList=list('END'=c('DiagEMR','DiagNAACCR','PostDiag'
                         ,'SurgEMR','SurgNAACCR','SURGDONE','PostSurg')
                 ,'PostDiag'=c('DiagNAACCR','DiagEMR')
                 ,'SURGDONE'=c('SurgEMR','SurgNAACCR','SURGDONE')
                 ,'PostSurg'='SURGDONE'
               ));
stm <- gsub('.*END.*','END',stm) %>% 
  gsub('(Diag(EMR|NAACCR)\\.){1,2}(PostDiag.*)','\\3',.) %>% 
  gsub('DiagEMR\\.DiagNAACCR(\\.{0,1})','PostDiag\\1',.) %>% 
  gsub('(.*)(\\.{0,1}Surg(EMR|NAACCR)){1,2}\\.PostSurg','\\1PostSurg',.) %>%
  gsub('(.*)(\\.{0,1}Surg(EMR|NAACCR)){1,2}\\.PostSurg','\\1PostSurg',.) %>% 
  gsub('\\.(Surg(EMR|NAACCR)\\.){0,2}SURGDONE','',.);
  #gsub('SurgEMR\\.SurgNAACCR(\\.{0,1})','\\2PostSurg',.);

#stm[stm=='END'] <- NA;

dat01sts <- TSE_to_STS(dat01tse,id='patient_num',timestamp='time',stm=stm
                        ,tmax=1000,event='event');
dat01sts[] <- lapply(dat01sts,.%>% ifelse(.=='END',NA,.) %>%
                       gsub('.*SurgEMR\\.SurgNAACCR.*','XXX',.) %>% 
                       gsub('.*(Surg(EMR|NAACCR)).*','\\1',.) %>% 
                       gsub('XXX','SurgEMR.SurgNAACCR',.));
# getting rid of empty records
dat01sts00 <- dat01sts[rowSums(is.na(dat01sts))<ncol(dat01sts)-2,];

dat01seq <- seqdef(dat01sts00);

# similarity ----
#' Clustering of similare patient histories. Right now the clustering sucks
#' but at least I figured out how to get it to work. Need to choose the right
#' distance metric and parameters.

# transition costs
# dat01costs <- seqcost(dat01seq, method = "INDELSLOG",with.missing = F);
# distance
# dat01dist <- seqdist(dat01seq,method='OMspell',indel=max(dat01costs$indel)
#                      ,sm=dat01costs$sm);
dat01dist <- seqdist(dat01seq,method='OM',indel=1,sm="TRATE");

# #' convert it to a distance matrix and cluster that. `dat01hc$order` can be
# #' passed to the `sortv` argument of `seqplot()`.
# dat01hc <- hclust(as.dist(dat01dist));
dat01cl <- agnes(dat01dist00,diss=T,method='ward');
#' The following can also be passed to the `sortv` argument
dat01clk4 <- cutree(dat01cl,k=4);

#' Alternatively, can group by which main events represented
.prepdat01grp <- data.table(sapply(
  c('DiagEMR','SurgEMR','DiagNAACCR','SurgNAACCR','PostDiag','PostSurg')
  ,function(xx) apply(dat01seq,1,function(yy) any(grepl(xx,yy))) ))[
    ,`:=`(DiagEMR=DiagEMR|PostDiag,SurgEMR=SurgEMR|PostSurg
          ,DiagNAACCR=DiagNAACCR|PostDiag,SurgNAACCR=SurgNAACCR|PostSurg)];
dat01grp <- lapply(colnames(.prepdat01grp)[1:4],function(xx){
  ifelse(.prepdat01grp[[xx]]>0,xx,'')}) %>% interaction(drop=T);
levels(dat01grp) <- gsub('\\.{2,}','.',levels(dat01grp)) %>% 
  gsub('^\\.|\\.$','',.);

#' Legend colors
.seqpal<-with(attributes(dat01seq),{
  pal<-rev(rainbow(length(alphabet),alpha=0.1)); 
  case_when(grepl('^Surg',alphabet) ~ 
              adjustcolor(pal,alpha.f=10,red.f=1.6,green.f=0.8)
            ,grepl('Post',alphabet) ~ 
              adjustcolor(pal,alpha.f=10,offset=rep_len(0.5,4))
            ,TRUE~pal) %>% setNames(alphabet)});

# .seqpar <- seqplot(dat01seq,type='I',sortv='from.start',with.legend='auto'
#                    ,use.layout=T,cex.legend=0.5,cpal=.seqpal);
.seqpar <- subset(dat01seq[,1:700],dat01grp=='DiagEMR.SurgEMR.DiagNAACCR') %>%
  seqplot(type='I',sortv='from.start'
          #,with.legend='auto',axes='bottom',ylab=NA,use.layout=T,
          ,xtlab=1:700, cex.legend=0.8,cpal=.seqpal,cex.axis=0.8);
plot(dat01seq[,1:700],idxs = dat01grp=='DiagEMR.SurgEMR.DiagNAACCR'
     ,border=NA,cpal=.seqpal,cex.axis=0.8,xtlab=1:700);
#     ,xtlab=1:700, cex.legend=0.8);

#' ## Recode or derive variables
#'
#' Recode the death-related columns
# 
# doesn't work with this dataset at this time
dat01$v_vitalstatnaaccr <- grepl('NAACCR|1760:0',dat01$v_vitalstatnaaccr);
dat01$vi_dischdsp_death <- grepl('DischDisp:E',dat01$v_dischdsp);
dat01$vi_dischst_death <- grepl('DischStat:EX',dat01$v_dischst);
dat01[,vi_c_death := do.call(pmax,.SD) %>% as.logical()
                          ,.SDcols=v(c_death)];

#' Recode visit-related columns
# 
# doesn't work with this dataset at this time

# dat01$vi_icu <- grepl('VISITDETAIL\\|SPEC:80',dat01$v_department);
#' `vi_emergdept` is emergency department as per provider specialty
#' for now not using this, using the one below
# dat01$vi_emergdept <- grepl('VISITDETAIL\\|SPEC:45',dat01$v_department);
# dat01$vi_ip <- grepl('ENC\\|TYPE:IP',dat01$v_enctype);
#' `vi_ed` is emergency department as per encounter type.
# dat01$vi_ed <- grepl('ENC\\|TYPE:ED',dat01$v_enctype);

#' Create hospital stay variables
#'
#' `z_ipv` : Which inpatient stay is it for this patient?
#dat01[,z_ipv := cumsum(vi_ip),by=patient_num];
#' `z_inip` : Does this row of data represent a day that's part of
#'            an inpatient stay?
#dat01[,z_inip := any(vi_ip) &
      #   seq_len(.N) <= rle(vi_ip|is.na(v_enctype) &
      #                        diff(c(NA,age_at_visit_days))==1)$length[1]
      # ,by=list(patient_num,z_ipv)];
#' `a_los` : Length of stay
#dat01[,a_los := -1][,a_los := ifelse(vi_ip,sum(as.numeric(z_inip)),NA)
      # ,by=list(patient_num,z_ipv)];
#' `z_age_at_disch_days` : age at discharge (needed for readmission calc)
# dat01[,z_age_at_disch_days := -1 ][
#   ,z_age_at_disch_days := age_at_visit_days[1]+a_los[1]-1
#   , by=list(patient_num,z_ipv)];
#' `a_t_since_disch` : at admission, days since previous discharge
# dat01[,a_t_since_disch := -1][
#   ,a_t_since_disch := ifelse(vi_ip & z_ipv > 1
#                              ,age_at_visit_days - shift(z_age_at_disch_days
#                                                         ,fill=-1E6),NA)
#   ,by=patient_num];
#' `vi_readm30` : 30-day readmission
# dat01$vi_readm30 <- !is.na(dat01$a_t_since_disch) & dat01$a_t_since_disch <=30;

#' Simplify `race_cd`
dat01$race_cd <- forcats::fct_collapse(dat01$race_cd,White='white',Black='black'
                                       ,Other=c('other','pac islander'
                                                ,'unknown/othe','more than on')
                                       ,Unknown=c('@','unknown','i choose not')
                                       ,Asian='asian') %>% forcats::fct_infreq();

#' Discharge to intermediate care or skilled nursing (for patients originally
#' admitted from home)
# 
# not working for this dataset
# dat01$vi_snf <- grepl('DischStat:(SN|ICF)',dat01$v_dischst) &
#   grepl('ADMIT|SOURCE:HO',dat01$v_admitsrc);

# debug/QC ----
#' ### QC
#' 
#' Duplicated column names: (should be empty)
names(dat01)[duplicated(names(dat01))];
#' 
#' A list of problems that could possibly occur
#' 
# . problems ----
#' * Categorical mismatches between NAACCR and EMR (including missing)
#'     * Sex
#'     * Hispanic ethnicity
#'     * Race
#' * Date discrepancies
#'     * Diagnosis
#'     * Principal surgery
#'     * Relapse
#'     * Death
#'     
#' * Impossible sequences of events
#'     * Anything prior to birth
#'     * Principal surgery, relapse, or death before first (de-facto) diagnosis
#'     * Relapse or death before principal surgery
#'     * Death before relapse
#'     * Anything at all after death
#' * Patients who had a kidney tumor removed before their first encounter
#' * Patients without an actual kidney tumor, only e.g. renal pelvis
#' * Patients whose kidney tumor was a met from some other location 
#'   (does this happen?)
#'   
# . special subgroups ----
#' Other special subgroups (not necessarily QC issues)
#' 
#' * Patients who have a visit history prior to their kidney cancer
#' * Pediatric patients
#' * Missing NAACCR data
#' * Missing EMR data
#' * Not missing either NAACCR or EMR
#' 
#' Number of visits seeming to occur prior to date of birth
.debug_birth <- subset(dat01,age_at_visit_days < 0);
nrow(.debug_birth);
#' Number of patients with such visits
length(unique(.debug_birth$patient_num)) %>%
  c(number=.,fraction=(.)/length(unique(dat01$patient_num)));
#' Distribution of number of visits per patient
if(nrow(.debug_birth)) .debug_birth[,.N,by=patient_num]$N %>% table %>%
  as.data.frame %>% setNames(c('number suspect visits','patients'));

#' Check the death dates. This is how many visits there are that seem to occur
#' after the patients' date of death:
.debug_death00 <- subset(dat01,age_at_visit_days>age_at_death_days);
nrow(.debug_death00);
#' Number of patients with such visits
length(unique(.debug_death00$patient_num)) %>%
  c(number=.,fraction=(.)/length(unique(dat01$patient_num)));
#' Distribution of number of visits per patient
.debug_death00[,.N,by=patient_num]$N %>% table %>% as.data.frame %>%
  setNames(c('number suspect visits','patients'));
#' Does discharge status ever disagree with discharge disposition for death?
with(dat01,table(vi_dischst_death,vi_dischdsp_death));
#' Does NAACCR vital status ever disagree with discharge disposition for death?
with(dat01,table(v_vitalstatnaaccr,vi_dischdsp_death));
#' How many patients have death records from additional sources?
#' TODO: Update c_death and use that
.debug_death01 <- subset(dat01,vi_dischdsp_death|vi_dischst_death|
                           v_vitalstatnaaccr|
                           age_at_visit_days>=age_at_death_days)[
  ,d_death:=age_at_visit_days >= age_at_death_days];
.debug_death01$patient_num %>% unique %>% length;
#' How many of those patients lack an `age_at_death_days`?
subset(.debug_death01,is.na(age_at_death_days))$patient_num %>%
  unique %>% length;
#' Deceased patients
.debug_decpt <- subset(dat01,!is.na(age_at_death_days))$patient_num %>% unique;
length(.debug_decpt);
#' Deceased patients with visits before DOD
.debug_predodpt <- subset(dat01,age_at_visit_days<=age_at_death_days
                           )$patient_num %>% unique;
length(.debug_predodpt);
#' Deceased patients with visits after DOD
.debug_postdodpt <- subset(dat01,age_at_visit_days>age_at_death_days
                           )$patient_num %>% unique;
length(.debug_postdodpt);
#' Patients with visits ONLY after DOD
.debug_postdodonlypt <- setdiff(.debug_decpt,.debug_predodpt);
length(.debug_postdodonlypt);

#' Anyway, long story short we know that date of death probably not ready to
#' be a first-priority response variable, but can still be a censoring variable
#'
#' FOR NOW: If we get to competing risks before resolving death dates, just
#' censor on earliest death date suggested by the minimum of
#' NAACCR, discharge status/disposition, and nominal date of death.
# dat01[,c('z_deathmin','z_deathmax') := list(
#   pmin(age_at_death_days,age_at_visit_days[match(1,do.call(pmax,.SD))]
#        ,na.rm = TRUE)-z_ixvis
#   ,pmax(age_at_death_days,age_at_visit_days[match(1,do.call(pmax,.SD))]
#         ,na.rm = TRUE)-z_ixvis)
#   ,by=patient_num,.SDcols=v(c_death)];
#'


# make binned variables ----
#' Maked binned versions of certain variables
#' age
dat01$a_agegrp <- cut(dat01$age_at_visit_days,365.25*c(-Inf,45,65,Inf)
                      ,labels=c(' 18-45 ',' 45-65 ',' 65+ '));
# subsamples ----
dat01devel <- dat01[z_subsample=='devel',];
dat01test <- dat01[z_subsample=='test',];

#' # Diagnostic Summary
#'
#' ## Final encounter and patient counts in each dataset
sapply(ls(patt='dat01'),function(xx) {
  yy<-get(xx); c(encounters=nrow(yy)
                 ,patients=length(unique(yy$patient_num)))}) %>% pander;
#' ## Undocumented variables
setdiff(names(dat01),dct0$colname) %>% select_at(dat01,.) %>%
  sapply(class) %>% cbind %>% pander(col.names='class');

# save out ----
#' ## Save all the processed data to tsv files
#'
.outfile <- export(dat01,tempfile(),format='tsv');
file.rename(.outfile,paste0(format(Sys.time(),'%y%m%d%H%M'),'_'
                           ,substr(tools::md5sum(.outfile),1,5),'_'
                           ,submulti(basename(inputdata['dat01'])
                                     ,rbind(c('\\.[^.]*$','.tsv')
                                            ,c('^[0-9]{11,13}_','')))));
.outfile <- export(dat01devel,tempfile(),format='tsv');
file.rename(.outfile,paste0(format(Sys.time(),'%y%m%d%H%M'),'_'
                            ,substr(tools::md5sum(.outfile),1,5),'_'
                            ,submulti(basename(inputdata['dat01'])
                                      ,rbind(c('\\.[^.]*$','_dev.tsv')
                                             ,c('^[0-9]{11,13}_','')))));
.outfile <- export(dat01test,tempfile(),format='tsv');
file.rename(.outfile,paste0(format(Sys.time(),'%y%m%d%H%M'),'_'
                            ,substr(tools::md5sum(.outfile),1,5),'_'
                            ,submulti(basename(inputdata['dat01'])
                                      ,rbind(c('\\.[^.]*$','_test.tsv')
                                             ,c('^[0-9]{11,13}_','')))));

.savelist <- setdiff(ls(),.origfiles);
suppressWarnings(save(file=file.path(.workdir
                                     ,paste0(basename(.currentscript)
                                             ,'.rdata'))
                      ,list=.savelist));

#+ echo=F,eval=F
c()
