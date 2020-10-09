
#' Left join columns from `yy` onto `xx` and then merge any duplicated columns
#' into the originals with a user-specified command.
#'
#' @param xx,yy        Pair of [data.frame()] like objects
#' @param by           Same as for [dplyr::left_join()] but has a default value.
#' @param yysubset     An [alist()] of arguments passed to [subset()] (for `yy`)
#' @param yydropcols   A character vector of column names to drop from `yy` 
#'                     before merging
#' @param mergefn      What function to use for merging duplicate columns. If 
#'                     `NA` then leave them as-is and this becomes similar to 
#'                     [dplyr::left_join()] but with additional transforms on
#'                     `yy`
#' @param expandaction What to do if the number of rows in the output is 
#'                     is different from the input.
#' @param keepdups     Whether to keep the duplicate columns after merging them.
#'                     `FALSE` by default.
#' @param yysuffix     A suffix by which to identify duplicate columns from `yy`
#'                     No need to change this unless the default value 
#'                     (`___new`) collides with existing suffixes in `xx` or 
#'                     `yy`
#' @param ...          Unused for now
#'
#' @return  Object of the same type as `xx`
#'
# @examples 
left_join_merge <- function(xx,yy,by=c('durablename','name')
                            ,yysubset=alist(TRUE)
                            ,yydropcols=c()
                            ,yymergefn=coalesce
                            ,expandaction=c('warning','error','nothing')
                            ,keepdups=FALSE
                            ,yysuffix='___new',...){
  yy <- do.call(subset,c(list(x=yy),yysubset));
  if(is(yy,'data.table')) yy <- data.frame(yy,check.names=FALSE
                                           ,stringsAsFactors = FALSE);
  yy <- yy[,setdiff(names(yy),yydropcols)];
  out <- dplyr::left_join(xx,yy,by=by,suffix=c('',yysuffix));
  xxrows <- nrow(xx); outrows <- nrow(out);
  if(xxrows != outrows){
    msg <- paste('The number of input rows,', xxrows
                 , 'does not match the number of output rows,', outrows);
    switch(match.arg(expandaction)
           ,warning=warning(msg),error=stop(msg))};
  for(ii in merged <- grep(yysuffix,names(out),val=TRUE)){
    iibase <- gsub(yysuffix,'',ii);
    if(!iibase %in% names(out)){
      warning('Base column not found for',ii
              ,", may need to choose a different `yysuffix` than", yysuffix);
      merged <- setdiff(merged,ii);
    } else {
      out[[iibase]] <- yymergefn(out[[iibase]],out[[ii]]);
      if(!keepdups) out[[ii]] <- NULL;
    }
  }
  if(length(merged)>0){
    message('The following columns were merged into existing ones: '
            , paste(merged,collapse=', '))};
  out;
};

# returns the number of disjoint matches found in each element of vector xx
mostmatches <- function(xx,pattern='TRUE'){
  sapply(gregexpr(pattern,xx),function(yy) sum(yy>0))};

# Take a vector with possibly missing or varying values, and standardize to one
# of several pre-defined values in order of priority. So this despite the name,
# preserved for backward compatibility, this function operates on an ordinary 
# vector in order to, e.g., choose which value should represent that group
adjudicate_levels <- function(xx,levels,...,DEFAULT=tail(na.omit(xx),n=1)
                              ,MISSING=NA){
  xx <- unique(na.omit(xx));
  # If there are no values, return the default value
  if(length(xx)==0) return(MISSING);
  # otherwise, step through the levs list of values and return the first matched
  out <- xx[na.omit(match(unlist(levels),xx))[1]];
  if(!is.na(out)) return(out);
  if(is(DEFAULT,'language')||is(DEFAULT,'function')) return(DEFAULT(xx)) else {
    return(DEFAULT)};
}


#' A function to re-order and/or rename the levels of a factor or 
#' vector with optional cleanup.
#'
#' @param xx            a vector... if not a factor will be converted to 
#'                      one
#' @param lookuptable   matrix-like objects where the first column will
#'                      be what to rename FROM and the second, what to
#'                      rename TO. If there is only one column or if it's
#'                      not matrix-like, the levels will be set to these
#'                      values in the order they occur in `lookuptable`.
#'                      If there are duplicated values in the first column
#'                      only the first one gets used, with a warning.
#'                      Duplicate values in the second column are allowed 
#'                      and are a feature.
#' @param reorder       Whether or not to change the order of the factor
#'                      levels to match those in `lookuptable`. True by
#'                      default (logical). By default is `TRUE`, is set to
#'                      `FALSE` will try to preserve the original order of
#'                      the levels.
#' @param unmatched     Scalar value. If equal to -1 and `reorder` is `TRUE` 
#'                      then unmatched original levels are prepended to the 
#'                      matched ones in their original order of occurence. If 
#'                      equal to 1, then appended in their original order of 
#'                      occurrence. If some other value, then they are all 
#'                      binned in one level of that name. The (empty) new ones 
#'                      always go to the end.
#' @param droplevels    Drop unused levels from the output factor (logical)
#' @param case          Option to normalize case (`'asis'`` leaves it as it was)
#'                      before attempting to match to `lookuptable`. One value 
#'                      only
#' @param mapnato       If the original levels contain `NA`s, what they should 
#'                      be instead. They stay `NA` by default.
#' @param remove        Vector of strings to remove from all level names (can
#'                      be regexps) before trying to match to `lookuptable`.
#' @param otherfun      A user-specified function to make further changes to
#'                      the original level names before trying to match and
#'                      replace them. Should expect to get the output from
#'                      `levels(xx)` as its only argument.
#' @param spec_mapper   A constrained lookup table specification that includes
#'                      a column named `varname` in addition to the two columns
#'                      that will become `from` and `to`. This is for extra
#'                      convenience in projects that use such a table. If you
#'                      aren't working on a project that already follows this
#'                      convention, you should ignore this parameter.
#' @param var           What value should be in the `varname` column of 
#'                      `spec_mapper`
#' @param fromto        Which columns in the `spec_mapper` should become the 
#'                      `from` and `to` columns, respectively. Vector of length
#'                      2, is `c('code','label')` by default.
factorclean <- function(xx,lookuptable,reorder=T,unmatched=1
                        ,droplevels=F,case=c('asis','lower','upper')
                        ,mapnato=NA,remove='"',otherfun=identity
                        ,spec_mapper,var,fromto=c('code','label')){
  if(!is.factor(xx)) xx <- factor(xx);
  lvls <- levels(xx);
  lvls <- switch (match.arg(case)
                  ,asis=identity,lower=tolower,upper=toupper)(lvls) %>% 
    submulti(cbind(remove,'')) %>% otherfun;
  levels(xx) <- lvls;
  # Check to see if spec_mapper available.
  if(!missing(spec_mapper)&&!missing(var)){
    lookuptable <- subset(spec_mapper,varname==var)[,fromto];
  }
  # The assumption is that if you're passing just a vector or something like
  # it, then you want to leave the level names as-is and just want to change
  # the ordering
  if(is.null(ncol(lookuptable))) lookuptable <- cbind(lookuptable,lookuptable);
  # can never be too sure what kind of things with rows and columns are getting 
  # passed, so coerce this to a plain old vanilla data.frame
  lookuptable <- data.frame(lookuptable[,1:2]) %>% setNames(c('from','to'));
  if(length(unique(lookuptable[,1]))!=nrow(lookuptable)) {
    lookuptable <- lookuptable[match(unique(lookuptable$from),lookuptable$from),];
    warning("You have duplicate values in the first (or only) column of your 'lookuptable' argument. Only the first instances of each will be kept");
  }
  if(reorder){
    extras <- data.frame(from=lvls,to=NA,stringsAsFactors = F) %>%
      subset(!from %in% lookuptable$from);
    lookupfinal <- if(unmatched==-1) rbind(extras,lookuptable) else {
      rbind(lookuptable,extras)};
  } else {
    lookupfinal <- left_join(data.frame(from=lvls,stringsAsFactors = F)
                             ,lookuptable,by='from') %>% 
      rbind(subset(lookuptable,!from%in%lvls));
  }
  # if the 'unmatched' parameter has the special value of -1 or 1, leave the 
  # original names for the unmatched levels. Otherwise assign them to the bin
  # this parameter specifies
  lookupfinal$to <- with(lookupfinal,if(unmatched %in% c(-1,1)){
    ifelse(is.na(to),from,to)} else {ifelse(is.na(to),unmatched,to)});
  # Warning: not yet tested on xx's that are already factors and have an NA
  # level
  lookupfinal$to <- with(lookupfinal,ifelse(is.na(from),mapnato,to));
  out <- factor(xx,levels=lookupfinal$from);
  levels(out) <- lookupfinal$to;
  if(droplevels) droplevels(out) else out;
};

update_persist <- function(varname,...,c_groups=c(),persist='data/persistent_dict.tsv'
                           ,varmap='varmap.csv',colname='colname'
                           ,durablename='durablename',name='name'){
  values <- list(...); values <- values[unique(names(values))];
  # try to read 'name' for that varname from varmap
  # try to extract info for that varname from persistent_dict
  # The character elements of c_groups, if any, are given a 'c_' prefix if missing 
  c_groups <- ifelse(grepl('^c_',c_groups),c_groups,paste0('c_',c_groups));
  for(ii in c_groups) values[[ii]] <- TRUE;
  # (to set FALSE or NA, put it into ...)
  # override existing info with 'values' except 'name' and 'durablename'
  # 'durablename' is derived automatically if not found in persistent_dict or 
  # supplied by user (in that order of precedence)
  # name has the following precedence: varmap > persistent_dict >
  # name in 'values' if any > user prompted for name if interactive session
  # otherwise, hash
  # if already exists in persistent_dict, that value is removed and new one
  # inserted in its place (might need to do bind_rows twice) otherwise appended
  # written out to file again.
  # maybe some kind of confirmatory output
  # maybe optionally update the live dictionary
}