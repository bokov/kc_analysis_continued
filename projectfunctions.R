
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
}
