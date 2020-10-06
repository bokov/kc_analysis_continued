renameoutput <- function(file=get('.currentscript')
                         ,fromdata=basename(get('inputdata')['dat03'])
                         ,suffix='.html'){
  fromdata <- gsub('^[0-9]{10,12}_[0-9a-z]{5,7}',''
                   ,fromdata) %>% gsub('\\.[^.]*$','',.);
  file <- file.path(dirname(file),gsub('\\.[^.]*$','',basename(file)));
  newfile <- file.path(dirname(file)
                       ,paste0(basename(file),'_'
                               ,substr(tools::md5sum(paste0(file,suffix)),1,5)
                               ,fromdata,suffix));
  file.symlink(paste0(file,suffix),newfile);
  if(file.exists(newfile)) sprintf('Linked %s to %s',paste0(file,suffix)
                                   ,newfile);
  newfile;
}

fs <- function(str,text=str,url=paste0('#',gsub('[^_a-z0-9]','-',tolower(str)))
               ,tooltip=alist(str),class='note2self'
               # %1 = text, %2 = url, %3 = class, %4 = tooltip
               # TODO: %5 = which position 'str' occupies in fs_reg if
               #       applicable and if not found, append 'str'
               #,template='[%1$s]: %2$s "%4$s"\n'
               ,template='[%1$s]{ .%3$s custom-style="%3$s" }'
               # Turns out that the below template will generate links, but they
               # only render properly for HTML output because pandoc doesn't
               # interpret them. However, if we use the markdown implicit link
               # format (https://pandoc.org/MANUAL.html#reference-links) we
               # don't have to wrap links in anything, but we _can_ use fs()
               # with the new template default above to generate a block of
               # link info all at once in the end. No longer a point in using
               # the fs_reg feature for this case, the missing links will be
               # easy to spot in the output hopefully
               #,template="<a href='%2$s' class='%3$s' title='%4$s'>%1$s</a>"
               ,dct=NA,col_tooltip='colname_long',col_class='',col_url=''
               ,col_text='',match_col=c('varname','colname'),fs_reg=NULL
               ,retfun=return # could also be cat
               #,fs_reg='fs_reg'
               ,...){
  # if a data dictionary is specified use that instead of the default values
  # for arguments where the user has not explicitly provided values (if there
  # is no data dictionary or if the data dictionary doesn't have those columns,
  # fall back on the default values)
  if(is.data.frame(dct) &&
     length(match_col<-intersect(match_col,names(dct)))>0){
    dctinfo <- dct[match(str,do.call(coalesce,dct[,match_col])),];
    #!all(is.na(dctinfo <- dct[which(dct[[match_col]]==str)[1],]))){
    if(missing(tooltip) #&&
       #length(dct_tooltip<-na.omit(dctinfo[[col_tooltip]]))==1) {
       #tooltip <- dct_tooltip;}
    ){tooltip <- do.call(coalesce,c(dctinfo[,col_tooltip],tooltip,''))};
    if(missing(text) &&
       length(dct_text<-na.omit(c(dctinfo[[col_text]],NA)))==1) {
      text <- dct_text;}
    if(missing(url) &&
       length(dct_url<-na.omit(c(dctinfo[[col_url]],NA)))==1) {
      url <- dct_url;}
    if(missing(class) &&
       length(dct_class<-na.omit(c(dctinfo[[col_class]],NA)))==1) {
      class <- dct_class;}
  } else dctinfo <- data.frame(NA);
  out <- sprintf(rep(template,nrow(dctinfo)),text,url,class,tooltip,...);
  # register each unique str called by fs in a global option specified by
  # fs_register
  if(!is.null(fs_reg)) {
    dbg<-try(do.call(options,setNames(list(union(getOption(fs_reg),str))
                                      ,fs_reg)));
    if(is(dbg,'try-error')) browser();
  }
  retfun(out);
}
