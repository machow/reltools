#' Convert data.frame to matrix with Subjects on rows and items on columns
#'
#' @param df dataframe to convert to a matrix
#' @param dv name of column with observations to be put on columns
#' @return matrix with Subjects on the rows and items on the column
#' @examples
#' trial.mat(data.frame(Subject=rep(1:2, each=3), 1:6))
#' trial.mat(data.frame(Subject=rep(1:2, each=3), a=1:6, b=11:16), dv='b')
trial.mat = function(df, dv=NULL){
  # convert simple data.frame with two columns or dv specified
  # so subjects are on the rows and items are on columns.
  if(is.null(dv) & ncol(df) == 2){
    # set dv to other non-subject column
    dv = names(df)[names(df) != 'Subject']
  }
  df = ddply(df, .(Subject), transform, trialnum=1:length(Subject))
  mat = cast(df, Subject ~ trialnum, value=dv)
  M = as.matrix(subset(mat, select=-Subject))       # remove column with subject numbers
  
  # Remove columns with more than half NAs (TODO: throw warning)
  countNA = apply(M, 2, function(col) sum(is.na(col)))
  to_rm = which(countNA > nrow(M) / 2)
  M = if (length(to_rm)) M[,-to_rm] else M
  return(M)
}

#' Convert cocron output (from cocron package) to data.frame
#' 
#' @param out cocron output
#' @param onerow not used
#' @return dataframe with sample size, items, alpha, confidence intervals, and p-value
df.from.cocron = function(out, onerow=FALSE){
  data.frame(sample.size=out@n, 
             number.of.items=out@items, 
             alpha=out@alpha,
             ci=out@conf.int,
             p.val=out@p.value)
}
