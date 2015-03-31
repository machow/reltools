#' Convenience wrapper for cocron.
#' Tests hypothesis that cronbachs alpha for one test length versus another are
#' is equal using Feldt's test (corrected for dependence)
#'
#' @param M1 first matrix to calculate alpha for
#' @param M2 second matrix
#' @param to_df parse cocron output into dataframe
#' @return cocron output or the same output parsed into dataframe
calc.alphadiff = function(M1, M2, to_df=TRUE){
  alphas = c(cronbach(M1)$alpha, cronbach(M2)$alpha)
  if (any(alphas < 0 | is.na(alphas)))
    return("Produces alpha less than 0")
  else out = cocron(list(whole=M1, half=M2), dep=TRUE)
  
  if (to_df) df.from.cocron(out)
  else out
}


#' Cronbach's alpha for bootstrap function
#' 
#' @param data
#' @param x
bootfunc = function(data, x) cronbach(data[x,])$alpha

#' Bootstrap cronbach's alpha with BCA confidence intervals
#' 
#' @param df matrix or dataframe (to be converted using trial.mat)
#' @param n number of bootstrap samples to draw
#' @return dataframe with alpha and confidence intervals
alpha.boot = function(df, n=1000){
  if ('data.frame' %in% class(df)) trialMat = trial.mat(df)
  else trialMat = df
  res = boot(trialMat, bootfunc, n)
  conf = boot.ci(res, type='bca')
  stopifnot(length(conf$bca)==5)
  data.frame(cronbach(trialMat),
             conf.025=conf$bca[4], conf.95 = conf$bca[5])
  
}


#' Calculate cronbach's alpha across a range of test lengths.
#' 
#' @param M matrix with subjects on rows and items on columns
#' @param method method for calculating confidence intervals (standard or boot)
#' @return dataframe with numitems, alpha, and confidence intervals
calc.alpha = function(M, method=c('standard', 'boot')[1]){
  # calculate cronbach's alpha for a range of items
  minitems = floor(ncol(M) / 4)
  if (method == 'standard'){
    ldply(minitems:ncol(M), function(numitems){
      cron = cronbach(M[,1:numitems])
      cron$alpha = max(0, cron$alpha, na.rm = TRUE)
      if (cron$alpha > 0){
        CI = cronbach.alpha.CI(cron$alpha, cron$sample.size, cron$number.of.items)
      }
      else CI = c(lower.bound=0, upper.bound=0)
      return(data.frame(numitems = numitems, alpha = cron$alpha,
                        conf.025 = CI['lower.bound'],
                        conf.95 = CI['upper.bound']
      ))
    })
  }
  else {
    ldply(minitems:ncol(M), function(numitems){
      return(data.frame(numitems = numitems, alpha.boot(M[,1:numitems])))
    })      
  }
  #as.data.frame(cronbach(M))
}
