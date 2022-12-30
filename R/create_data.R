getTrainingData <- function(series, ar_order){
  
  series.length = length(series)
  
  ar_order = max(ar_order, 1)
  
  ncol = ar_order+1
  
  nrow = series.length - ar_order
  
  data = matrix(nrow = nrow, ncol = ncol)
  
  for(col in 1:ncol){
    data[,col] = series[(ncol -col + 1):(series.length - col + 1)]
  }
  
  return(as.data.frame(data))
}

getTestData <- function(series, ar_order){
  
  series.length = length(series)
  
  ar_order = max(1, ar_order)
  
  test.data = matrix(nrow = 1, ncol = ar_order)
  
  if(ar_order == 0)
    test.data[1,1] = series[series.length]
  
  
  else{
    for(col in 1:ar_order){
      test.data[, col] = series[(series.length - col + 1)]
    }
  }
  
  
  ##### Naming the columns of the test Data V2,..,V_{p+1}
  column.names = c()
  for(col in 1:ar_order){
    column.names = c(column.names, paste0("V", (col+1)))
  }
  
  test.data = as.data.frame(test.data)
  
  colnames(test.data) = column.names
  
  return(test.data)
}


ar_model <- function(series){
 
  #if(aic)
  ar_fit = stats::ar(series)
  # else
  #   ar_fit = ar(series, demean = TRUE)
  
  #obtain errors and center them around 0
  ar_error = ar_fit$resid[which(!is.na(ar_fit$resid))]
  ar_error = ar_error - mean(ar_error)
  
  #Obtain coefficients; if ar(0) is fitted, coefficient = 0
  coefs = ar_fit$ar
  #coefs = ar_fit$coef
  if(length(coefs) == 0) 
    coefs = 0
  
  #ar_order
  ar_order = length(coefs)
  
  # mean = 0
  # if(demean){
    #mean = ar_fit$x.mean
    coefs = c(coefs, ar_fit$x.mean)
  #}
  
  
  return(list(coefs = coefs, errors = ar_error, order = ar_order))
  
}