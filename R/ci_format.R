

ci_format <- function(
  eff, 
  err, 
  fun = NULL, 
  alpha = 0.05, 
  reference_label = "1 (reference)"
){
  
  fun <- fun %||% function(x) return(x)
  
  na <- is.na(eff)
  ref <- which(err == -1)
  
  output <- rep(NA_character_, length(eff))
  
  output[!na] <- paste0(
    adapt_round(fun(eff[!na])), " (",
    adapt_round(fun(eff[!na] + qnorm(alpha / 2) * err[!na])), ", ",
    adapt_round(fun(eff[!na] + qnorm(1 - alpha / 2) * err[!na])), ")"
  )
  
  if(length(ref) > 0){
    output[ref] <- reference_label
  }
  
  output
  
}

