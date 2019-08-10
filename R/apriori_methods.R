

#' extract the analysis component from an apriori object.
#'  This makes it easier to pass apriori objects into other
#'  functions for presentation.
#' @param object an apriori object
#' @param ... further arguments passed on to \code{\link[dplyr]{select}}
#' @export

pull_analysis <- function(object, ...) {
  
  .dots <- ensyms(...)
  
  output <- object$analysis
  
  if(!vec_is_empty(.dots)){
    output %<>% select(!!!.dots)
  }
  
  output
  
}


