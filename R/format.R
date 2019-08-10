

#' format estimated regression coefficients from a model.
#'   It is common to present effects in terms of odds ratios,
#'   prevalence ratios, or hazard ratios by applying the exponential
#'   function (`exp`) to the regression coefficients. It is also
#'   common to present these ratios with 95% confidence intervals
#'   (i.e., `conf_level = 0.95`) in model summary tables. 
#' @param effect a numeric vector of estimated regression coefficients
#' @param std.error a numeric vector of standard error estimates corresponding
#'   to the regression coefficients in the `effect` vector.
#' @param transform a function that will be applied to the values in `effect`
#'   (e.g., exponentiating to produce odds ratios).
#' @param conf_level (optional) the desired confidence level for interval
#'   estimation. It is common to use `conf_level = 0.95` to produce 95%
#'   confidence intervals.
#' @param reference_index (optional) If there is a referent group in the
#'   `effect` values, specify the indices of the group. For example, if
#'   `effect = c(0, 1/2, 1)` describing the relative increase in hazard 
#'   for groups B and C relative to A, then `reference_index = 1`.
#' @param reference_label (optional) a string indicating how the reference
#'   group will be labeled in representations of the model effects.
#'   
#' @export

fmt_effect <- function(
  effect,
  std.error,
  transform = NULL,
  conf_level = NULL,
  reference_index = NULL,
  reference_label = '1 (reference)'
) {
  
  fun <- transform %||% function(x) return(x)
  
  if(!is.null(conf_level)){
    alpha = 1 - conf_level
    
    output <- paste0(
      adapt_round(fun(effect)), " (",
      adapt_round(fun(effect + qnorm(alpha / 2) * std.error)), ", ",
      adapt_round(fun(effect + qnorm(1 - alpha / 2) * std.error)), ")"
    )
    
  } else {
    
    output <- adapt_round(fun(effect))
    
  }
  
  if(vec_size(reference_index) > 0){
    output[reference_index] <- reference_label
  }
  
  output
  
}