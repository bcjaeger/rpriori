

gee_no_ID <- function (formula, family, data, ...) {
  
  .dots <- list(...)
  
  if ("id" %in% names(.dots)) .dots$id = NULL
  
  ids = 1:nrow(data)
  
  .dots$id = ids
  .dots$data = data
  .dots$family = family
  .dots$formula = formula

  do.call(
    what = geepack::geeglm,
    args = .dots
  )
    
}
