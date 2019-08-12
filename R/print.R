#' print a model specification
#' @param x An a-priori model specification object to be printed
#' @param ... additional arguments passed to print
#' @export
#' 

print.apriori <- function(x, ...){
  
  atts <- attributes(x)
  main_hypo <- as.character(atts$main_hypothesis)
  n_control <- vec_size(atts$model_description)
  
  atts$model_description[1] <- paste(" ", atts$model_description[1])
  mdl_descr <- glue_collapse(atts$model_description, sep = '\n  ')
  
  out <- glue(
    "A priori model specifications for assessing {main_hypo}: \n {mdl_descr}"
  )
  
  print(out)
  cat("\n Analysis object \n")
  print(x$analysis)
  
}

#' print an apriori analysis plan
#' @param x An a-priori object to be printed
#' @param ... additional arguments passed to print
#' @export
#' 

print.apri_mspec <- function(x, ...){
  
  print(mspec_describe(x), ...)
  
}


#' helper function to work with survival formulas.
#'   Generates labels that look reasonable in printed dataframes
#' @param string the left hand side of a survival formula.
#' 

sf_labels <- function(string){
  
  output <- regmatches(
    x = string,
    m = gregexpr(
      pattern = "(?<=\\().*?(?=\\))", 
      text = string, 
      perl = TRUE
    )
  ) %>% 
    reduce(c) %>% 
    str_split(fixed(','), simplify = TRUE) %>% 
    trimws() %>% 
    apply(1, function(x) paste(x, collapse = '/'))
  
  if(vec_is_empty(output)) {
    return(string)
  } else {
    output
  }
  
}