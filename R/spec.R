

#' Create, edit, and combine model specifications
#'
#' @description
#'
#' `spec_empty()`, `spec_add()`, `spec_rmv()`, and `spec_sub()` are 
#'   functions that can be used to create model fitting specifications. 
#'   `spec_describe()` is a function that helps clarify model specifications
#'   and facilitates interactive work and clear communication.
#'
#' @details
#'
#' Use `spec_empty()` to create a generic unadjusted specification. 
#'  Use `spec_add()`, `spec_rmv()`, and `spec_sub()` to create descendant
#'  model specifications, and use `spec_describe()` to summarize the structure
#'  of the model specifications in a concise manner that accounts for
#'  their dependencies (see examples).
#'
#' @family model specification functions
#'
#' @param name (character value). The label that will be used 
#'   to describe this model specification
#' @param control A set of variable names (separated by commas) that will be
#'   used to adjust estimates in fitted models when this specification is
#'   fit. The values may be supplied as unquoted variable names or as 
#'   a character vector.
#' @param parent (`apri_spec` object). If you are creating an empty model,
#'   this argument is not needed. For all descendant models, this argument
#'   specifies what specification the descendant is derived from.
#' @param relation (character value). Descendant models may add, remove, 
#'   or substitute control variables from their parent model. This argument
#'   sets the relationship between parent and descendant. Valid inputs are
#'   'add', 'rmv', or 'sub'. 
#'   
#' @return
#'  - `spec_empty()` returns an unadjusted model specification.
#'  - `spec_add()`, `spec_rmv()` returns a model specification with the 
#'      designated control variables added or removed from the parent 
#'      model specification.
#'  - `spec_sub()` returns a model specification with the designated
#'      control variables (left hand side of the `=`) replaced by 
#'      variable names supplied by the user (right hand side of the `=`) 
#'
#' @examples
#'
#' 
#' # Make an unadjusted model
#' m0  <- spec_empty("Model 0")
#' 
#' # spec_describe(spec) is the same as print(spec)
#' spec_describe(m0)
#' print(m0)
#' 
#' # Model 1 includes adjustment for sex and class
#' m1  <- spec_add(m0, name = "Model 1", sex, pclass)
#' 
#' m1
#' 
#' # Model 2a = model 1 + no. of siblings/spouses
#' m2a <- spec_add(m1, name = 'Model 2a', sibsp)
#' 
#' m2a
#' 
#' # Model 2b = model 1 + no. of parents/children
#' m2b <- spec_add(m1, name = 'Model 2b', parch)
#' 
#' m2b
#' 
#' # Model 3 = model 1, swapping out class for ticket fare
#' 
#' m3 <- spec_sub(m1, name = 'Model 3', pclass = fare)
#' 
#' m3
#'
#' @export

spec_new <- function(
  name = "Model 0",
  control = "1",
  parent = NULL,
  relation = NULL
){
  
  structure(
    .Data = list(
      name = name,
      control = control,
      parent = parent,
      relation = relation
    ),
    class = 'apri_spec'
  )
  
}

#' @rdname spec_new
#' @export
spec_empty <- function(name = 'Model 0'){
  
  spec_new(
    name = name
  )
  
}

#' @rdname spec_new
#' @export
spec_add <- function(model_spec, name, ...){
  
  vars_to_add <- map_chr(ensyms(...), deparse)
  
  spec_new(
    name = name,
    control = union(model_spec$control, vars_to_add),
    parent = model_spec,
    relation = 'add'
  )
  
}

#' @rdname spec_new
#' @export
spec_rmv <- function(model_spec, name, ...){
  
  vars_to_rmv <- map_chr(ensyms(...), deparse)
  
  spec_new(
    name = name,
    control = setdiff(model_spec$control, vars_to_rmv),
    parent = model_spec,
    relation = 'rmv'
  )
  
}

#' @rdname spec_new
#' @export
spec_sub <- function(model_spec,  name, ...){
  
  .dots <- map_chr(enexprs(...), deparse)
  
  spec_new(
    name = name,
    control = recode(model_spec$control, !!!.dots),
    parent = model_spec,
    relation = 'sub'
  )
  
}

#' @rdname spec_new
#' @export
spec_describe <- function(model_spec){
  
  if(all(model_spec$control == "1")){
    out <- paste(model_spec$name, "is unadjusted.")
    return(as.character(out))
  }
  
  new_vars <- setdiff(model_spec$control, model_spec$parent$control)
  old_vars <- setdiff(model_spec$parent$control, model_spec$control)
  
  if(model_spec$relation == 'sub'){
    
    out_vars <- glue_collapse(
      x = glue("{new_vars} replacing {old_vars}"),
      sep = ', ',
      last = ' and '
    )
    
  } 
  
  if(model_spec$relation == 'add'){
    out_vars <- glue_collapse(new_vars, sep = ', ', last = ' and ')
  }
  
  if(model_spec$relation == 'rmv'){
    out_vars <- glue_collapse(old_vars, sep = ', ', last = ' and ')
  }
  
  if(all(model_spec$parent$control == "1")){
    out <- glue("{model_spec$name} includes adjustment for {out_vars}.")
    return(as.character(out))
  }
  
  string1 <- glue(
    "{model_spec$name} includes adjustment for \\
    variables in {model_spec$parent$name}"
  )
  
  string2 <- switch(
    EXPR = model_spec$relation,
    "add" = glue("plus {out_vars}."),
    "rmv" = glue("minus {out_vars}."),
    "sub" = glue("with {out_vars}.")
  )
  
  out <- paste(string1, string2)
  
  return(as.character(out))
  
}

#' push a model specification into a model formula.
#' @param model_spec a model specification object
#' @param formula a formula with outcome and exposure.

spec_push <- function(model_spec, formula){
  
  vars_to_add <- paste(model_spec$control, collapse = ' + ')
  
  exposure <- rhs.vars(formula)
  
  output <- update.formula(
    old = formula,
    new = as.formula(
      glue("~ . + {vars_to_add}")
    )
  )
  
  attr(output, 'exposure') <- exposure 
  
  output
  
}

#' combine all pairs of outcome ~ exposure with
#'   all model specifications in a given set.
#' @param formula two-sided formula with outcome(s) 
#'   on the left hand side and exposure(s) on the
#'   right hand side.
#' @param ... model specification objects.
#' @export

spec_embed <- function(formula, ...) {
  
  control <- list(...)
  
  model_names <- map_chr(control, ~.x$name)
  
  if (is.one.sided(formula)) {
    msg <- "formula must have a response, i.e. response ~ exposure"
    stop(msg, call. = FALSE)
  }
  
  formula_lhs <- deparse(lhs(formula)) %>% 
    strsplit("+", fixed = TRUE) %>%
    flatten() %>% 
    reduce(base::c) %>% 
    trimws() 
  
  if(any(grepl("Surv", formula_lhs, fixed=TRUE))) {
    
    formula_lhs_labs <- sf_labels(formula_lhs)
    
  } else {
    
    formula_lhs_labs <- formula_lhs
    
  }
  
  formula_data <- expand.grid(
    outcome = formula_lhs,
    exposure = rhs.vars(formula)
  ) %>% 
    mutate(formula = paste(outcome, exposure, sep = ' ~ '))
  
  formulas <- formula_data %>% 
    pull(formula) %>% 
    map(as.formula) %>% 
    map(
      .f = function(formula){
        map(
          .x = set_names(control, model_names),
          .f = spec_push,
          formula = formula
        )
      }
    ) %>% 
    flatten()
  
  output <- enframe(formulas, value = 'formula') %>% 
    mutate(
      outcome = map_chr(formula, ~sf_labels(deparse(lhs(.x)))),
      exposure = map_chr(formula,~all.vars(rhs(.x))[1])
    ) %>% 
    select(name, outcome, exposure, formula)
  
  
  
}

#' print a model specification
#' @param x An a-priori model specification object to be printed
#' @param ... additional arguments passed to print
#' @export
#' 

print.apri_spec <- function(x, ...){
  
  print(spec_describe(x), ...)
  
}


sf_labels <- function(string){
  
  output <- regmatches(
    x = string,
    m = gregexpr(
      pattern = "(?<=\\().*?(?=\\))", 
      text = string, 
      perl = TRUE
    )
  ) %>% 
    reduce(c)
  
  if(vec_is_empty(output)) {
    return(string)
  }
  
  output
  
}

