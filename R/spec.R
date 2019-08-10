

#' Create, edit, and combine model specifications
#'
#' @description
#'
#' `mspec_empty()`, `mspec_add()`, `mspec_rmv()`, and `mspec_sub()` are 
#'   functions that can be used to create model fitting specifications. 
#'   `mspec_describe()` is a function that helps clarify model specifications
#'   and facilitates interactive work and clear communication.
#'
#' @details
#'
#' Use `mspec_empty()` to create a generic unadjusted specification. 
#'  Use `mspec_add()`, `mspec_rmv()`, and `mspec_sub()` to create descendant
#'  model specifications, and use `mspec_describe()` to summarize the structure
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
#' @param parent (`apri_mspec` object). If you are creating an empty model,
#'   this argument is not needed. For all descendant models, this argument
#'   specifies what specification the descendant is derived from.
#' @param relation (character value). Descendant models may add, remove, 
#'   or substitute control variables from their parent model. This argument
#'   sets the relationship between parent and descendant. Valid inputs are
#'   'add', 'rmv', or 'sub'. 
#'   
#' @return
#'  - `mspec_empty()` returns an unadjusted model specification.
#'  - `mspec_add()`, `mspec_rmv()` returns a model specification with the 
#'      designated control variables added or removed from the parent 
#'      model specification.
#'  - `mspec_sub()` returns a model specification with the designated
#'      control variables (left hand side of the `=`) replaced by 
#'      variable names supplied by the user (right hand side of the `=`) 
#'
#' @examples
#'
#' 
#' # Make an unadjusted model
#' m0  <- mspec_empty("Model 0")
#' 
#' # mspec_describe(mspec) is the same as print(mspec)
#' mspec_describe(m0)
#' print(m0)
#' 
#' # Model 1 includes adjustment for sex and class
#' m1  <- mspec_add(m0, name = "Model 1", sex, pclass)
#' 
#' m1
#' 
#' # Model 2a = model 1 + no. of siblings/spouses
#' m2a <- mspec_add(m1, name = 'Model 2a', sibsp)
#' 
#' m2a
#' 
#' # Model 2b = model 1 + no. of parents/children
#' m2b <- mspec_add(m1, name = 'Model 2b', parch)
#' 
#' m2b
#' 
#' # Model 3 = model 1, swapping out class for ticket fare
#' 
#' m3 <- mspec_sub(m1, name = 'Model 3', pclass = fare)
#' 
#' m3
#'
#' @export

mspec_new <- function(
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
    class = 'apri_mspec'
  )
  
}

#' @rdname mspec_new
#' @export
mspec_empty <- function(name = 'Model 0'){
  
  mspec_new(
    name = name
  )
  
}

#' @rdname mspec_new
#' @export
mspec_add <- function(mspec, name, ...){
  
  vars_to_add <- map_chr(ensyms(...), deparse)
  
  mspec_new(
    name = name,
    control = union(mspec$control, vars_to_add),
    parent = mspec,
    relation = 'add'
  )
  
}

#' @rdname mspec_new
#' @export
mspec_rmv <- function(mspec, name, ...){
  
  vars_to_rmv <- map_chr(ensyms(...), deparse)
  
  mspec_new(
    name = name,
    control = setdiff(mspec$control, vars_to_rmv),
    parent = mspec,
    relation = 'rmv'
  )
  
}

#' @rdname mspec_new
#' @export
mspec_sub <- function(mspec,  name, ...){
  
  .dots <- map_chr(enexprs(...), deparse)
  
  mspec_new(
    name = name,
    control = recode(mspec$control, !!!.dots),
    parent = mspec,
    relation = 'sub'
  )
  
}

#' @rdname mspec_new
#' @export
mspec_describe <- function(mspec){
  
  if(all(mspec$control == "1")){
    out <- paste(mspec$name, "is unadjusted.")
    return(as.character(out))
  }
  
  new_vars <- setdiff(mspec$control, mspec$parent$control)
  old_vars <- setdiff(mspec$parent$control, mspec$control)
  
  if(mspec$relation == 'sub'){
    
    out_vars <- glue_collapse(
      x = glue("{new_vars} replacing {old_vars}"),
      sep = ', ',
      last = ' and '
    )
    
  } 
  
  if(mspec$relation == 'add'){
    out_vars <- glue_collapse(new_vars, sep = ', ', last = ' and ')
  }
  
  if(mspec$relation == 'rmv'){
    out_vars <- glue_collapse(old_vars, sep = ', ', last = ' and ')
  }
  
  if(all(mspec$parent$control == "1")){
    out <- glue("{mspec$name} includes adjustment for {out_vars}.")
    return(as.character(out))
  }
  
  string1 <- glue(
    "{mspec$name} includes adjustment for \\
    variables in {mspec$parent$name}"
  )
  
  string2 <- switch(
    EXPR = mspec$relation,
    "add" = glue("plus {out_vars}."),
    "rmv" = glue("minus {out_vars}."),
    "sub" = glue("with {out_vars}.")
  )
  
  out <- paste(string1, string2)
  
  return(as.character(out))
  
}

#' push a model specification into a model formula.
#' @param mspec a model specification object
#' @param formula a formula with outcome and exposure.

mspec_push <- function(mspec, formula){
  
  vars_to_add <- paste(mspec$control, collapse = ' + ')
  
  exposure <- rhs.vars(formula)
  outcome <- lhs.vars(formula)
  
  output <- update.formula(
    old = formula,
    new = as.formula(
      glue("~ . + {vars_to_add}")
    )
  )
  
  attr(output, 'exposure') <- exposure 
  attr(output, 'outcome') <- outcome
  
  output
  
}

#' combine all pairs of outcome ~ exposure with
#'   all model specifications in a given set.
#' @param formula two-sided formula with outcome(s) 
#'   on the left hand side and exposure(s) on the
#'   right hand side.
#' @param ... model specification objects.
#' @export



