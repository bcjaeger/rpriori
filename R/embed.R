

#' embed a set of model specifications into a primary hypothesis.
#' @param formula a two-sided formula containing outcome(s) on the
#'   left and exposure(s) on the right. (Control variables do not
#'   belong here.)
#' @param ... Model specification objects created using \link{mspec_new}, 
#'  \link{mspec_add}, \link{mspec_sub}, etc. (Control variables do belong
#'  here).
#' @export

embed_mspecs <- function(formula, ...) {
  
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
          .f = mspec_push,
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
  
  attr(output, 'main_hypothesis') = formula
  attr(output, 'model_description') = map_chr(control, mspec_describe)
  
  output
  
}

#' embed a dataframe into a set of model specifications
#' @param mspecs two-sided formula with outcome(s) 
#'   on the left hand side and exposure(s) on the
#'   right hand side.
#' @param ... key and value pairs specifying labels for variables in data.
#'   For example, a variable called sbp may have the label of systolic blood
#'   pressure. This could be set by writing sbp = 'systolic blood pressure'
#' @export

embed_data <- function(mspecs, data, ...) {
  
  .dots <- list(...)
  
  if(!vec_is_empty(names(.dots))){
    for(i in names(.dots)){
      attr(data[[i]], 'label') <- .dots[[i]]
    }
  }
  
  var_data <- create_var_data(
    formulas = mspecs$formula, 
    data = data
  )
  
  output <- list(
    analysis = mspecs,
    var_data = var_data,
    fit_data = data
  )
  
  class(output) <- "apriori"
  
  attr(output, 'main_hypothesis') = attr(mspecs, 'main_hypothesis')
  attr(output, 'model_description') = attr(mspecs, 'model_description')
  
  output
  
}

#' Create a dataset with variable names, types, and 
#'   the corresponding terms related to that variable 
#'   in statistical models. 
#'   
#' @param formulas a list of formulas
#' @param data the dataframe that will be used to fit models with `formulas`

create_var_data <- function(formulas, data){
  
  analysis_variables <- map(formulas, all.vars) %>% 
    reduce(union) %>% 
    purrr::set_names() %>% 
    map_lgl(~any(is.na(data[[.x]]))) %>% 
    enframe('variable', 'has_missing') %>% 
    mutate(type = map_chr(variable, ~class(data[[.x]])[1]))
  
  outcome_variables <- formulas %>% 
    map(~all.vars(lhs(.x))) %>% 
    reduce(base::c) %>% 
    unique()
  
  if(any(analysis_variables$has_missing)){
    
    miss_vars <- analysis_variables %>% 
      filter(has_missing == TRUE) %>% 
      pull(variable) %>% 
      glue_collapse(sep = ', ', last = ' and ')
    
    msg <- glue("There are missing values in {miss_vars}")
    stop(msg, call. = FALSE)
    
  }
  
  analysis_variables %<>% select(-has_missing)
  
  if(any(analysis_variables$type == 'character')){
    
    char_vars <- analysis_variables %>% 
      filter(type == "character") %>% 
      pull(variable)
    
    var_string <- if(length(char_vars)>1) "are" else "is"
    
    char_vars %<>% glue_collapse(sep = ', ', last = ' and ')
    
    msg <- glue(
      "{char_vars} {var_string} coded as character values. \\
      Convert these variables to factors before modeling."
    )
    
    stop(msg, call. = FALSE)
    
  }
  
  fctr_variables <- analysis_variables %>% 
    filter(type %in% c('factor','ordered')) %>% 
    pull(variable)
  
  if(length(fctr_variables > 0)){
    
    fctr_variables %<>% 
      purrr::set_names() %>% 
      map(~levels(data[[.x]])) %>% 
      enframe('variable', 'level') %>% 
      unnest() %>% 
      filter(!variable %in% outcome_variables) %>% 
      mutate(term = paste0(variable, level))
    
  } else {
    
    fctr_variables = NULL
    
  }
  
  nmrc_variables <- analysis_variables %>% 
    filter(type %in% c('numeric','integer')) %>% 
    pull(variable)
  
  if(length(nmrc_variables > 0)){
    
    nmrc_variables %<>% 
      purrr::set_names() %>% 
      map(~ attr(data[[.x]], 'unit') %||% "1 unit" ) %>% 
      enframe('variable', 'level') %>% 
      unnest() %>% 
      filter(!variable %in% outcome_variables) %>% 
      mutate(term = paste0(variable))
    
  } else {
    
    nmrc_variables = NULL
    
  }
  
  variable_labels <- names(data) %>% 
    purrr::set_names() %>% 
    map_chr(~ attr(data[[.x]], 'label') %||% .x) %>% 
    enframe(name = 'variable', value = 'label')
  
  output <- bind_rows(nmrc_variables, fctr_variables) %>% 
    left_join(variable_labels, by = 'variable') %>% 
    left_join(analysis_variables, by = 'variable')
  
  refs <- output %>% 
    filter(type %in% c('factor', 'ordered')) %>% 
    group_by(variable) %>% 
    slice(1) %>% 
    pull(term)
  
  output %>%
    mutate(ref = term %in% refs) %>% 
    select(variable, type, term, ref, label, level)
  
}


#' A function to fit various types of models based on apriori 
#'   specifications. This function should be applied within the 
#'   context of the `mutate()` function in the `dplyr` package.
#' 
#' @param object an object of class 'apriori'
#' @param family (character or function) a description of the error 
#'   distribution and link function to be used in the model. Valid options
#'   for character values are 'binomial' and 'poisson' for binary and
#'   count models, respectively. For regression models, family can be
#'   left unspecified but engine should be set to 'lm'.
#' @param engine a character value specifying the model type. 
#'   Valid options are 
#'   
#'   - 'lm' for linear models
#'   
#'   - 'glm' for generalized linear models
#'   
#'   - 'gee' for generalized linear models fitted with 
#'      generalized estimating equations
#'   
#'   - 'cph' for Cox proportional hazards models.
#' @param keep_models (T/F) if TRUE, the model output will 
#'   be saved from its native R function. If FALSE, only 
#'   point estimates and covariance estimates will be returned.
#' @param ... Arguments to be passed into the specified engine function.
#' @export

# object = apri
# engine = 'glm'
# .dots <- list(family = binomial(link = 'logit'))

embed_fits <- function(
  object, 
  engine,
  keep_models = FALSE,
  fill_models = TRUE,
  ...
){
  
  .dots <- list(...)
  
  if(engine %in% c('glm','gee')) {
    
    family_specified <- 'family' %in% names(.dots)
    
    if(!family_specified){
      msg <- "family must be specified for glm and geeglm objects"
      stop(msg, call. = FALSE)
    }
    
    family <- .dots$family
    
    if (is.character(family)) {
      family <- get(family, mode = "function", envir = parent.frame())
    }
    
    if (is.function(family)){
      family <- family()
    }
    
    if (is.null(family$family)) {
      print(family)
      stop("'family' not recognized")
    }
    
    if(engine == 'gee'){
      
      if(family$family %in% c('binomial', 'poisson')) {
        
        outcomes <- lhs.vars(attr(object, 'main_hypothesis'))
        
        outcome_is_fctr <- map_lgl(
          .x = outcomes, 
          .f = ~is.factor(object$fit_data[[.x]])
        )
        
        if(any(outcome_is_fctr)){
          
          stop(
            glue("geeglm expects numeric outcome variables for \\
            {family$family} models. Change {outcome} to numeric."),
            call. = FALSE
          )
          
        }
        
      }
      
    }
    
  }
    
  fit_fun <- if (is_character(engine)) {
    switch(
      engine,
      'lm'  = lm_apri,
      'glm' = glm_apri,
      'gee' = glm_apri,
      'cph' = cph_apri,
      stop(
        glue(
          "{engine} engine is unrecognized. Use lm, glm, gee, or cph"
        ),
        call. = FALSE
      )
    )
  }
  
  output <- map(
    .x = object$analysis$formula,
    .f = fit_fun,
    engine = engine,
    data = object$fit_data,
    mvars = object$var_data,
    fill_models = fill_models,
    keep_models = keep_models,
    ...
  ) %>% 
    map(
      .f = function(mdl){
          class(mdl) <- "apri_fit"  
          mdl
        }
      )
  
  object$analysis$fit = output
  
  object

}



# model = apri$analysis$fit[[1]]$model
# exposure = 'pclass'
# fitter = stats::glm
# apri_model = apri$analysis$fit[[1]]
# mvars = apri$var_data
# family = model$family$family
# data = apri$fit_data

# .dots <- list(family = family, data = data)

fill_estimates <- function(
  model, 
  exposure, 
  control,
  fitter,  
  mvars, 
  ...
){
  
  .dots <- list(...)
  
  variables = unique(mvars$variable)
  
  variables_to_add <- setdiff(variables, exposure)
  
  output <- select(mvars, variable, term, level, ref) %>% 
    mutate(
      estimate = 0, 
      std.error = 0,
      pv_term = NA_real_,
      pv_ovrl = NA_real_
    ) %>% 
    filter(variable != exposure)
  
  .frmla <- as.character(model$formula)
  
  for(v in variables_to_add){
    
    v_is_control_variable <- v %in% control
    
    # If v is a control variable, it's already in the model,
    # so we just need to remove the exposure variable. Otherwise,
    # we need to remove the exposure variable and also include v.
    if(v_is_control_variable){
      frmla_string <- "{.frmla} - {exposure}"
    } else {
      frmla_string <- "{.frmla} + {v} - {exposure}"
    }
    
    .dots$formula <- as.formula(glue(frmla_string))
    
    # Fit a new model that does not contain the exposure variable
    # (why?) to get the estimated effect of this variable (v) in
    # the same model that we estimated the effect of the main
    # exposure variable in.
    .model <- do.call(fitter, args=.dots)
    
    vv <- output %>% 
      filter(variable == v, !ref) %>% 
      pull(term)
    
    pv_ovrl <- pv_term <- get_term_pval(model = .model, variable = vv)
    
    # Overall p-value
    if(vec_size(vv) > 1L){
      pv_ovrl <- get_ovrl_pval(model = .model, variable = v)
    } 

    output$estimate[output$term %in% vv] <- coef(.model)[vv]
    output$std.error[output$term %in% vv] <- sqrt(diag(vcov(.model)))[vv]
    output$pv_term[output$term %in% vv] <- pv_term
    output$pv_ovrl[output$term %in% vv] <- pv_ovrl
    
  }
  
  output
  
}

glm_apri <- function(
  formula,
  data,
  mvars,
  engine,
  fill_models,
  keep_models,
  ...
) {
  
  exposure = attr(formula, 'exposure')
  
  if(engine == 'gee'){
    
    model <- gee_no_ID(
      formula = formula,
      data = data, 
      ...
    )
    
  } else if(engine == 'glm'){
    
    model <- glm(
      formula = formula, 
      data = data,
      ...
    )
    
  } else if(engine == 'lm'){
    
    model <- lm(
      formula = formula, 
      data = data,
      ...
    )
    
  } else if(engine == 'cph'){
    
    model <- coxph(
      formula = formula, 
      data = data, 
      ...
    )
    
  }
  
  terms <- mvars %>%
    filter(variable == exposure, !ref) %>%
    pull(term)
  
  beta_df <- enframe(
    x = coef(model), 
    name = 'term', 
    value = 'estimate'
  )
  
  covb_df <- enframe(
    x = sqrt(diag(vcov(model))), 
    name = 'term',
    value = 'std.error'
  )
  
  main_estm <- mvars %>% 
    filter(variable == exposure) %>% 
    select(variable, term, level, ref) %>% 
    left_join(beta_df, by = 'term') %>% 
    left_join(covb_df, by = 'term') %>% 
    mutate(pv_term = NA_real_, pv_ovrl = NA_real_)
  
  main_estm$pv_term[!main_estm$ref] <- get_term_pval(
    model = model, 
    variable = terms
  )
  
  # Overall p-value
  if(vec_size(terms) > 1L){
    
    main_estm %<>% mutate(
      pv_ovrl = get_ovrl_pval(model = model, variable = exposure)
    )
    
  }
  
  if (fill_models) {
    supp_estm <-
      fill_estimates(
        model = model,
        exposure = exposure,
        control = rhs.vars(formula),
        fitter = switch(engine, 'glm' = glm, 'gee' = gee_no_ID),
        mvars = mvars,
        family = model$family$family,
        data = data
      )
  }
  
  apri_model <- list(
    mcall = formula,
    model = NULL,
    betas = coef(model),
    covbs = vcov(model),
    estms = bind_rows(main_estm, supp_estm)
  )
  
  if(keep_models){
    apri_model$model = model
    apri_model$model$call = formula
  } 
  
  apri_model
  
}

vcov.geeglm <- function(object, ...) {
  
  sobj <- summary(object)
  covb <- sobj$cov.scaled
  
  covb_names <- names(coef(object))
  colnames(covb) <- covb_names
  rownames(covb) <- covb_names
  
  covb
  
}

  
