

#' A wrapper function to fit various types of models based on apriori 
#'   specifications. This function should be applied within the 
#'   context of the `mutate()` function in the `dplyr` package.
#' 
#' @param formulas a list of formula objects
#' @param data a dataframe containing the variables in the model.
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
#' @param light_output (T/F) if TRUE, the model output will 
#'   be abridged to a list containing regression point 
#'   estimates and covariance estimates. If FALSE, the full
#'   model output will be returned.
#' @param ... Arguments to be passed into the specified engine function.
#' @export

# formulas = analysis$formula
# data = drop_na(titanic)
# family = binomial(link = 'logit')
# engine = 'glm'

fit_apri <- function(
  formulas, 
  data, 
  engine = 'lm', 
  fill_models = TRUE,
  light_output = FALSE,
  ...
) {
  
  .mvars <- get_model_variables(formulas, data)
  
  mrefs <- .mvars %>% 
    filter(type %in% c('factor', 'ordered')) %>% 
    group_by(variable) %>% 
    slice(1) %>% 
    pull(term)
  
  .mvars %<>% mutate(ref = term %in% mrefs)
  
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
        
        outcome <- as.character(lhs(formula))
        
        if(inherits(data[[outcome]],'factor')){
          
          data %<>% mutate_at(.vars = outcome, .funs = ~as.numeric(.x)-1)
          
          warning(
            glue("geeglm expects numeric outcome variables, \\
            even for binomial models. Change {outcome} to numeric \\
            to avoid seeing this warning message"),
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
      'gee' = gee_apri,
      'cph' = cph_apri,
      stop(glue("{engine} engine is unrecognized. \\
        Please use lm, glm, gee, or cph"),
        call. = FALSE
      )
    )
  }
  
  output <- map(
    .x = formulas,
    .f = fit_fun,
    data = data,
    mvars = .mvars,
    fill_models = fill_models,
    light_output = light_output,
    ...
  ) %>% 
    map(
      .f = function(mdl){
        
        mdl$apri_data <- .mvars
        class(mdl) <- "apri_fit"  
        mdl
        
      }
    )
  
}

fill_estimates <- function(
  model, 
  exposure, 
  fitter, 
  apri_model, 
  apri_data, 
  ...
){
  
  .dots <- list(...)
  
  variables = unique(apri_data$variable)
  
  variables_to_add <- setdiff(
    x = variables,
    y = rhs.vars(model$formula)
  )
  
  apri_ref_coded <- apri_data %>% 
    group_by(variable) %>% 
    nest() %>% 
    mutate(
      data = map(
        .x = data,
        .f = function(df){
          if(nrow(df) == 1) return(df) else return(df[-1, ])
        }
      )
    ) %>% 
    unnest() %>% 
    filter(variable %in% variables_to_add)
  
  n_to_add <- nrow(apri_ref_coded)
  
  new_betas <- apri_model$betas
  
  new_covbs <- matrix(
    data = NA_real_, 
    nrow = nrow(apri_model$covbs) + n_to_add,
    ncol = ncol(apri_model$covbs) + n_to_add
  ) %>% 
    set_colnames(
      c(colnames(apri_model$covbs), apri_ref_coded$term)
    ) %>% 
    set_rownames(
      c(rownames(apri_model$covbs), apri_ref_coded$term)
    )
  
  new_covbs[colnames(apri_model$covbs), rownames(apri_model$covbs)] <- 
    apri_model$covbs
  
  .frmla <- as.character(model$formula)
  
  for(v in variables_to_add){
    
    .dots$formula <- as.formula(glue("{.frmla} + {v} - {exposure}"))
    .model <- do.call(fitter, args=.dots)
    .covbs <- vcov(.model)
    
    vv <- apri_ref_coded %>% 
      filter(variable == v) %>% 
      pull(term)
    
    new_betas %<>% c(coef(.model)[vv])
    new_covbs[rownames(.covbs), colnames(.covbs)] <- .covbs
    
  }
  
  list(
    betas = new_betas,
    covbs = new_covbs
  )
  
  
}


glm_apri <- function(
  formula,
  data,
  mvars,
  fill_models,
  light_output,
  ...
) {
  
  exposure = attr(formula, 'exposure')
  
  model <- glm(
    formula = formula, 
    data = data,
    ...
  )
  
  apri_model <- list(
    mcall = formula,
    model = NULL,
    betas = coef(model),
    covbs = vcov(model)
  )
  
  if (fill_models) {
    apri_model <-
      fill_estimates(
        model = model,
        exposure = exposure,
        fitter = stats::glm,
        apri_model = apri_model,
        apri_data = mvars,
        family = model$family$family,
        data = data
      )
  }
  
  if(!light_output){
    apri_model$model = model
  } 
  
  apri_model

}

vcov.geeglm <- function(object, ...) {
  sobj <- summary(object)
  return(sobj$cov.scaled)
}

# formula = analysis$formula[[1]]
# data = drop_na(titanic)
# .dotdots <- list(family = 'binomial')

gee_apri <- function(
  formula,
  data,
  light_output,
  ...
) {
  
  .dotdots         <- list(...)
  .dotdots$id      <- 1:nrow(data)
  .dotdots$data    <- data
  .dotdots$formula <- formula
  
  output <- do.call(
    geeglm,
    args = .dotdots
  )
  
  apri_model <- list(
    mcall = formula,
    betas = coef(output),
    covbs = set_colnames(vcov(output), names(coef_output))
  )
  
  if(fill_models){
    apri_model <- fill_estimates(output, apri_model, mvars)
  }
  
  if(light_output){
    return(apri_model)
  }
  
  output$apri_model <- apri_model
  output$call <- formula
  output
  
}

lm_apri <- function(
  formula,
  data,
  light_output, 
  ...
) {
  
  output <- lm(
    formula = formula, 
    data = data, 
    ...
  )
  
  apri_model <- list(
    mcall = formula,
    betas = coef(output),
    covbs = vcov(output)
  )
  
  if(fill_models){
    apri_model <- fill_estimates(output, apri_model, mvars)
  }
  
  if(light_output){
    return(apri_model)
  }
  
  output$apri_model <- apri_model
  output$call <- formula
  output
  
}

cph_apri <- function(
  formula,
  data,
  light_output, 
  ...
) {
  
  output <- coxph(
    formula = formula, 
    data = data, 
    ...
  )
  
  apri_model <- list(
    mcall = formula,
    betas = coef(output),
    covbs = vcov(output)
  )
  
  if(fill_models){
    apri_model <- fill_estimates(output, apri_model, mvars)
  }
  
  if(light_output){
    return(apri_model)
  }
  
  output$apri_model <- apri_model
  output$call <- formula
  output

}

#' Create a dataset with variable names, types, and 
#'   the corresponding terms related to that variable 
#'   in statistical models. 
#'   
#' @param formulas a list of formulas
#' @param data the dataframe that will be used to fit models with `formulas`

get_model_variables <- function(formulas, data){
  
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
  
  bind_rows(nmrc_variables, fctr_variables) %>% 
    left_join(analysis_variables, by = 'variable')
  
}

#' get the estimated effects for a designated exposure variable
#'   from a set of statistical models fitted using apriori specs.
#' @param data a dataset with containing a list of models
#' @param models a column in dataset containing a list of models
#' @param effect (character or literal) the effect to hoist
#' @param ci a numeric value between 0 and 1 indicating desired 
#'   confidence level for interval estimates. If left unspecified,
#'   confidence intervals will not be included in the output.
#' @param fun a function that will be applied to the model's
#'   estimated regression coefficients. For example, it is common
#'   to exponentiate regression coefficients from logistic and
#'   proportional hazards models to present odds and hazard ratios,
#'   respectively. 
#' @export

# data = analysis
# effect = 'pclass'
# models = 'fit'
# ci = 0.95
# transform = exp
# reference_label = '1 (Ref)'

hoist_effect <- function(
  data, 
  models, 
  effect, 
  ci = NULL, 
  transform = NULL,
  reference_label = "1 (reference)"
){
  
  # Check on the confidence interval coverage value
  if(!is.null(ci)){
    
    ci_okay <- ci < 1 && ci > 0
    
    if(!ci_okay){
      msg <- "confidence interval (ci) should be between 0 and 1"
      stop(msg, call. = FALSE)
    }
    
  }
  
  model_col <- vars_pull(names(data), !!enquo(models))
  
  apri_data <- data[[model_col]][[1]][["apri_data"]]
  
  model_variables <- unique(apri_data$variable)
  
  mdl_eff <- vars_pull(
    vars = model_variables, 
    var = !!enquo(effect)
  )
  
  # map user defined transform function to fun if needed
  fun <- transform %||% function(x) return(x)
  
  # mdl = data$fits[[1]]
  
  effect_data <- map_dfr(
    .x = data[[model_col]],
    .f = function(mdl){
      
      betas <- enframe(mdl$betas, name = 'term', value = 'eff')
      covbs <- enframe(diag(mdl$covbs), name = 'term', value = 'err')
      
      effect_estm <- apri_data %>%
        filter(variable %in% mdl_eff) %>%
        left_join(betas, by = 'term') %>%
        left_join(covbs, by = 'term')
      
      if(!is.null(ci)) {
        
        effect_estm %<>%
          mutate(
            estimate = fmt_effect(
              effect = eff,
              error = err,
              transform = fun,
              conf_level = ci,
              reference_index = which(ref),
              reference_label = reference_label
            )
          )
        
      } else {
        
        effect_estm %<>% 
          mutate(
            estimate = if_else(ref, 0, eff),
            estimate = fun(estimate)
          )
        
      }
      
      
      if(nrow(effect_estm) > 1){
        effect_estm %<>% 
          select(level, estimate) %>% 
          spread(level, estimate)
      } else {
        effect_estm %<>%
          select(estimate) %>% 
          set_names(mdl_eff)
      }
      
      effect_estm
      
    }
  )
  
  type_of_mdl_eff <- apri_data %>% 
    filter(variable == mdl_eff) %>% 
    slice(1) %>% 
    pluck("type")
  
  if(type_of_mdl_eff %in% c('ordered', 'factor')){
    
    select_cols <- apri_data %>% 
      filter(variable == mdl_eff) %>% 
      pull(level)
    
    effect_data %<>% select(!!!select_cols)
    
  }
  
  bind_cols(data, effect_data)
  
}

get_apri_data <- function(model){
  
  if(inherits(model, 'apri_model')){
    return(model$apri_data)
  } else {
    return(model$apri_model$apri_data)
  }
  
}

#' summarize an `apri_fit` object.
#' 
#' @param object an object of class `apri_fit`
#' @param ... additional arguments affecting the summary values.
#' @export

summary.apri_fit <- function(object, ...){
  
  betas <- enframe(object$betas, name = 'term', value = 'estimate')
  covbs <- enframe(diag(object$covbs), name = 'term', value = 'std.error')
  
  object$apri_data %>% 
    left_join(betas, by = 'term') %>% 
    left_join(covbs, by = 'term') %>% 
    mutate_at(
      .vars = vars(estimate, std.error),
      .funs = ~ {
        .x[ref] = 0
        .x
      }
    )
  
}
