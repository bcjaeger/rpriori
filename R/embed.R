
#' initialize an a priori analysis with a hypothesis.
#' 
#' @param formula a two-sided formula. Outcomes go on the
#'   left hand side of the formula, and exposures on the right.
#'
#' @note Control variables do not belong here. Instead, they 
#'   are specified in the \code{\link{mspec_new}}, 
#'   \code{\link{mspec_add}}, \code{\link{mspec_rmv}}, and
#'   \code{\link{mspec_sub}} functions.
#'   
#' @export

hypothesize_that <- function(formula){
  
  if (is.one.sided(formula)) {
    msg <- "formula must have an outcome, i.e. outcome ~ exposure"
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
  
  output <- expand.grid(
    outcome = formula_lhs,
    exposure = rhs.vars(formula)
  ) %>% 
    mutate(formula = paste(outcome, exposure, sep = ' ~ ')) %>% 
    as_tibble()
  
  attr(output, 'main_hypothesis') = formula
  
  output
  
}


#' embed a set of model specifications into a primary hypothesis.
#' @param ... Model specification objects created using \link{mspec_new}, 
#'  \link{mspec_add}, \link{mspec_sub}, etc. (Control variables do belong
#'  here).
#' @export

embed_mspecs <- function(hypothesis, ..., verbose = FALSE) {
  
  control <- list(...)
  
  model_names <- map_chr(control, ~.x$name)
  
  formulas <- hypothesis %>% 
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
  
  attr(output, 'main_hypothesis') = attr(hypothesis, 'main_hypothesis')
  
  attr(output, 'model_description') = map_chr(
    .x = control, 
    .f = mspec_describe,
    verbose = verbose
  )
  
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
  
  using_mi <- class(data)[1] %in% c('list', 'mild') 
  
  if(using_mi){
    data %<>% bind_rows(.id = 'imputation_id')
  }
  
  if(!vec_is_empty(names(.dots))){
    for(i in names(.dots)){
      attr(data[[i]], 'label') <- .dots[[i]][1L]
      
      if(vec_size(.dots[[i]]) > 1L){
        attr(data[[i]], 'unit') <- .dots[[i]][2L]
      }
      
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
  ) %>% 
    map(.f = as_tibble)
  
  class(output) <- "apriori"
  
  attr(output, 'main_hypothesis') = attr(mspecs, 'main_hypothesis')
  attr(output, 'model_description') = attr(mspecs, 'model_description')
  attr(output, 'multiple_imputation') = using_mi
  
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
      map(~ attr(data[[.x]], 'unit') %||% NA_character_ ) %>% 
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
  
  engine_args <- list(...)
  
  if(!engine %in% c('lm', 'glm', 'gee', 'cph')){
    stop(
      glue(
        "{engine} engine is unrecognized. Use lm, glm, gee, or cph"
      ),
      call. = FALSE
    )
  }
  
  if(engine %in% c('glm','gee')) {
    
    family_specified <- 'family' %in% names(engine_args)
    
    if(!family_specified){
      msg <- "family must be specified for glm and geeglm objects"
      stop(msg, call. = FALSE)
    }
    
    family <- engine_args$family
    
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
            {family$family} models. Change {outcomes} to numeric."),
            call. = FALSE
          )
          
        }
        
      }
      
    }
    
  }
  
  output <- map(
    .x = object$analysis$formula,
    .f = fit_fun,
    data = object$fit_data,
    mvars = object$var_data,
    engine = engine,
    engine_args = engine_args,
    fill_models = fill_models,
    keep_models = keep_models,
    use_mi = attr(object, 'multiple_imputation')
  ) %>% 
    map(.f = function(mdl){
      class(mdl) <- "apri_fit"  
      mdl
    })
  
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

fit_fun <- function(
  formula,
  data,
  mvars,
  use_mi,
  engine,
  engine_args = list(),
  fill_models,
  keep_models
) {
  
  exposure = attr(formula, 'exposure')
  
  fitter <- switch(
    engine,
    'lm' = lm,
    'glm' = glm,
    'cph' = coxph,
    'gee' = gee_no_ID
  )
  
  fitter_args = engine_args
  fitter_args$formula = formula
  
  if(use_mi){
    
    model <- data %>% 
      split(.$imputation_id) %>% 
      map(
        .f = function(df){
          fitter_args$data = df
          do.call(fitter, args = fitter_args)
        }  
      )
    
    pooled_model <- testEstimates(model = model)
    
    beta_df <- pooled_model$estimates %>% 
      as_tibble(rownames = 'term') %>% 
      select(term, estimate = Estimate)
    
    betas <- pull(beta_df, estimate)
    
    covb_df <- pooled_model$estimates %>% 
      as_tibble(rownames = 'term') %>% 
      select(term, std.error = Std.Error)
    
    covbs <- map(model, vcov) %>% 
      reduce(`+`) %>% 
      divide_by(length(model))
    
  } else {
    
    fitter_args$data = data
    model <- do.call(fitter, args = fitter_args)
    
    betas <- coef(model)
    covbs <- vcov(model)
    
    beta_df <- enframe(
      x = betas, 
      name = 'term', 
      value = 'estimate'
    )
    
    covb_df <- enframe(
      x = sqrt(diag(covbs)), 
      name = 'term',
      value = 'std.error'
    )
    
  }
  
  terms <- mvars %>%
    filter(variable == exposure, !ref) %>%
    pull(term)
  
  main_estm <- mvars %>% 
    filter(variable == exposure) %>% 
    select(variable, term, level, ref) %>% 
    left_join(beta_df, by = 'term') %>% 
    left_join(covb_df, by = 'term') %>% 
    mutate(pv_term = NA_real_, pv_ovrl = NA_real_)
  
  main_estm$pv_term[!main_estm$ref] <- if(use_mi){
    get_term_pval_mi(model = model, variable = terms)
  } else {
    get_term_pval(model = model, variable = terms)
  }
    
  # Overall p-value
  if(vec_size(terms) > 1L){
    
    if(use_mi){
      main_estm %<>% 
        mutate(
          pv_ovrl = get_ovrl_pval_mi(
            model = model, 
            variable = exposure,
            data = data, 
            fitter = fitter, 
            fitter_args = fitter_args
          )
        )
    } else {
      main_estm %<>% 
        mutate(
          pv_ovrl = get_ovrl_pval(
            model = model, 
            variable = exposure
          )
        )
    }
    
    
    
  }
  
  if (fill_models) {
    supp_estm <-
      fill_estimates(
        model = model,
        data = data,
        use_mi = use_mi,
        exposure = exposure,
        control = rhs.vars(formula),
        fitter = fitter,
        fitter_args = fitter_args,
        mvars = mvars
      )
  }
  
  apri_model <- list(
    mcall = formula,
    model = NULL,
    betas = betas,
    covbs = covbs,
    estms = bind_rows(main_estm, supp_estm)
  )
  
  if(keep_models){
    apri_model$model = model
    apri_model$model$call = formula
  } 
  
  apri_model
  
}

fill_estimates <- function(
  model, 
  data,
  use_mi,
  exposure, 
  control,  
  mvars,
  fitter,
  fitter_args = list()
){
  
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
  
  .frmla <- if(use_mi) {
    as.character(model[[1]]$formula)
  } else {
    as.character(model$formula)
  }
  
  for(v in variables_to_add){
    
    v_is_control_variable <- v %in% control
    
    # If v is a control variable, it's already in the model,
    # so we just need to remove the exposure variable. Otherwise,
    # we need to remove the exposure variable and also include v.
    if(v_is_control_variable) {
      
      frmla_string <- "{.frmla} - {exposure}"
      
    } else {
      
      frmla_string <- "{.frmla} + {v} - {exposure}"
      
    }
    
    fitter_args$formula <- as.formula(glue(frmla_string))
    
    # Fit a new model that does not contain the exposure variable
    # (why?) to get the estimated effect of this variable (v) in
    # the same model that we estimated the effect of the main
    # exposure variable in.
    
    if(use_mi){
      
      .model <- map(
        .x = split(data, data$imputation_id), 
        .f = function(df){
          .args = fitter_args
          .args$data = df
          do.call(fitter, args = .args)
        }
      )
      
    } else {
      
      fitter_args$data <- data
      .model <- do.call(fitter, args=fitter_args)
      
    }
    
    vv <- output %>% 
      filter(variable == v, !ref) %>% 
      pull(term)
    
    pv_ovrl <- pv_term <- if(use_mi){
      get_term_pval_mi(model = .model, variable = vv)
    } else {
      get_term_pval(model = .model, variable = vv)
    }
      
    
    # Overall p-value
    if(vec_size(vv) > 1L){
      
      if(use_mi){
        
        pv_ovrl <- get_ovrl_pval_mi(
          model = .model, 
          variable = v, 
          data = data, 
          fitter = fitter, 
          fitter_args = .dots
        )
        
      } else {
        
        pv_ovrl <- get_ovrl_pval(model = .model, variable = v)

      }
      
      
    } 
    
    betas <- if(use_mi){
      testEstimates(.model) %>% 
        use_series('estimates') %>% 
        as_tibble(rownames = 'term') %>% 
        select(term, estimate = Estimate) %>% 
        filter(term %in% vv) %>% 
        deframe()
    } else {
      coef(.model)[vv]
    }
    
    covbs <- if(use_mi){
      testEstimates(.model) %>% 
        use_series('estimates') %>% 
        as_tibble(rownames = 'term') %>% 
        select(term, std.error = Std.Error) %>% 
        filter(term %in% vv) %>% 
        deframe()
    } else {
      sqrt(diag(vcov(.model)))[vv]
    }

    output$estimate[output$term %in% vv] <- betas
    output$std.error[output$term %in% vv] <- covbs
    output$pv_term[output$term %in% vv] <- pv_term
    output$pv_ovrl[output$term %in% vv] <- pv_ovrl
    
  }
  
  output
  
}


#' estimate the variance-covariance matrix of regression coefficients
#'   from a generalized linear model fitted with generalized estimating
#'   equations. 
#' @param object an object of class `geeglm`.
#' @param ... additional arguments passed along to other functions. 
#' @export

vcov.geeglm <- function(object, ...) {
  
  sobj <- summary(object)
  covb <- sobj$cov.scaled
  
  nms <- names(coef(object))
  colnames(covb) <- rownames(covb) <- nms
  
  covb
  
}

