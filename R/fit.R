



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

# data = apri
# effect = 'age'
# ci = 0.95
# transform = exp
# reference_label = '1 (Ref)'

hoist_effect <- function(
  data, 
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
  
  mvars <- data$var_data
  model_variables <- unique(mvars$variable)
  
  mdl_eff <- vars_pull(model_variables, !!enquo(effect))
  
  # map user defined transform function to fun if needed
  fun <- transform %||% function(x) return(x)
  
  hoisted_values <- map_dfr(
    .x = data$analysis$fit,
    .f = function(mdl){
        filter(mdl$estms, variable == mdl_eff)
    },
    .id = 'id'
  )
  
  if(!is.null(ci)) {
    
    hoisted_values %<>%
      mutate(
        estimate = fmt_effect(
          effect = estimate,
          std.error = std.error,
          transform = fun,
          conf_level = ci,
          reference_index = which(ref),
          reference_label = reference_label
        )
      )
    
  } else {
    
    hoisted_values %<>% 
      mutate(
        estimate = if_else(ref, 0, estimate),
        estimate = fun(estimate)
      )
    
  }
  
  if(vec_size(unique(hoisted_values$term)) > 1){
    
    hoisted_values %<>% 
      select(id, level, estimate) %>% 
      spread(level, estimate) %>% 
      select(-id)
    
  } else {
    
    hoisted_values %<>%
      select(estimate) %>% 
      set_names(mdl_eff)
    
  }
  
  type_of_mdl_eff <- mvars %>% 
    filter(variable == mdl_eff) %>% 
    slice(1) %>% 
    pluck("type")
  
  if(type_of_mdl_eff %in% c('ordered', 'factor')){
    
    select_cols <- mvars %>% 
      filter(variable == mdl_eff) %>% 
      pull(level)
    
    hoisted_values %<>% select(!!!select_cols)
    
  }
  
  data$analysis %<>% bind_cols(hoisted_values)
  
  data
  
}


#' summarize an `apri_fit` object.
#' 
#' @param object an object of class `apri_fit`
#' @export

summary.apri_fit <- function(object){
  
  sm <- object$estms 
  
  ref_rows <- which(sm$ref)
  
  nmr_cols <- map_chr(sm, class) %>% 
    enframe() %>% 
    filter(value == 'numeric') %>% 
    pull(name)
  
  pv_indx <- grepl(pattern = 'pv_', x = nmr_cols)
  
  pv_cols <- nmr_cols[pv_indx]
  nmr_cols <- nmr_cols[-pv_indx]
  
  sm[ref_rows, nmr_cols] <- 0
  sm[ref_rows, pv_cols] <- NA_real_
  
  sm %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate_if(is.factor, fct_inorder)

}
