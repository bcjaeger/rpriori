

get_ovrl_pval <- function(model, variable){
  anova(model, test = "Chisq") %>% 
    as_tibble(rownames = 'model_variable') %>% 
    filter(model_variable == variable) %>% 
    pull(ncol(.))
}

get_ovrl_pval_mi <- function(
  model, 
  variable,
  data, 
  fitter, 
  fitter_args = list()
){
  
  full_model <- testEstimates(model)
  full_frmla <- as.character(model[[1]]$formula)
  rdcd_frmla <- as.formula(glue("{full_frmla} - {variable}"))
  rdcd_model <- data %>% 
    split(.$imputation_id) %>% 
    map(
    .f = function(df){
      args = fitter_args
      args$data <- df
      args$formula <- rdcd_frmla
      do.call(fitter, args = args)
    }
  )
  
  testModels(model = model, null.model = rdcd_model) %>% 
    use_series('test') %>% 
    as_tibble() %>% 
    select(-RIV) %>% 
    pull(ncol(.))
  
}


# get_ovrl_pval.glm <- function(model, variable){
#   anova(model, test = "LRT") %>% 
#     as_tibble(rownames = 'model_variable') %>% 
#     filter(model_variable == variable) %>% 
#     pluck("Pr(>Chi)")
# }
# 
# get_ovrl_pval.geeglm <- function(model, variable){
#   anova(model, test = "LRT") %>% 
#     as_tibble(rownames = 'model_variable') %>% 
#     filter(model_variable == variable) %>% 
#     pluck("P(>|Chi|)")
# }
# 
# 
# get_ovrl_pval.coxph <- function(model, variable){
#   anova(model, test = "Chisq") %>% 
#     as_tibble(rownames = 'model_variable') %>% 
#     filter(model_variable == variable) %>% 
#     pluck("Pr(>|Chi|)")
# }


get_term_pval <- function(model, variable){
  summary(model) %>% 
    use_series("coefficients") %>% 
    as_tibble(rownames = 'model_variable') %>% 
    filter(model_variable %in% variable) %>% 
    pull(ncol(.))
}

get_term_pval_mi <- function(model, variable){
  testEstimates(model) %>% 
    use_series("estimates") %>% 
    as_tibble(rownames = 'model_variable') %>% 
    filter(model_variable %in% variable) %>%
    select(-RIV, - FMI) %>% 
    pull(ncol(.))
}

# get_term_pval.glm <- function(model, variable){
#   summary(model) %>% 
#     use_series("coefficients") %>% 
#     as_tibble(rownames = 'model_variable') %>% 
#     filter(model_variable %in% variable) %>% 
#     pull(ncol(.))
# }
# 
# get_term_pval.geeglm <- function(model, variable){
#   summary(model) %>% 
#     use_series("coefficients") %>% 
#     as_tibble(rownames = 'model_variable') %>% 
#     filter(model_variable %in% variable) %>% 
#     pluck("Pr(>|W|)")
# }
# 
# get_term_pval.coxph <- function(model, variable){
#   summary(model) %>% 
#     use_series("coefficients") %>% 
#     as_tibble(rownames = 'model_variable') %>% 
#     filter(model_variable %in% variable) %>% 
#     pluck("Pr(>|z|)")
# }