get_ovrl_pval <- function(model, variable){
  UseMethod("get_ovrl_pval")
}


get_ovrl_pval.glm <- function(model, variable){
  anova(model, test = "LRT") %>% 
    as_tibble(rownames = 'model_variable') %>% 
    filter(model_variable == variable) %>% 
    pluck("Pr(>Chi)")
}

get_ovrl_pval.geeglm <- function(model, variable){
  anova(model, test = "LRT") %>% 
    as_tibble(rownames = 'model_variable') %>% 
    filter(model_variable == variable) %>% 
    pluck("P(>|Chi|)")
}


get_term_pval <- function(model, variable){
  UseMethod("get_term_pval")
}

get_term_pval.glm <- function(model, variable){
  summary(model) %>% 
    use_series("coefficients") %>% 
    as_tibble(rownames = 'model_variable') %>% 
    filter(model_variable %in% variable) %>% 
    pluck("Pr(>|z|)")
}

get_term_pval.geeglm <- function(model, variable){
  summary(model) %>% 
    use_series("coefficients") %>% 
    as_tibble(rownames = 'model_variable') %>% 
    filter(model_variable %in% variable) %>% 
    pluck("Pr(>|W|)")
}