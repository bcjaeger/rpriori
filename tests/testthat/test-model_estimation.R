

test_that(
  "Proportional hazard models are fit correctly (no imputation)", 
  {
    
    library(rpriori)
    library(tidyverse)
    
    data <- droplevels(drop_na(pbc)) %>% 
      mutate(trt = relevel(trt, ref = 'Placebo'))
    
    m0 <- mspec_new()
    m1 <- mspec_add(m0, name = "Model 1", age, sex)
    m2 <- mspec_add(m1, name = "Model 2", stage, ascites, hepato, spiders, edema)
    m3 <- mspec_add(m2, name = "Model 3", bili, chol, albumin, copper, alk.phos)
    
    control <- list(m1, m2, m3)
    
    main_hypothesis <- Surv(time, status) ~ trt
    
    tst_model <- coxph(
      Surv(time, status) ~ trt + age + sex,
      data = data
    )
    
    modl_val <- summary(tst_model) %>% 
      use_series('coefficients') %>% 
      as_tibble(rownames = 'variable') %>% 
      filter(variable == 'trtD-penicillmain') %>% 
      rename(estimate = coef, std.error = `se(coef)`) %>% 
      mutate(
        tst_val = fmt_effect(
          effect = estimate, 
          std.error = std.error, 
          conf_level = 0.95, 
          transform = exp)
      ) %>% 
      pull(tst_val)
    
    
    apri_val <- main_hypothesis %>% 
      embed_mspecs(m1, m2, m3) %>% 
      embed_data(
        data = data, 
        age = "Age",
        sex = "Gender",
        stage = "Histologic stage of disease",
        trt = "Treatment",
        ascites = "Ascites"
      ) %>% 
      embed_fits(engine = 'cph', keep_models = TRUE) %>% 
      hoist_effect(trt, ci = 0.95, transform = exp) %>% 
      pull_analysis() %>% 
      filter(name == 'Model 1') %>% 
      pull(`D-penicillmain`)
    
    expect_equal(modl_val, apri_val)
  }
)

test_that(
  "Proportional hazard models are fit correctly (imputation)", 
  {
    
    library(rpriori)
    library(mice)
    library(tidyverse)
    
    dnames <- names(pbc)
    inames <- setdiff(dnames, 'trt')
    
    pmat <- matrix(
      data = 0, 
      ncol = length(dnames), 
      nrow = length(dnames)
    ) %>% 
      set_colnames(dnames) %>% 
      set_rownames(dnames)
    
    pmat[, inames] <- 1
    
    data <- mice(pbc, m = 3, predictorMatrix = pmat) %>% 
      mice::complete(action = 'all')
    
    
    m1 <- mspec_new(name = "Model 1", age, sex)
    m2 <- mspec_add(m1, name = "Model 2", stage, ascites, hepato, spiders, edema)
    m3 <- mspec_add(m2, name = "Model 3", bili, chol, albumin, copper, alk.phos)
    
    main_hypothesis <- Surv(time, status) ~ trt
    
    object <- main_hypothesis %>% 
      embed_mspecs(m1, m2, m3) %>% 
      embed_data(
        data = data, 
        age = "Age",
        sex = "Gender",
        stage = "Histologic stage of disease",
        trt = "Treatment",
        ascites = "Ascites"
      ) %>% 
      embed_fits(engine = 'cph', keep_models = TRUE) 
    
    object$analysis$fit[[1]]
    
    apri_val <- object %>% 
      hoist_effect(sex, ci = 0.95, transform = exp) %>% 
      pull_analysis() %>%  
      select(-c(exposure, formula, fit)) %>% 
      filter(name == 'Model 1') %>% 
      pull(Female)
    
    mice_object <- mice(pbc, m = 5, predictorMatrix = pmat)
    mdl <- with(mice_object, coxph(Surv(time, status) ~ age + sex))
    
    mice_val <- suppressWarnings(pool(mdl)) %>% 
      summary() %>% 
      as_tibble(rownames = 'variable') %>% 
      filter(variable == 'sexFemale') %>% 
      mutate(
        tst_val = fmt_effect(
          effect = estimate, 
          std.error = std.error, 
          conf_level = 0.95, 
          transform = exp
        )
      ) %>% 
      pull(tst_val)
    
    expect_equal(mice_val, apri_val)
  }
)

test_that(
  "geeglm models are fit correctly (imputation)", 
  {
    
    library(rpriori)
    library(mice)
    library(tidyverse)
    library(magrittr)
    
    titanic$survived %<>% as.numeric() %>% subtract(1)
    
    data <- mice(titanic, m = 10) %>% 
      mice::complete(action = 'all')
    
    m1 <- mspec_new(name = "Model 1", age)
    m2 <- mspec_add(m1, name = "Model 2", sex, sibsp)
    m3 <- mspec_sub(m2, name = "Model 3", sibsp = parch)
    
    main_hypothesis <- survived ~ pclass
    
    object <- main_hypothesis %>% 
      embed_mspecs(m1, m2, m3) %>% 
      embed_data(data = data) %>% 
      embed_fits(engine = 'gee', family = 'poisson', keep_models = TRUE) 
    
    apri_val <- object %>%
      pull_analysis() %>%
      mutate(smry = map(fit, summary)) %>%
      select(name, outcome, smry) %>%
      unnest() %>%
      mutate(
        tbl_val = fmt_effect(
          effect = estimate,
          std.error = std.error,
          transform = exp,
          conf_level = 0.95,
          reference_index = which(ref)
        )
      ) %>% 
      select(name, variable, ref, term, tbl_val) %>% 
      spread(name, tbl_val) %>% 
      filter(!ref, variable == 'pclass') %>% 
      select(term, `Model 3`) %>% 
      deframe()
    
    mdl <- map(
      .x = data,
      .f = ~geeglm(
        survived ~ pclass + age + sex + parch, 
        family = 'poisson', 
        data = .x,
        id = 1:nrow(.x)
      )
    )
    
    mice_val <- mitml::testEstimates(mdl) %>% 
      use_series('estimates') %>% 
      as_tibble(rownames = 'variable') %>%
      mutate(tbl_val = fmt_effect(
        effect = Estimate,
        std.error = Std.Error,
        transform = exp,
        conf_level = 0.95
      )) %>% 
      filter(variable %in% c('pclassSecond','pclassThird')) %>% 
      select(variable, tbl_val) %>% 
      deframe()
    
    expect_equal(mice_val, apri_val)
  }
)

