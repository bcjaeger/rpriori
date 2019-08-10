

test_that(
  "multiplication works", 
  {
    expect_equal(2 * 2, 4)
  }
)


# tst = geeglm(
#   formula = survived ~ sex + age ,
#   family = poisson(link = 'log'),
#   data = ttnc,
#   id = 1:nrow(ttnc)
# )
# 
# anova(tst)
# 
# summary(tst)
# 
# apri$analysis$fit[[2]] %>% 
#   use_series("estms") %>% 
#   mutate(
#     tbl_val = fmt_effect(
#       effect = estimate,
#       transform = exp,
#       std.error = std.error, 
#       conf_level = 0.95
#     ) 
#   )
# 
# library(broom)
# 
# coef(tst) %>% 
#   enframe() %>% 
#   bind_cols(confint_tidy(tst)) %>% 
#   mutate_at(vars(2:4), ~exp(.x))
# 
# 
# 
# 
# 
# exp(-0.969568 +c(-1.96, 1.96) * 0.109443)
# 
# summary(tst)
