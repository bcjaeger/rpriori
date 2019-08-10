
## code to prepare `pbc` dataset goes here

library(survival)
library(tidyverse)

data("pbc",package='survival')

pbc$status[pbc$status>=1]=pbc$status[pbc$status>=1]-1

pbc$id=NULL

pbc$trt[is.na(pbc$trt)]=0

as_no_yes <- function(x){
  factor(x, labels = c("No","Yes"))
}

pbc %<>%
  as_tibble() %>%
  mutate(
    sex = fct_recode(sex, 'Female' = "f", 'Male' = "m"),
    trt = factor(
      x = trt,
      levels = 0:2,
      labels = c("Not randomized", "D-penicillmain", "Placebo")
    ),
    stage = factor(stage, labels = c("I", "II", "III", "IV")),
    edema = factor(
      edema,
      labels = c(
        "None",
        "Untreated or successfully treated",
        "Treatment resistant"
      )
    )
  ) %>%
  mutate_at(
    .vars = c('ascites', 'spiders', 'hepato'),
    .funs = as_no_yes
  )

usethis::use_data(pbc)

