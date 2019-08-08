
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rpriori

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/njtierney/rpriori.svg?branch=master)](https://travis-ci.org/njtierney/rpriori)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/njtierney/rpriori?branch=master&svg=true)](https://ci.appveyor.com/project/njtierney/rpriori)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `rpriori` is to provide a framework that simplifies apriori
hypothesis testing. In particular, `rpriori` focuses on building sets of
models that examine one primary hypothesis under several sets of
potential confounding variables.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("bcjaeger/rpriori")
```

# Logistic regression

Here are the packages we’ll use for this example:

``` r

library(rpriori)
library(magrittr)
library(glue)
library(tidyverse)
library(knitr)
library(kableExtra)
library(geepack)
```

Let’s use the `titanic` data to show how the pieces of `rpriori` fit
together. The first thing we need is a question that we can engage with
using a-priori models. Let’s investigate whether survival on the titanic
was associated with ticket class. We’ll start by initiating an empty
model.

``` r

# Make an unadjusted model
m0  <- spec_empty("Model 0")

# spec_describe(spec) is the same as print(spec)
spec_describe(m0)
#> [1] "Model 0 is unadjusted."
```

Now we can Model 1, a descendant of the unadjusted model.

``` r

# Model 1 includes adjustment for sex and age
m1  <- spec_add(m0, name = "Model 1", sex, age)

m1
#> [1] "Model 1 includes adjustment for sex and age."

# model 0 is automatically set as the parent since m0
# was supplied to spec_add.
m1$parent$name
#> [1] "Model 0"

# relation is automatically set by the spec_add function
m1$relation
#> [1] "add"
```

And now we can make descendants of model 1.

``` r

# Model 2a = model 1 + no. of siblings/spouses
m2a <- spec_add(m1, name = 'Model 2a', sibsp)

# Model 2b = model 1 + no. of parents/children
m2b <- spec_add(m1, name = 'Model 2b', parch)

# Model 3 = model 1, swapping out age for ticket fare
m3 <- spec_sub(m1, name = 'Model 3', age = fare)
```

What comes next? Our specifications are set, but they are separate. They
also haven’t been embedded into the main question of interest,
i.e. `survival ~ pclass`. We can pull these specifications together
into an object that encapsulates our main hypothesis with `spec_embed`

``` r

main_hypothesis <- survived ~ pclass

analysis <- main_hypothesis %>% 
  spec_embed(m0, m1, m2a, m2b, m3)
```

The next step is to fit models defined by the specifications in
`analysis`. Here, we use the `fit_apri()` function, which spans multiple
different modeling frameworks, including

1.  linear and generalized linear models (`engine = 'lm'` and `engine =
    'glm'`, respectively),

2.  generalized linear models fit with generalized estimating equations
    (`engine = 'gee'`), and

3.  Cox proportional hazards models (`engine = 'cph'`).

Additionally, `fit_apri()` is designed to work with lists of formulas,
making it pair well with the `mutate()` function in the `dplyr` package.

``` r

# formulas = analysis$formula
# data = drop_na(titanic)
# light_output = TRUE
# family = binomial(link = 'logit')
# engine = 'glm'

analysis %<>%
  mutate(
    fit = fit_apri(
      formulas = formula,
      data = drop_na(titanic),
      family = binomial(link = 'logit'),
      engine = 'glm',
      light_output = TRUE 
    )
  )
```

Now we can dig a little deeper into these models. How about we start by
peeking at the effects of our main exposure? To hoist these effects out
of the model objects, we use the `hoist_effect()` function. The main
input to this function is a data frame containing a list (or lists) of
model fits. If we want to get the effect of `pclass` from each model, we
just specify `effect = pclass`.

``` r

analysis %>% 
  hoist_effect(fit, effect = pclass)
#> # A tibble: 5 x 8
#>   name     outcome  exposure formula   fit        First Second Third
#>   <chr>    <chr>    <chr>    <list>    <list>     <dbl>  <dbl> <dbl>
#> 1 Model 0  survived pclass   <formula> <apri_fit>     0 -0.726 -1.80
#> 2 Model 1  survived pclass   <formula> <apri_fit>     0 -1.31  -2.58
#> 3 Model 2a survived pclass   <formula> <apri_fit>     0 -1.41  -2.65
#> 4 Model 2b survived pclass   <formula> <apri_fit>     0 -1.33  -2.58
#> 5 Model 3  survived pclass   <formula> <apri_fit>     0 -0.800 -1.83
```

Neat, but maybe not as easy to read as it could be. `hoist_effect` has a
few aesthetic helper inputs to make model output a little easier to
interpret. For example, instead of looking at estimates on the
log-scale, we can exponentiate them:

``` r

analysis %>% 
  hoist_effect(fit, effect = pclass, transform = exp)
#> # A tibble: 5 x 8
#>   name     outcome  exposure formula   fit        First Second  Third
#>   <chr>    <chr>    <chr>    <list>    <list>     <dbl>  <dbl>  <dbl>
#> 1 Model 0  survived pclass   <formula> <apri_fit>     1  0.484 0.165 
#> 2 Model 1  survived pclass   <formula> <apri_fit>     1  0.270 0.0757
#> 3 Model 2a survived pclass   <formula> <apri_fit>     1  0.243 0.0705
#> 4 Model 2b survived pclass   <formula> <apri_fit>     1  0.265 0.0757
#> 5 Model 3  survived pclass   <formula> <apri_fit>     1  0.449 0.160
```

Okay\! Now we have odds-ratios instead of regression coefficients, and
we can easily see that, according to the apriori models we specified,
ticket class has a pretty strong effect. A natural follow-up question is
how much uncertainty we have regarding those point estimates, and a
natural follow-up answer is to use the `ci` input argument of
`hoist_effect` like so:

``` r

analysis %>% 
  hoist_effect(fit, effect = pclass, ci = 0.95, transform = exp)
#> # A tibble: 5 x 8
#>   name    outcome  exposure formula  fit     First    Second     Third     
#>   <chr>   <chr>    <chr>    <list>   <list>  <chr>    <chr>      <chr>     
#> 1 Model 0 survived pclass   <formul~ <apri_~ 1 (refe~ 0.48 (0.4~ 0.17 (0.1~
#> 2 Model 1 survived pclass   <formul~ <apri_~ 1 (refe~ 0.27 (0.2~ 0.08 (0.0~
#> 3 Model ~ survived pclass   <formul~ <apri_~ 1 (refe~ 0.24 (0.2~ 0.07 (0.0~
#> 4 Model ~ survived pclass   <formul~ <apri_~ 1 (refe~ 0.27 (0.2~ 0.08 (0.0~
#> 5 Model 3 survived pclass   <formul~ <apri_~ 1 (refe~ 0.45 (0.3~ 0.16 (0.1~
```

This type of output can be passed right into your favorite table
function.

``` r

footer <- list(m0, m1, m2a, m2b, m3) %>% 
  map_chr(spec_describe)

analysis %>% 
  hoist_effect(fit, effect = pclass, ci = 0.95, transform = exp) %>% 
  select(name, First, Second, Third) %>% 
  kable(
    col.names = c(glue("Model{footnote_marker_symbol(1)}"), names(.)[-1]), 
    align = 'lccc', 
    format = 'html',
    escape = FALSE,
    caption = glue("Odds ratios (95% confidence limits) \\
      for survival on the titanic, stratified by ticket class")
  ) %>% 
  kable_styling(full_width = FALSE, bootstrap_options = c('striped')) %>% 
  add_header_above(header = c(" " = 1, "Ticket Class" = 3)) %>% 
  footnote(symbol = glue_collapse(footer, sep = ' '))
```

<img src="fig/kable_example1.png" width="100%" />

## Summarizing `rpriori` models

Many a-priori analyses aim to present a tabular summary of **all** the
variables used for analyses, i.e., not just the main exposure. For
example, a summary should show the relationship between `age` (a control
variable in `m1`) and `survival` as well as the relationship between
`sibsp` (is a control variable in `m2a`) and `survival`, for **all** of
the given model fits.

  - In order to obtain a regression coefficient for `age` in **all** of
    the models for this analysis, we need to re-fit Model 0 (the
    unadjusted model) as `survived ~ age` instead of `survived ~
    pclass`.

  - In order to obtain a regression coefficient for `sibsp` in **all**
    of the models for this analysis, we need to re-fit Model 0 (the
    unadjusted model) as `survived ~ sibsp` instead of `survived ~
    pclass`, but that’s not all\! We also need to re-fit Model 1 as
    `survived ~ sibsp + sex + age` instead of `survived ~ pclass + sex +
    age`.

This approach is standard for population science papers and it can also
creat very informative tables, but making those tables can get very
tedious very quickly. `rpriori` is designed to help generate and
tabulate these tables without having to fit dozens of models by hand.
All we need to do is apply the `summary` function to an `apri_fit` model
to get regression coefficients estimated by the recursive substitution
process outlined above:

``` r

# Summary of unadjusted relationships between survival
# and each of the variables used in this analysis.
summary(analysis$fit[[1]])
#> # A tibble: 9 x 7
#>   variable level  term         type    ref   estimate std.error
#>   <chr>    <chr>  <chr>        <chr>   <lgl>    <dbl>     <dbl>
#> 1 age      1 unit age          numeric FALSE  -0.0110   -0.0110
#> 2 sibsp    1 unit sibsp        integer FALSE  -0.0384   -0.0384
#> 3 parch    1 unit parch        integer FALSE   0.220     0.220 
#> 4 fare     1 unit fare         numeric FALSE   0.0160    0.0160
#> 5 pclass   First  pclassFirst  factor  TRUE    0         0     
#> 6 pclass   Second pclassSecond factor  FALSE  -0.726    -0.726 
#> 7 pclass   Third  pclassThird  factor  FALSE  -1.80     -1.80  
#> 8 sex      Male   sexMale      factor  TRUE    0         0     
#> 9 sex      Female sexFemale    factor  FALSE   2.48      2.48
```

These summaries are meant to be fairly easy to manipulate using `dplyr`
and other tools in the `tidyverse`. For example, the code below creates
a summary for all models used in the analysis, then applies `tidyverse`
functions to create a table with estimated odds ratios (95% confidence
intervals) for each variable in each of the five models we specified
apriori.

``` r

analysis %>% 
  mutate(mdl_smry = map(fit, summary)) %>% 
  select(name, mdl_smry) %>% 
  unnest() %>% 
  mutate(
    tbl_value = fmt_effect(
      effect = estimate,
      error = std.error,
      transform = exp,
      conf_level = 0.95,
      reference_index = which(ref),
      reference_label = '1 (ref)'
    )
  ) %>% 
  select(name, variable, level, tbl_value) %>% 
  spread(name, tbl_value)
#> # A tibble: 9 x 7
#>   variable level  `Model 0`   `Model 1`  `Model 2a`  `Model 2b`  `Model 3` 
#>   <chr>    <chr>  <chr>       <chr>      <chr>       <chr>       <chr>     
#> 1 age      1 unit 0.99 (1.01~ 0.96 (1.0~ 0.96 (1.04~ 0.96 (1.04~ 0.99 (1.0~
#> 2 fare     1 unit 1.02 (0.98~ 1.01 (0.9~ 1.02 (0.98~ 1.02 (0.99~ 1.00 (1.0~
#> 3 parch    1 unit 1.25 (0.81~ 0.86 (1.1~ 0.94 (1.07~ 0.86 (1.15~ 0.75 (1.3~
#> 4 pclass   First  1 (ref)     1 (ref)    1 (ref)     1 (ref)     1 (ref)   
#> 5 pclass   Second 0.48 (2.01~ 0.27 (3.5~ 0.24 (3.89~ 0.27 (3.58~ 0.45 (2.1~
#> 6 pclass   Third  0.17 (5.63~ 0.08 (11.~ 0.07 (12.8~ 0.08 (11.9~ 0.16 (5.8~
#> 7 sex      Female 11.9 (0.09~ 12.5 (0.0~ 13.8 (0.08~ 13.3 (0.08~ 12.8 (0.0~
#> 8 sex      Male   1 (ref)     1 (ref)    1 (ref)     1 (ref)     1 (ref)   
#> 9 sibsp    1 unit 0.96 (1.04~ 0.74 (1.3~ 0.68 (1.44~ 0.76 (1.30~ 0.69 (1.4~
```
