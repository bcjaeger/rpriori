
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

The goal of `rpriori` is to

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("bcjaeger/rpriori")
```

# Example

Here are the packages we’ll use for this example:

``` r

library(rpriori)
library(magrittr)
library(glue)
library(tidyverse)
#> Warning: package 'tidyr' was built under R version 3.6.1
#> Warning: package 'dplyr' was built under R version 3.6.1
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
into an object that encapsulates our main hypothesis with `specs_embed`

``` r

main_hypothesis <- survived ~ pclass

analysis <- main_hypothesis %>% 
  specs_embed(m0, m1, m2a, m2b, m3)

analysis
#> # A tibble: 5 x 4
#>   name     outcome  exposure formula  
#>   <chr>    <chr>    <chr>    <list>   
#> 1 Model 0  survived pclass   <formula>
#> 2 Model 1  survived pclass   <formula>
#> 3 Model 2a survived pclass   <formula>
#> 4 Model 2b survived pclass   <formula>
#> 5 Model 3  survived pclass   <formula>
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


analysis %<>%
  mutate(
    fits = fit_apri(
      formulas = formula,
      data = drop_na(titanic),
      family = binomial(link = 'logit'),
      engine = 'glm'
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
  hoist_effect(fits, effect = pclass)
#> # A tibble: 5 x 8
#>   name     outcome  exposure formula   fits   First Second Third
#>   <chr>    <chr>    <chr>    <list>    <list> <dbl>  <dbl> <dbl>
#> 1 Model 0  survived pclass   <formula> <glm>     NA -0.726 -1.80
#> 2 Model 1  survived pclass   <formula> <glm>     NA -1.31  -2.58
#> 3 Model 2a survived pclass   <formula> <glm>     NA -1.41  -2.65
#> 4 Model 2b survived pclass   <formula> <glm>     NA -1.33  -2.58
#> 5 Model 3  survived pclass   <formula> <glm>     NA -0.800 -1.83
```

Neat, but maybe not as easy to read as it could be. `hoist_effect` has a
few aesthetic helper inputs to make model output a little easier to
interpret. For example, instead of looking at estimates on the
log-scale, we can exponentiate them:

``` r

analysis %>% 
  hoist_effect(fits, effect = pclass, transform = exp)
#> # A tibble: 5 x 8
#>   name     outcome  exposure formula   fits   First Second  Third
#>   <chr>    <chr>    <chr>    <list>    <list> <dbl>  <dbl>  <dbl>
#> 1 Model 0  survived pclass   <formula> <glm>     NA  0.484 0.165 
#> 2 Model 1  survived pclass   <formula> <glm>     NA  0.270 0.0757
#> 3 Model 2a survived pclass   <formula> <glm>     NA  0.243 0.0705
#> 4 Model 2b survived pclass   <formula> <glm>     NA  0.265 0.0757
#> 5 Model 3  survived pclass   <formula> <glm>     NA  0.449 0.160
```

Okay\! Now we have odds-ratios instead of regression coefficients, and
we can easily see that, according to the apriori models we specified,
ticket class has a pretty strong effect. A natural follow-up question is
how much uncertainty we have regarding those point estimates, and a
natural follow-up answer is to use the `ci` input argument of
`hoist_effect` like so:

``` r

analysis %>% 
  hoist_effect(fits, effect = pclass, ci = 0.95, transform = exp)
#> # A tibble: 5 x 8
#>   name    outcome  exposure formula   fits  First  Second       Third      
#>   <chr>   <chr>    <chr>    <list>    <lis> <chr>  <chr>        <chr>      
#> 1 Model 0 survived pclass   <formula> <glm> 1 (re~ 0.48 (0.44,~ 0.17 (0.15~
#> 2 Model 1 survived pclass   <formula> <glm> 1 (re~ 0.27 (0.23,~ 0.08 (0.06~
#> 3 Model ~ survived pclass   <formula> <glm> 1 (re~ 0.24 (0.21,~ 0.07 (0.06~
#> 4 Model ~ survived pclass   <formula> <glm> 1 (re~ 0.27 (0.23,~ 0.08 (0.06~
#> 5 Model 3 survived pclass   <formula> <glm> 1 (re~ 0.45 (0.38,~ 0.16 (0.14~
```

This type of output can be passed right into your favorite table
function.

``` r

footer <- list(m0, m1, m2a, m2b, m3) %>% 
  map_chr(spec_describe)

analysis %>% 
  hoist_effect(fits, effect = pclass, ci = 0.95, transform = exp) %>% 
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
