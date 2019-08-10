
#' rpriori: A grammar for testing apriori hypotheses in R.
#'
#'
#' To learn more about rpriori, start with the vignettes:
#' `browseVignettes(package = "rpriori")`
#'
#' @import dplyr
#' @import tibble
#' @importFrom forcats fct_inorder
#' @importFrom rlang %||% is_character ensyms
#' @importFrom vctrs vec_size vec_is_empty
#' @importFrom tidyr unnest spread
#' @importFrom purrr map map_dfr map_chr map_dbl map_lgl 
#'   set_names modify pluck reduce flatten
#' @importFrom tidyselect vars_pull
#' @importFrom formula.tools lhs rhs lhs.vars rhs.vars is.one.sided
#' @importFrom geepack geeglm
#' @importFrom stats glm lm qnorm coef vcov as.formula update.formula
#' @importFrom survival coxph
#' @importFrom glue glue glue_collapse
#' @importFrom magrittr %>% %<>% set_colnames
#' @importFrom tibbleOne build_meta adapt_round

"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")
  utils::globalVariables(
    c(
      ".",
      ".x",
      "err",
      "eff",
      "type",
      "name",
      "level",
      "formula",
      "outcome",
      "exposure",
      "estimate",
      "variable",
      "coef_output",
      "has_missing"
    )
  )