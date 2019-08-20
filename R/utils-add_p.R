#' Tests used in add_p
#'
#' These functions calculate pvalues for various tests.
#'
#' @param data input data set
#' @param variable categorical or continuous variable for which a
#' test with \code{by_var} is desired
#' @param by categorical variable
#' @param group the group variable for clustered data
#' @keywords internal
#' @noRd
#' @author Daniel D. Sjoberg

add_p_test <- function(data, ...) UseMethod("add_p_test")

add_p_test.t.test <- function(data, variable, by, ...) {
  result = list()
  result$p <- stats::t.test(data[[variable]] ~ as.factor(data[[by]]))$p.value
  result$test <- "t-test"
  result
}

add_p_test.kruskal.test <- function(data, variable, by, ...) {
  result = list()
  result$p <- stats::kruskal.test(data[[variable]], as.factor(data[[by]]))$p.value
  result$test <- "Kruskal-Wallis test"
  result
}

add_p_test.wilcox.test <- function(data, variable, by, ...) {
  result = list()
  if (length(unique(data[[by]])) > 2)
    stop("Wilcoxon rank-sum test cannot be calculated with more than 2 groups")
  result$p <- stats::kruskal.test(data[[variable]], as.factor(data[[by]]))$p.value
  result$test <- "Wilcoxon rank-sum test"
  result
}

add_p_test.chisq.test <- function(data, variable, by, ...) {
  result = list()
  result$p <- stats::chisq.test(data[[variable]], as.factor(data[[by]]))$p.value
  result$test <- "chi-square test of independence"
  result
}

add_p_test.fisher.test <- function(data, variable, by, ...) {
  result = list()
  result$p <- stats::fisher.test(data[[variable]], as.factor(data[[by]]))$p.value
  result$test <- "Fisher's exact test"
  result
}

add_p_test.lme4 <- function(data, variable, by, group, type, ...) {
  result = list()
  # input checks for lme4 tests
  if (data[[by]] %>% unique() %>% length() != 2) {
    # only allowing logistic regression models for now
    stop("'by' variable must have two and only two levels.")
  }
  if (is.null(group)) {
    # lme4 requires a group to account for the correlated data
    stop("'group' argument cannot be NULL for 'test = \"lme4\"'")
  }

  # creating formulas for base model (without variable) and full model
  formula0 <- glue("as.factor({by}) ~ 1 + (1 | {group})")
  if (type == "continuous") {
    formula1 <- glue("as.factor({by}) ~ {variable} + (1 | {group})")
  } else {
    formula1 <- glue("as.factor({by}) ~ as.factor({variable}) + (1 | {group})")
  }

  # building base and full models
  mod0 <- lme4::glmer(stats::as.formula(formula0),
                      data = data, family = stats::binomial)
  mod1 <- lme4::glmer(stats::as.formula(formula1),
                      data = data, family = stats::binomial)

  #returning p-value
  result$p <- stats::anova(mod0, mod1)$"Pr(>Chisq)"[2]
  result$test <- "random intercept logistic regression"
  result
}


# this is a safe wrapper for add_p_test().  It does some brief prepping before
# passing the arguments along to add_p_test()
add_p_test_safe <- function(data, variable, by, group, test, include = NULL, type) {
  # omitting variables not in include
  if (!variable %in% include) {
    return(NULL)
  }

  # keeping non-missing values
  data <- stats::na.omit(data[c(variable, by, group)])
  # add class of the test type to have the method function work
  class(data) <- c(test, class(data))

  # calculating pvalue
  tryCatch({
    # initializing to NA
    pval <- NA_real_
    pval <- add_p_test(
      data = data, variable = variable,
      by = by, group = group, test = test,
      type = type
    )
  },
  # printing warning and errors as message
  warning = function(w) {
    message(glue("Warning in 'add_p()' for variable '{variable}' ",
                 "and test '{test}', p-value omitted:\n", as.character(w)))
    return(NULL)
  },
  error = function(e) {
    message(glue("Error in 'add_p()' for variable '{variable}' ",
                 "and test '{test}', p-value omitted:\n", as.character(e)))
    return(NULL)
  })

  pval
}

# vectorized version of the functions that calculate a single pvalue
calculate_pvalue <- function(data, variable, by, test, type, group, include) {
  pmap(
    list(variable, by, test, type),
    ~ add_p_test_safe(
      data = data, variable = ..1, by = ..2,
      group = group, test = ..3, include = include, type = ..4
    )
  )
}