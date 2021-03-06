
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/ddsjoberg/gtsummary.svg?branch=master)](https://travis-ci.org/ddsjoberg/gtsummary)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ddsjoberg/gtsummary?branch=master&svg=true)](https://ci.appveyor.com/project/ddsjoberg/gtsummary)
[![Coverage
status](https://codecov.io/gh/ddsjoberg/gtsummary/branch/master/graph/badge.svg)](https://codecov.io/github/ddsjoberg/gtsummary?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/gtsummary)](https://cran.r-project.org/package=gtsummary)
[![](https://cranlogs.r-pkg.org/badges/gtsummary)](https://cran.r-project.org/package=gtsummary)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## gtsummary <a href='https://github.com/ddsjoberg/gtsummary'><img src='man/figures/logo.png' align="right" height="120" /></a>

The {gtsummary} package provides an elegant and flexible way to create
publication-ready and reproducible analytical tables. The tables
summarize data sets, regression models, and more. The code is concise
and the tables are highly customizable. Data frames can be summarized
with any function, e.g. mean(), median(), even user-written functions.
Regression models are summarized and include the reference rows for
categorical variables. Common regression models, such as logistic
regression and Cox proportional hazards regression, are automatically
identified and the tables are pre-filled with appropriate column headers
(i.e. Odds Ratio, and Hazard Ratio). The package uses
[{broom}](https://broom.tidyverse.org/) to perform initial tidying of
the regression models, which means there is broad support for many types
of regression models.

{gtsummary} uses the [{gt}](https://gt.rstudio.com/) package enabling
each table to be tailored to your preferences. If you label your data
(which I recommend\!), the labels will be used in the table output. With
{gtsummary} and [{labelled}](http://larmarange.github.io/labelled/)
data, you get beautifully formatted, ready-to-share tables in a single
line of code\! Check out the examples below, and review the vignettes
for a detailed exploration of the output options.

## Installation

The {gtsummary} package was written as a companion to the {gt} package
from RStudio, and it is recommended to install both {gt} and
{gtsummary}. The {gt} package is not automatically installed. If {gt} is
not installed, `knitr::kable()` will be used to produce the summary
tables. You can install {gtsummary} and {gt} with the following code.

1.  Install {gtsummary}
    
    ``` r
    install.packages("gtsummary")
    ```

2.  Install {gt} from GitHub (recommended)
    
    ``` r
    install.packages("remotes")
    remotes::install_github("rstudio/gt", gtsummary::gt_sha)
    ```

Install the development version of {gtsummary} with:

``` r
remotes::install_github("ddsjoberg/gtsummary")
```

## Examples

The {gtsummary} vignettes/tutorials contain detailed examples.

### Summary Table

``` r
library(gtsummary)
t1 <-
  tbl_summary(
    data = trial[c("trt", "age", "grade", "response")],
    by = trt
  ) %>%
  add_p() 
```

<img src="man/figures/README-tbl_summary_print-1.png" width="66%">

### Regression Models

``` r
mod1 <- glm(response ~ trt + age + grade, trial, family = binomial)

t2 <- tbl_regression(mod1, exponentiate = TRUE)
```

<img src="man/figures/README-tbl_regression_print-1.png" width="50%">

### Side-by-side Regression Models

Side-by-side regression model results from `tbl_merge()`

<img src="man/figures/tbl_merge_ex1.png" width="66%">

Review even more output options in the [table
gallery](http://www.danieldsjoberg.com/gtsummary/articles/gallery.html).

## Print Engine

{gtsummary} uses the {gt} package to print all summary tables. In
addition to supporting {gt}, the {gtsummary} package works well with
`knitr::kable()`. This is particularly useful when outputting documents
to Microsoft Word. If the {gt} package is not installed, {gtsummary}
will fall back to `knitr::kable()`. To explicitly set the printing
engine, set the option in the script or in the user- or project R
profile, `.Rprofile`.

    options(gtsummary.print_engine = "kable") 

or

    options(gtsummary.print_engine = "gt")

Output from {kable} is less full featured compared to summary tables
produced with {gt}. For example, {kable} summary tables do not include
indentation, footnotes, and spanning header rows.

## Contributing

Please note that the {gtsummary} project is released with a [Contributor
Code of
Conduct](http://www.danieldsjoberg.com/gtsummary/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms. A big
thank you to all contributors\!  
[@ablack3](https://github.com/ablack3),
[@ahinton-mmc](https://github.com/ahinton-mmc),
[@ddsjoberg](https://github.com/ddsjoberg),
[@emilyvertosick](https://github.com/emilyvertosick),
[@jeanmanguy](https://github.com/jeanmanguy),
[@jennybc](https://github.com/jennybc),
[@jflynn264](https://github.com/jflynn264),
[@jwilliman](https://github.com/jwilliman),
[@karissawhiting](https://github.com/karissawhiting),
[@ltin1214](https://github.com/ltin1214),
[@margarethannum](https://github.com/margarethannum),
[@michaelcurry1123](https://github.com/michaelcurry1123),
[@sammo3182](https://github.com/sammo3182),
[@slobaugh](https://github.com/slobaugh), and
[@zabore](https://github.com/zabore)
