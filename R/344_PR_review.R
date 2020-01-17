

library(tidyverse)
library(gtsummary)
library(forcats)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# READ ME ----
# '****' in document outline indicates a note about an issue


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Checking code from PR ----

df_missing <-
  tibble(
    my_by_var = c(1,1,1,2,2),
    fct = rep(NA, 5) %>% factor(levels = c("lion", "tiger", "bear")),
    lgl = NA,
    chr = NA_character_,
    int = NA_integer_,
    dbl = NA_real_
  )

tbl_summary(df_missing, by = my_by_var)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# percent arg ----

tbl_summary(df_missing, by = my_by_var,
            percent = "cell")

tbl_summary(df_missing, by = my_by_var,
            percent = "row")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# statistic arg ----

tbl_summary(df_missing, by = my_by_var,
            statistic = list(all_continuous() ~ "{mean} ({sd})"))

tbl_summary(df_missing, by = my_by_var,
            statistic = list(all_continuous() ~ "{median} ({min}, {max})"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# missing arg ----

tbl_summary(df_missing, by = my_by_var,
            missing = "no")
tbl_summary(df_missing, by = my_by_var,
            missing = "always")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# missing_text arg ----

tbl_summary(df_missing, by = my_by_var,
            missing_text = "Hey, I'm missing!")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sort arg ----
# **** NOTE 1 ----
# Sorting factor levels by alphanumeric does not work (see order of lion, tiger,
# bear). Not sure this really matters when the entire column is missing. Also
# unsure if this is happening because the alphabetic order of the factor
# levels doesn't override the numeric order of them

tbl_summary(df_missing, by = my_by_var,
            sort = list(everything() ~ "frequency"))
tbl_summary(df_missing, by = my_by_var,
            sort = list(everything() ~ "alphanumeric"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# type arg ----
# **** NOTE 2 ----
# Unclear errors when try to tell tbl_summary to report int or dbl var as
# categorical variables. Also when try to report all logical as categorical

tbl_summary(df_missing, by = my_by_var,
            type = list("int" ~ "categorical"))
tbl_summary(df_missing, by = my_by_var,
            type = list("dbl" ~ "categorical"))
tbl_summary(df_missing, by = my_by_var,
            type = list(all_logical() ~ "categorical"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# digits arg ----

tbl_summary(df_missing, by = my_by_var,
            digits = list("int" ~ c(2,3,3)))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# label arg ----

tbl_summary(df_missing, by = my_by_var,
            label = list("lgl" = "Logical", "dbl" = "Double",
                         "chr" = "Character", "fct" = "Factor",
                         "int" = "Integer"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# value arg ----

tbl_summary(df_missing, by = my_by_var,
            value = list("lgl" ~ "FALSE"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# add_p ----
# **** NOTE 3 ----
# Based on error throwbn, it's unclear why add_p won't work (if user) doesn't
# realize that they have all missing values for all columns.

tbl_summary(df_missing, by = my_by_var) %>%
  add_p()

# *one col not missing ----
df <-
  tibble(
    my_by_var = c(1,1,1,2,2),
    fct = c("lion","lion","tiger","tiger","lion") %>% factor(levels = c("lion", "tiger", "bear")),
    lgl = NA,
    chr = NA_character_,
    int = NA_integer_,
    dbl = NA_real_
  )

tbl_summary(df, by = my_by_var) %>% add_p()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Larger dataset ----
df <-
  tibble(
    my_by_var = c(rep(1,1000),rep(2,500)),
    fct = rep(NA,1500) %>% factor(levels = c("lion","tiger","bear")),
    lgl = NA,
    chr = NA_character_,
    int = NA_integer_,
    dbl = NA_real_
  )

df %>% tbl_summary(by = my_by_var)

# *No by ----
df %>% tbl_summary()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Character by var ----

df <-
  tibble(
    my_by_var = c(rep("1",1000),rep("2",500)),
    fct = rep(NA,1500) %>% factor(levels = c("lion","tiger","bear")),
    lgl = NA,
    chr = NA_character_,
    int = NA_integer_,
    dbl = NA_real_
  )

df %>% tbl_summary(by = my_by_var)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Factor by var ----

df <-
  tibble(
    my_by_var = c(rep(1,1000),rep(2,500)),
    fct = rep(NA,1500) %>% factor(levels = c("lion","tiger","bear")),
    lgl = NA,
    chr = NA_character_,
    int = NA_integer_,
    dbl = NA_real_
  ) %>%
  mutate(
    my_by_var_fct = as_factor(my_by_var)
  )

glimpse(df)
class(df$my_by_var_fct)
count(df, my_by_var_fct)

df %>% tbl_summary(by = my_by_var_fct)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Logical by var ----

df <-
  tibble(
    my_by_var = c(rep(TRUE,1000),rep(FALSE,500)),
    fct = rep(NA,1500) %>% factor(levels = c("lion","tiger","bear")),
    lgl = NA,
    chr = NA_character_,
    int = NA_integer_,
    dbl = NA_real_
  )

glimpse(df)
class(df$my_by_var)

df %>% tbl_summary(by = my_by_var)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# By var with level missing ----

df <-
  tibble(
    my_by_var = c(rep(1,1000),rep(2,500),rep(NA,500)),
    fct = rep(NA,2000) %>% factor(levels = c("lion","tiger","bear")),
    lgl = NA,
    chr = NA_character_,
    int = NA_integer_,
    dbl = NA_real_
  )

df %>% tbl_summary(by = my_by_var)
df$my_by_var <- forcats::fct_explicit_na(df$my_by_var)
df$my_by_var <- forcats::fct_explicit_na(as_factor(df$my_by_var))
df %>% tbl_summary(by = my_by_var)

# *1 non-missing value in each var----
# **** NOTE 4 ----
# Seems like numeric variables don't get reported as continuous variables should
# when there is only a single obs with non-missing values these variables.
# Instead they are reported as the categorical default dictates (i.e. N (%)).
# Tried various values for the statistic arg. No error message thrown.
# Works when you specify the type arg, but doesn't not properly guess that
# these vars are continuous.
# Also, there is a by variable level with all missing values

df <-
  tibble(
    my_by_var = c(rep(1,1000),rep(2,500),rep(NA,500)),
    fct = rep(NA,2000) %>% factor(levels = c("lion","tiger","bear")),
    lgl = NA,
    chr = NA_character_,
    int = NA_integer_,
    dbl = NA_real_
  )

df_nonmiss_by1 <-
  tibble(
    my_by_var = c(1,1,1),
    fct = rep("lion",3) %>% factor(levels = c("lion","tiger","bear")),
    lgl = FALSE,
    chr = "Hello",
    int = 2 %>% as.integer(),
    dbl = c(8.2,5.1,3.4),
    num = c(1,2,3)
  ) %>%
  filter(num == 1) %>%
  select(-num)

glimpse(df_nonmiss_by1)

df2 <- bind_rows(
  df, df_nonmiss_by1
)

tail(df2)
glimpse(df2)
class(df2$int)
class(df2$dbl)

tbl_summary(df2, by = my_by_var)
df2$my_by_var <- forcats::fct_explicit_na(as_factor(df2$my_by_var))
tbl_summary(df2, by = my_by_var)
tbl_summary(df2, by = my_by_var,
            statistic = list(all_continuous() ~ "{median} ({p25}, {p75})"))
tbl_summary(df2, by = my_by_var,
            statistic = list(all_continuous() ~ "{mean} ({sd})"))

tbl_summary(df2, by = my_by_var,
            type = list("int" ~ "continuous", "dbl" ~ "continuous"),
            statistic = list(all_continuous() ~ "{mean} ({sd})"))

tbl_summary(df2, by = my_by_var,
            type = list("int" ~ "continuous"),
            statistic = list(all_continuous() ~ "{mean} ({sd})"))

# **add_p ----

tbl_summary(df2, by = my_by_var) %>% add_p()




