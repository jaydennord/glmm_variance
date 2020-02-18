
#!/usr/bin/env Rscript
index <- commandArgs(trailingOnly=TRUE)


# packages ----------------------------------------------------------------

checkpoint::checkpoint("2019-11-01", scanForPackages = FALSE, checkpointLocation = ".")

library(data.table)
library(tidyverse)
library(rstanarm)
library(lme4)

rstan::rstan_options(auto_write = FALSE)

devtools::session_info()


# custom functions --------------------------------------------------------

robust_template <- 'function(formula, data, ...) {

  e <- w <- NA_character_
  res <- matrix(NA_real_, 1, 2)

  tryCatch(

    withCallingHandlers(
      res <- my_conf(<FUN>(formula, data, ...)),
      warning = function(w) {
        w <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    ),

    error = function(e) {
      e <<- conditionMessage(e)
    }

  )

  colnames(res) <- c("estimate", "c025", "c975")

  cbind(
    as_tibble(res),
    warning = w,
    error = e,
    stringsAsFactors = FALSE
  )

}'

funs <- c(
  "stan_glmer",
  "stan_glmer.nb",
  "glmer",
  "glmer.nb"
)

for (f in funs) {
  new_f <- str_replace_all(robust_template, "<FUN>", f) %>%
    str2expression() %>%
    eval()
  assign(paste0("r_", f), new_f)
}

my_conf <- function(...) UseMethod("my_conf")

my_conf.stanreg <- function(fit, ...) {
  sqrt(c(estimate = c(VarCorr(fit)$blk), posterior_interval(fit, pars = "Sigma[blk:(Intercept),(Intercept)]", prob = .95)))
}

my_conf.glmerMod <- function(fit, ...) {
  cbind(estimate = c(VarCorr(fit)$blk), confint(fit, parm = "sd_(Intercept)|blk", method = "profile", quiet = TRUE, oldNames = FALSE))
}


ana_data <- function(data, ...) {
  fits <- list(
    stan_fit_normal = r_stan_glmer   (y ~ 0 + trt + (1 | blk) + (1 | blk:trt), data = data, family = "poisson", refresh = 0, adapt_delta = .99, seed = 12479),
    stan_fit_nb     = r_stan_glmer.nb(y ~ 0 + trt + (1 | blk), data = data, refresh = 0, adapt_delta = .99, seed = 12479),

    lme4_fit_normal = r_glmer   (y ~ 0 + trt + (1 | blk) + (1 | blk:trt), data = data, family = "poisson"),
    lme4_fit_nb     = r_glmer.nb(y ~ 0 + trt + (1 | blk), data = data)

  )

  bind_rows(fits, .id = "ana_method")

}


# analyses ----------------------------------------------------------------

dgroup <- fread(
  file = paste0("data/data_", index, ".csv"),
  sep = ",",
  data.table = FALSE,
  colClasses = c("character", "factor", "factor", "numeric")
)

dres <- dgroup %>%
  group_by(id) %>%
  nest() %>%
  mutate(
    res = map(data, ana_data),
    data = NULL
  ) %>%
  unnest(res)


fwrite(dres, paste0("res/res_", index, ".csv"), sep = ",")
