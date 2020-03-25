
#!/usr/bin/env Rscript
index <- commandArgs(trailingOnly=TRUE)


# packages ----------------------------------------------------------------

checkpoint::checkpoint("2019-11-01", scanForPackages = FALSE, checkpointLocation = ".")

library(tidyverse)
library(rstanarm)
library(lme4)
library(broom.mixed)

rstan::rstan_options(auto_write = FALSE)

devtools::session_info()


# custom functions --------------------------------------------------------

robust_template <- 'function(formula, data, ...) {

  e <- w <- NA_character_
  
  res <- tibble(estimate = NA_real_)
  
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

  cbind(
    res,
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
  summary(fit, prob = c(.025, .975), regex_pars = "^(Sigma|trt)") %>% 
    as_tibble(rownames = "term") %>%
    mutate_at(vars(mean, `2.5%`, `97.5%`), ~ ifelse(grepl("^trt\\d", term), exp(.), .)) %>%
    rename(
      estimate  = mean,
      conf.low  = `2.5%`,
      conf.high = `97.5%`
    )
}

my_conf.glmerMod <- function(fit, ...) {
  tidy(fit, exponentiate = T, conf.int = T, conf.method = "profile")
}


ana_data <- function(data, ...) {
  fits <- list(
    stan_fit_normal = r_stan_glmer   (y ~ 0 + trt + (1 | blk) + (1 | blk:eu), data = data, family = "poisson", refresh = 0, adapt_delta = .99, seed = 12479),
    stan_fit_nb     = r_stan_glmer.nb(y ~ 0 + trt + (1 | blk), data = data, refresh = 0, adapt_delta = .99, seed = 12479),

    lme4_fit_normal = r_glmer   (y ~ 0 + trt + (1 | blk) + (1 | blk:eu), data = data, family = "poisson"),
    lme4_fit_nb     = r_glmer.nb(y ~ 0 + trt + (1 | blk), data = data)

  )

  bind_rows(fits, .id = "ana_method")

}


# analyses ----------------------------------------------------------------

dgroup <- read_csv(
  file = paste0("data/data_", index, ".csv"), 
  col_types = "cfffn"
)

dres <- dgroup %>%
  group_by(id) %>%
  nest() %>%
  mutate(
    res = map(data, ana_data),
    data = NULL
  ) %>%
  unnest(res)

write_csv(dres, paste0("res/res_", index, ".csv"))
