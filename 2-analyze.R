
#!/usr/bin/env Rscript
index <- commandArgs(trailingOnly=TRUE)


# packages ----------------------------------------------------------------

# pkgs <- c(
#   "data.table",
#   "tidyverse",
#   "rstanarm",
#   "lme4"
# )
# for (pkg in pkgs) library(pkg, character.only = TRUE)

#checkpoint()

checkpoint::checkpoint("2019-11-01", scanForPackages = FALSE, checkpointLocation = ".")

library(data.table)
library(tidyverse)
library(rstanarm)
library(lme4)

devtools::session_info()

# custom functions --------------------------------------------------------

robust_template <- function(formula, data, ...) {
  
  e <- w <- NA_character_
  res <- matrix(NA_real_, 1, 2)
  
  tryCatch(
    
    withCallingHandlers(
      res <- my_conf(fun(formula, data, ...)),
      warning = function(w) {
        w <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    ),
    
    error = function(e) {
      e <<- conditionMessage(e)
    }
    
  )
  
  colnames(res) <- c("c025", "c975")
  # if (!is.na(res)) {
  #   res <- my_conf(res)
  # }
  
  
  
  # list(
  #   result = res,
  #   warning = w,
  #   error = e
  #   
  # )
  
  cbind(
    as_tibble(res),
    warning = w,
    error = e, 
    stringsAsFactors = FALSE
  )
  
}

funs <- c(
  "stan_glmer",
  "stan_glmer.nb",
  "glmer",
  "glmer.nb"
)

for (f in funs) {
  new_f <- robust_template
  body(new_f)[[4]][[2]][[2]][[3]][[2]][[1]] <- substitute(fun, list(fun = as.name(f)))
  assign(paste0("r_", f), new_f)
}

my_conf <- function(...) UseMethod("my_conf")

my_conf.stanreg <- function(fit, ...) {
  sqrt(posterior_interval(fit, pars = "Sigma[blk:(Intercept),(Intercept)]", prob = .95))
}

my_conf.glmerMod <- function(fit, ...) {
  confint(fit, parm = "sd_(Intercept)|blk", method = "profile", quiet = TRUE, oldNames = FALSE)
}


ana_data <- function(data, ...) {
  fits <- list(
    bayes_fit_normal = r_stan_glmer   (y ~ 0 + trt + (1 | blk) + (1 | blk:trt), data = data, family = "poisson", refresh = 0),
    bayes_fit_nb     = r_stan_glmer.nb(y ~ 0 + trt + (1 | blk), data = data, refresh = 0),
    
    freq_fit_normal = r_glmer   (y ~ 0 + trt + (1 | blk) + (1 | blk:trt), data = data, family = "poisson"),
    freq_fit_nb     = r_glmer.nb(y ~ 0 + trt + (1 | blk), data = data)
    
  )
  
  # map(fits, ~ my_conf(.$result))
  # purrr::transpose(fits)
  bind_rows(fits, .id = "ana_method")
  
}




# analyses ----------------------------------------------------------------

dgroup <- fread(
  file = paste0("data/data_", index, ".csv"), 
  sep = ",", 
  data.table = FALSE,
  colClasses = c("character", "factor", "factor", "numeric")
)

dres <- dgroup %>% group_by(id) %>%
  group_map(ana_data)

fwrite(dres, paste0("res/res_", index, ".csv"), sep = ",")
