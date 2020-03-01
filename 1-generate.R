
#!/usr/bin/env Rscript
nrep <- commandArgs(trailingOnly=TRUE) # 1000


# packages ----------------------------------------------------------------

checkpoint::checkpoint("2019-11-01", scanForPackages = FALSE, checkpointLocation = ".")

library(data.table)
library(tidyverse)
library(stringi)

devtools::session_info()


# functions ---------------------------------------------------------------

gen_data <- function(id, nblk, neu, blk_sd, gen_method, phi, eu_sd, mu, ...) {

  mu <- switch(
    EXPR = mu,
    m10 = rep(10, 6),
    m45 = rep(45, 6)
  )

  n <- nblk * neu

  blk <- rep(seq_len(nblk), each = neu)
  eu  <- rep_len(seq_len(neu), n)
  trt <- rep_len(seq_len(6), n)
  mu  <- mu[trt]

  b <- rnorm(n = nblk, sd = blk_sd)[blk]

  y <- switch(

    EXPR = gen_method,

    pois_normal = {
      e <- rnorm(n, sd = eu_sd)
      eta <- log(mu) + b + e
      lambda <- exp(eta)
      rpois(n, lambda = lambda)
    },

    pois_gamma = {
      eta <- log(mu) + b
      lambda <- exp(eta)
      rnbinom(n, size = 1/phi, mu = lambda)
    }

  )

  return(data.frame(
    id  = id,
    blk = blk,
    eu  = eu,
    trt = trt,
    y   = y,
    stringsAsFactors = FALSE
  ))


}

calc_phi <- function(mu, blk_sd, eu_sd, ...) {
  
  mu <- case_when(
    mu == "m10" ~ 10,
    mu == "m45" ~ 45
  )
  
  s2 <- eu_sd^2
  t2 <- blk_sd^2
  
  elz <- exp(log(mu) + t2/2)
  vlz <- exp(2*log(10) + 2*t2) - exp(2*log(10) + t2)
  
  ely <- exp(log(mu) + (t2 + s2)/2)
  vly <- exp(2*log(10) + 2*(t2+s2)) - exp(2*log(10) + t2 + s2)
  
  phi <- (ely + vly - elz - vlz) / (elz^2 + vlz)
  
  return(phi)
  
}

# design matrix -----------------------------------------------------------

# SAS glimmix defaults CIs are possible
# write-up of some kind
# can't compare generation conditions
# 
# dsn_common <- crossing(
#   nblk = c(4, 20),
#   neu = 6,
#   blk_sd = c(.25, .5, .75),
#   mu = c("m10", "m45"),
#   rep = 1:nrep
# )

# dsn_methods <- list(
# 
#   pois_gamma = crossing(
#     phi = c(.25, 5)
#   ),
# 
#   pois_normal = crossing(
#     eu_sd = c(.5, .9)
#   )
# 
# ) %>% bind_rows(.id = "gen_method")


set.seed(12479)

dsn <- crossing(
  nblk = c(4, 10, 20, 30),
  neu = 1:3 * 6,
  blk_sd = c(.25, .5, .75),
  mu = c("m10", "m45"),
  rep = 1:nrep,
  gen_method = c("pois_gamma", "pois_normal")
) %>%
  mutate(
    id = stri_rand_strings(n(), length = 10),
    sim_block = sample(0:749, size = n(), replace = TRUE),
    eu_sd = (1 - blk_sd^2),
    phi = calc_phi(mu, blk_sd, eu_sd)
  )
# dsn <- dsn_common %>% crossing(
#   # gen_method = c("pois_gamma", "pois_normal")
#   dsn_methods
# ) %>%
#   mutate(
#     # eu_sd = sqrt(1 - blk_sd^2),
#     # phi = 1 / (exp(eu_sd^2 / 2) + 1),
#     id = stri_rand_strings(n(), length = 10),
#     sim_block = sample(0:749, size = n(), replace = TRUE)
#   )


# generate ----------------------------------------------------------------

dirs <- c("data", "res",  "slurm")
for (dir in dirs) if (!dir.exists(dir)) dir.create(dir)

set.seed(12479)

dsn %>%
  group_by(sim_block) %>%
  group_walk(~ {
    d <- pmap_dfr(.x, gen_data)
    fwrite(d, file = paste0("data/data_", .y, ".csv"), sep = ",")
  })

fwrite(dsn, "dsn.csv", sep = ",")
