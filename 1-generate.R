
#!/usr/bin/env Rscript
nrep <- commandArgs(trailingOnly=TRUE) # 1000


# packages ----------------------------------------------------------------

checkpoint::checkpoint("2019-11-01", scanForPackages = FALSE, checkpointLocation = ".")

library(data.table)
library(tidyverse)
library(stringi)

devtools::session_info()


# functions ---------------------------------------------------------------

gen_data <- function(id, nblk, blk_sd, gen_method, phi, eu_sd, mu, ...) {

  mu <- switch(
    EXPR = mu,
    m10 = rep(10, 6),
    m45 = rep(45, 6)
  )

  neu <- length(mu)
  n <- nblk * neu

  blk <- rep(seq_len(nblk), each = neu)
  trt  <- rep_len(seq_len(neu), n)
  mu <- mu[trt]

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
      rnbinom(n, size = phi, mu = lambda)
    }

  )

  return(data.frame(
    id  = id,
    blk = factor(blk),
    trt = factor(trt),
    y   = y,
    stringsAsFactors = FALSE
  ))


}

# design matrix -----------------------------------------------------------

# SAS glimmix defaults CIs are possible
# write-up of some kind
# can't compare generation conditions

dsn_common <- crossing(
  nblk = c(4, 20),
  neu = 6,
  blk_sd = c(.1, .25, .5),
  mu = c("m10", "m45"),
  rep = 1:nrep
)

# dsn_methods <- list(
#
#   pois_gamma = crossing(
#     phi = c(.1, .2)
#   ),
#
#   pois_normal = crossing(
#     eu_sd = .5
#   )
#
# ) %>% bind_rows(.id = "gen_method")


set.seed(12479)

# dsn <- crossing(dsn_common, dsn_methods) %>%
#   mutate(
#     id = stri_rand_strings(n(), length = 10),
#     sim_block = sample(0:749, size = n(), replace = TRUE)
#   )
dsn <- dsn_common %>% crossing(
  gen_method = c("pois_gamma", "pois_normal")
) %>%
  mutate(
    eu_sd = sqrt(1 - blk_sd^2),
    phi = 1 / (exp(eu_sd^2 / 2) + 1),
    id = stri_rand_strings(n(), length = 10),
    sim_block = sample(0:749, size = n(), replace = TRUE)
  )


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
