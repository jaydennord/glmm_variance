library(tidyverse)

d <- read_csv("results_full.csv", guess_max = 5e5)

grps <- quos(nblk, neu, blk_sd, mu, gen_method, phi, eu_sd, ana_method)

d %>% group_by(!!! grps) %>% count() %>% View

dplot <- d %>% filter(
  !is.na(c025) & !is.na(c975),
  str_detect(ana_method, "wald_", negate = TRUE) & is.na(warning) & is.na(error) |
  str_detect(ana_method, "wald_") & sas_status == 0 & sas_g_mat == 1
) %>%
  count(!!! grps)
  
dplot %>%
  filter(gen_method == "pois_normal") %>%
  ggplot(aes(x = ana_method, y = n)) +
    geom_bar(stat = "identity") +
    facet_grid(nblk + blk_sd ~ mu + eu_sd)

dplot %>%
  filter(gen_method == "pois_gamma") %>%
  ggplot(aes(x = ana_method, y = n)) +
  geom_bar(stat = "identity") +
  facet_grid(nblk + blk_sd ~ mu + phi)

