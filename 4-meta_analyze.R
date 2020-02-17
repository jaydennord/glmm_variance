library(tidyverse)

d <- read_csv("results_full.csv", guess_max = 5e5)

grps <- quos(nblk, neu, blk_sd, mu, gen_method, ana_method)

d %>% group_by(!!! grps) %>% count() %>% View

# dplot <- d %>% filter(
#   !is.na(c025) & !is.na(c975),
#   str_detect(ana_method, "wald_", negate = TRUE) & is.na(warning) & is.na(error) |
#   str_detect(ana_method, "wald_") & sas_status == 0 & sas_g_mat == 1
# ) %>%
#   count(!!! grps)
#   
# dplot %>%
#   filter(gen_method == "pois_normal") %>%
#   ggplot(aes(x = ana_method, y = n)) +
#     geom_bar(stat = "identity") +
#     facet_grid(nblk + blk_sd ~ mu + eu_sd)
# 
# dplot %>%
#   filter(gen_method == "pois_gamma") %>%
#   ggplot(aes(x = ana_method, y = n)) +
#   geom_bar(stat = "identity") +
#   facet_grid(nblk + blk_sd ~ mu + phi)

d %>% filter(c975 < 5) %>%
ggplot(aes(x = c975)) + 
  geom_histogram() +
  facet_grid(nblk + blk_sd ~ mu + gen_method + ana_method)




d2 <- d %>% filter(
  is.na(error),
  is.na(sas_g_mat) | sas_g_mat == 1,
  is.na(sas_status) | sas_status == 0
); View(d2)

str_remove(d2$warning, "max\\|grad\\| = \\d.\\d*") %>% unique()


d2 %>% filter(c975 < 5) %>%
  ggplot(aes(x = c975)) + 
  geom_histogram() +
  facet_grid(nblk + blk_sd ~ mu + gen_method + ana_method)






d2 %>%
  mutate(reject = c025 < blk_sd & blk_sd < c975) %>%
  count(!!! grps) %>%
  ggplot(aes(x = ana_method, y = n)) +
  geom_bar(stat = "identity") +
  facet_grid(nblk + blk_sd ~ gen_method + mu)

d2 %>%
  mutate(width = c975 - c025) %>%
  filter(c025 > 0 & c975 < 5, width > 0) %>%
  ggplot(aes(x = width, fill = ana_method)) +
  geom_histogram(alpha = .5, position = "identity") +
  facet_grid(nblk + blk_sd ~ gen_method + mu)

d2 %>%
  mutate(width = c975 - c025) %>%
  filter(c025 > 0 & c975 < 5, width > 0, !str_detect(ana_method, "wald"), nblk != 4) %>%
  ggplot(aes(x = width, fill = ana_method)) +
  geom_histogram(alpha = .5, position = "identity") +
  facet_grid(nblk + blk_sd ~ gen_method + mu)
