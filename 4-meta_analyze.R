library(data.table)
library(tidyverse)
library(latex2exp)


# d <- read_csv("results_full.csv", guess_max = 5e5)

d <- fread("results_full.csv", sep = ",") %>%
  as_tibble()

grps <- quos(nblk, neu, blk_sd, mu, gen_method, ana_method, term)



d2 <- d %>% 
  filter(
    !(
      str_detect(term, "(trt)|(blk.eu)") | 
        is.na(term)
    )
  ) %>%
  filter(
    !str_detect(group, "blk.eu") |
      is.na(group)
  ) %>%
# patch code to deal with misnamed columns
mutate(
  conf.low = case_when(
    !is.na(conf.low) &  is.na(c025) ~ conf.low,
     is.na(conf.low) & !is.na(c025) ~ c025,
    TRUE ~ NA_real_
  ),
  conf.high = case_when(
    !is.na(conf.high) &  is.na(c975) ~ conf.high,
     is.na(conf.high) & !is.na(c975) ~ c975,
    TRUE ~ NA_real_
  )
)


# d2 %>% count(!!! grps) %>% View()
# 
# wtf <- d2 %>% filter(
#   nblk == 4,
#   neu == 6,
#   blk_sd == .50,
#   mu == "m10", 
#   gen_method == "pois_normal",
#   ana_method == "lme4_fit_normal"
# )
# 
grps <- quos(nblk, neu, blk_sd, mu, gen_method, ana_method)

d3 <- d2 %>%
  mutate(
    cover = as.numeric(conf.low <= blk_sd & blk_sd <= conf.high),
    bias_rep = (estimate - blk_sd) / blk_sd
  ) %>%
  group_by(!!! grps) %>%
  summarize(
    coverage = mean(cover),
    bias = mean(bias_rep)
  ) %>%
  ungroup() %>%
  mutate(
    pt_shape = case_when(
      bias > .1 | bias < -.1 ~ 2L,
      TRUE ~ 1L
    ),
    bias = case_when(
      bias > .1 ~ .1,
      bias < -.1 ~ -.1,
      TRUE ~ bias
    ),
    
    # nblk = as_factor(TeX(paste0("$n_b = ", nblk, "$"), output = "text"))
    nblk = {
      f <- as_factor(nblk)
      l <- levels(f)
      nl <- paste0("$n_b = ", l, "$")
      levels(f) <- TeX(nl)
      f
    },
    
    neu = {
      f <- as_factor(neu)
      l <- levels(f)
      nl <- paste0("$n_e = ", l, "$")
      levels(f) <- TeX(nl)
      f
    },
    
    blk_sd = {
      f <- as_factor(blk_sd)
      l <- levels(f)
      nl <- paste0("$\\tau^2 = ", l, "$")
      levels(f) <- TeX(nl)
      f
    },
    # 
    mu = {
      f <- as_factor(mu)
      l <- levels(f)
      # message(class(mu))
      nl <- paste0(str_replace(l, "^m", "$\\\\mu = "), "$")
      # nl <- gsub("^m", "$\\\\mu = ", mu)
      levels(f) <- TeX(nl)
      f
    },
    
    gen_method = factor(
      gen_method, 
      levels = c("pois_gamma", "pois_normal"),
      labels = c("Gamma", "Normal")
    )
    # nblk = TeX(paste0("$n_b = ", as.character(nblk), "$")),
    # 
    # neu = TeX(paste0("$n_e = ", as.character(neu), "$")),
    # 
    # blk_sd = TeX(paste0("$\\tau^2 = ", blk_sd, "$")),
    # 
    # mu = str_replace(mu, "$m", "$\\mu = ") %>% paste0("$") %>% TeX()
  )


coverage <- ggplot(data = d3, aes(x = ana_method, y = coverage, fill = ana_method)) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(labels = c(
    "lme4 - NB",
    "lme4 - Norm",
    "stan - NB",
    "stan - Norm"
  )) +
  facet_grid(
    nblk + neu ~ blk_sd + mu + gen_method,
    labeller = label_parsed
  ) +
  labs(
    y = "Coverage",
    fill = "Analytic method"
  ) +
  theme(
    # axis.text.x = element_text(angle = 90)
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

bias <- ggplot(data = d3, aes(x = ana_method, color = ana_method, y = bias, shape = factor(pt_shape))) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = .05, ymax = Inf, alpha = .075) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -.05, alpha = .075) +
  geom_hline(yintercept = 0) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(19, 1), name = NULL, labels = NULL, guide = "none") +
  # scale_color_manual(values = 1:4, labels = letters[1:4]) +
  # scale_color_discrete(labels = letters[1:4]) +
  scale_color_discrete(labels = c(
    "lme4 - NB",
    "lme4 - Norm",
    "stan - NB",
    "stan - Norm"
  )) +
  ylim(-.1, .1) +
  facet_grid(
    nblk + neu ~ blk_sd + mu + gen_method,
    labeller = label_parsed
  ) +
  # theme_bw() +
  labs(
    shape = NULL,
    color = "Analytic method",
    y = "Relative bias"
  ) +
  theme(
    # axis.text.x = element_text(angle = 90)
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.border = element_rect(fill = NA)
  )
  
# for poster
ggsave("pres_coverage.png", coverage, width = 22.56, height = 14.24, dpi = 72)
ggsave("pres_bias.png", bias, width = 22.56, height = 14.24, dpi = 72)

# 
# 
# d2 <- d %>%
#   mutate(
#     bias = (estimate - blk_sd) / blk_sd,
#     # reject = as.numeric(between(blk_sd, c025, c975))
#     reject = as.numeric(c025 <= blk_sd & blk_sd <= c975)
#   ) %>% 
#   filter(
#     !is.na(c025) & !is.na(c975),
#     abs(bias) < 1,
#     str_detect(ana_method, "wald_", negate = TRUE) & is.na(error) |
#       str_detect(ana_method, "wald_") & sas_status == 0 & sas_g_mat == 1
#   ) 

# d3 <- d2 %>%
#   group_by(!!! grps) %>%
#   summarize(
#     bias_ugh = mean(bias),
#     bias_lo = mean(bias) - 1.96 * sd(bias) / sqrt(n()),
#     bias_hi = mean(bias) + 1.96 * sd(bias) / sqrt(n()),
#     cover = mean(reject),
#     cover_lo = mean(reject) - 1.96 * sd(reject) / sqrt(n()),
#     cover_hi = mean(reject) + 1.96 * sd(reject) / sqrt(n())
#   )
# 
# ggplot(d3, aes(x = ana_method, y = bias_ugh, ymin = bias_lo, ymax = bias_hi)) +
#   geom_point() +
#   geom_errorbar() +
#   facet_grid(nblk + neu ~ blk_sd + mu + gen_method) +
#   labs(title = "Bias")
# 
# ggplot(d3, aes(x = ana_method, y = cover, ymin = cover_lo, ymax = cover_hi)) +
#   geom_point() +
#   geom_errorbar() +
#   facet_grid(nblk + neu ~ blk_sd + mu + gen_method) +
#   labs(title = "Coverage")
# 
# 
# # ggplot(d2, aes(x = ana_method, y = reject)) +
# #   stat_summary(fun.y = "mean", geom = "bar") +
# #   facet_grid(nblk + neu ~ blk_sd + mu + gen_method) +
# #   labs(title = "Coverage")
# # 
# # ggplot(d2, aes(x = ana_method, y = bias)) +
# #   stat_summary(fun.y = "mean", geom = "point") +
# #   facet_grid(nblk + neu ~ blk_sd + mu + gen_method) +
# #   labs(title = "Bias")
# # 
# #   