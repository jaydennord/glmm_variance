
# packages ----------------------------------------------------------------

checkpoint::checkpoint("2019-11-01", scanForPackages = FALSE, checkpointLocation = ".")

library(tidyverse)

devtools::session_info()


# collection --------------------------------------------------------------

# from_r <- paste0("res/res_", 0:749, ".csv") %>%
#   map_dfr(read_csv, col_types = cols())
# 
# from_sas <- dir(path = "res", "wald_", full.names = TRUE) %>%
#   map_dfr(read_csv, col_types = cols(), .id = "source") %>%
#   filter(CovParm == "Intercept")


results <- c("res", "wald_normal", "wald_nb") %>%
  set_names() %>%
  map(~ 
    paste0("res/", ., "_", 0:749, ".csv") %>%
    map_dfr(read_csv, col_types = cols())
  ) %>%
  modify_at(
    vars(starts_with("wald_")), select,
    id, c025 = LowerWaldCL, c975 = UpperWaldCL,
    sas_msg = Reason, sas_status = Status, sas_g_mat = pdG
  ) %>%
  bind_rows(.id = "source") %>%
  mutate(
    ana_method = if_else(is.na(ana_method), source, ana_method),
    source = NULL
  )

# %>%
#   select(id, c025 = LowerWaldCL, c975 = UpperWaldCL)

# results <- bind_rows(from_r, from_sas, .id = "source") %>%
#   mutate(
#     ana_method = if_else(is.na(ana_method), source, ana_method),
#     source = NULL
#   )

dsn <- read_csv("dsn.csv")

d <- right_join(dsn, results, by = "id")

write_csv(d, "results_full.csv")
