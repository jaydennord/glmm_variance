
# packages ----------------------------------------------------------------

checkpoint::checkpoint("2019-11-01", scanForPackages = FALSE, checkpointLocation = ".")

library(tidyverse)

devtools::session_info()


# collection --------------------------------------------------------------

from_r <- paste0("res/res_", 0:749, ".csv") %>%
  map_dfr(read_csv, col_types = cols())

from_sas <- dir(path = "res", "wald_", full.names = TRUE) %>%
  map_dfr(read_csv, col_types = cols()) %>%
  select(id, c025 = LowerWaldCL, c975 = UpperWaldCL)

results <- bind_rows(from_r, from_sas, .id = "source") %>%
  mutate(
    ana_method = if_else(is.na(ana_method), source, ana_method),
    source = NULL
  )

dsn <- read_csv("dsn.csv")

d <- left_join(results, dsn, by = "id")

write_csv(d, "results.csv")
