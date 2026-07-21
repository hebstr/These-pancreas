source("rv/scripts/rvr.R")
source("rv/scripts/activate.R")
# .rv$sync()
# .rv$summary()

options(
  repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"),
  gargle_oauth_email = Sys.getenv("GARGLE_OAUTH_EMAIL", unset = NA)
  # openxlsx2.maxWidth = 60,
)

library(conflicted)
library(tidyverse)
library(googlesheets4)
library(rlang)
library(broom)
library(labelled)
library(gtsummary)
library(ggsurvfit)
library(survival)
library(tidycmprsk)
library(ggrepel)
library(gt)
library(ggswim)
library(patchwork)
library(Gmisc)
library(grid)
library(officer)
library(rvg)
library(hebstr)

print(conflict_scout())

conflicts_prefer(dplyr::filter, Gmisc::coords, .quiet = TRUE)

auto_exec("config", quiet = TRUE)

lang_fr()

set_opts(
  acro = acro(
    DM ~ "données manquantes par groupe"
  ),
  font = "luciole"
)

opts <- get_opts()

update_geom_defaults("text", list(family = opts$font$alpha))

### GS IMPORT ------------------------------------------------------------------

sheets <- map(
  set_names(1:3, "variables", "inclusions", "exclusions"),
  ~ read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/11yV-hnL3OBmuUa5EOKfJylzl1yeg4WrleTKy8CKc2yI/edit?gid=102661300#gid=102661300",
    sheet = .x,
    col_types = "c"
  )
)

dict <- extract_from_dict(
  data = sheets$variables,
  var_name = var_name,
  var_label = var_label,
  type = type,
  level = level
)

easy_label <- \(data, variable = list(), value = list()) {
  data |>
    modify_if(is.logical, as.numeric) |>
    set_variable_labels(!!!dict$var, !!!variable, .strict = FALSE) |>
    set_value_labels(!!!dict$val, !!!value, .strict = FALSE) |>
    mutate(across(any_of(dict$type$int) | where(is.labelled), labelled::to_factor))
}

df_init <- sheets$inclusions |>
  rownames_to_column() |>
  mutate(
    rowname = as.numeric(rowname) + 1,
    across(any_of(c(dict$type$dbl, dict$type$int)), as.numeric),
    across(all_of(dict$type$date), dmy)
  )
