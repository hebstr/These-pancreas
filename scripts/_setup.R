library(tidyverse)
library(googlesheets4)
library(labelled)
library(gtsummary)
library(hebstr)

lang_fr()

set_opts()

url <- "https://docs.google.com/spreadsheets/d/1iL6LS6ZXJpctP8I0Rj0HLSVGK3KoClZcX1EcIOkduUI/edit?pli=1&gid=2126636750#gid=2126636750"

dict <- read_sheet(url, sheet = 1)
data <- read_sheet(url, sheet = 2, col_types = "c")

.labels <- dict |>
  select(var_name, label) |>
  deframe() |>
  as.list()

.levels <- dict |>
  separate_longer_delim(
    cols = valeurs,
    delim = regex("\\s*[,;]\\s*")
  ) |>
  drop_na(valeurs) |>
  separate_wider_regex(
    cols = valeurs,
    patterns = c(val = "\\d+", "\\s*=\\s*", lab = ".+")
  ) |>
  summarise(
    labels = list(set_names(val, lab)),
    .by = var_name
  ) |>
  deframe()

df <- data |>
  drop_na(centre) |>
  mutate(
    across(everything(), ~ replace_values(., "," ~ NA)),
    across(matches("date"), dmy),
    age_diag = (interval(date_birth, date_diag) / years(1)) %/% 0.1 * 0.1,
  ) |>
  set_variable_labels(!!!.labels, .strict = FALSE) |>
  set_value_labels(!!!.levels, .strict = FALSE) |>
  keep(~ !is.null(var_label(.))) |>
  modify_if(is.labelled, as_factor) |>
  mutate(across(
    where(is.factor),
    ~ fct_na_level_to_value(., extra_levels = "Pas de donnée")
  ))

df |> hebstr::easy_view()
