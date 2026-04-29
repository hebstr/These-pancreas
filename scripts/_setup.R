### VARS -----------------------------------------------------------------------

.var_num <- variables |> filter(type == "num") |> pull(var_name)

.var_labels <- variables |>
  select(var_name, label) |>
  deframe() |>
  as.list()

.val_labels <- variables |>
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

### DF -------------------------------------------------------------------------

df <- recueil_ano |>
  drop_na(centre) |>
  mutate(
    across(matches("date"), dmy),
    across(all_of(.var_num), as.numeric),
    age_diag = (interval(date_birth, date_diag) / years(1)) %/% 0.1 * 0.1,
  ) |>
  set_variable_labels(!!!.var_labels, .strict = FALSE) |>
  set_value_labels(!!!.val_labels, .strict = FALSE) |>
  modify_if(is.labelled, as_factor) |>
  keep(~ !is.null(var_label(.)))

# easy_view(df, assign = TRUE)

### AUTO EXEC ------------------------------------------------------------------

# auto_exec()
