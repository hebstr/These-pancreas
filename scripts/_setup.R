set_opts()

url <- "https://docs.google.com/spreadsheets/d/1iL6LS6ZXJpctP8I0Rj0HLSVGK3KoClZcX1EcIOkduUI/edit?pli=1&gid=2126636750#gid=2126636750"

dict <- read_sheet(url, sheet = 1)
data <- read_sheet(url, sheet = 2, col_types = "c")

# openxlsx2::write_xlsx(dict, "data/dictionnaire.xlsx")
# openxlsx2::write_xlsx(data, "data/recueil.xlsx")

.num <- dict |> filter(type == "num") |> pull(var_name)

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
    across(matches("date"), dmy),
    across(all_of(.num), as.numeric),
    age_diag = (interval(date_birth, date_diag) / years(1)) %/% 0.1 * 0.1,
  ) |>
  set_variable_labels(!!!.labels) |>
  set_value_labels(!!!.levels) |>
  modify_if(is.labelled, as_factor) |>
  keep(~ !is.null(var_label(.)))

# easy_view(df, assign = TRUE)
