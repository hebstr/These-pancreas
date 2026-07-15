extract_from_dict <- \(data, var_name, var_label, type, level) {
  .var <- data |>
    select({{ var_name }}, {{ var_label }}) |>
    deframe() |>
    as.list()

  .val <- data |>
    separate_longer_delim(
      cols = {{ level }},
      delim = regex("\\s*[,;]\\s*")
    ) |>
    drop_na({{ level }}) |>
    separate_wider_regex(
      cols = {{ level }},
      patterns = c(.value = "\\d+", "\\s*[=:]\\s*", .label = ".+")
    ) |>
    summarise(
      labels = list(set_names(as.numeric(.value), .label)),
      .by = {{ var_name }}
    ) |>
    deframe()

  .type <- data |>
    split(~type) |>
    map(pull, {{ var_name }})

  list(
    var = .var,
    val = .val,
    type = .type
  )
}
