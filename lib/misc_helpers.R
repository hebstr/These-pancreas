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

easy_label <- \(
  data,
  variable = list(),
  value = list(),
  drop = FALSE
) {
  data <- data |>
    modify_if(is.logical, as.numeric) |>
    set_variable_labels(
      !!!dict$var,
      !!!variable,
      .strict = FALSE
    ) |>
    set_value_labels(
      !!!discard_at(dict$val, names2(value)),
      !!!value,
      .strict = FALSE
    ) |>
    mutate(across(
      any_of(dict$type$int) | where(is.labelled),
      labelled::to_factor
    ))

  if (drop) {
    data <- discard(data, ~ is.null(var_label(.x)))
  }

  data
}


title_suffix <- \(title, strata = "selon le protocole de chimiothérapie") {
  lst(
    strata = strata,
    overall = str_glue("globalement et {strata}")
  ) |>
    map(~ str_glue("{title}, {.x}."))
}
