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


pct_suffix <- \(x) str_glue("{x}\u00A0%")

style_pct <- \(x, digits = 1) pct_suffix(style_percent(x, digits = digits))

style_median <- \(x, unit = "mois") {
  str_replace(
    x,
    "^(\\S+)\\s+(\\[.+\\])$",
    paste0("\\1 ", unit, " (", str_remove_all(opts$ci$label, "\\[|\\]"), " \\2)")
  )
}

style_median_strata <- \(x, unit = "mois") {
  str_flatten(
    str_glue("{style_median(x, unit)} dans le groupe {str_to_lower(names(x))}"),
    collapse = ", ",
    last = " contre "
  )
}

title_suffix <- \(title, strata = "selon le protocole de chimiothérapie") {
  lst(
    strata = strata,
    overall = str_glue("globalement et {strata}")
  ) |>
    map(~ str_glue("{title}, {.x}."))
}
