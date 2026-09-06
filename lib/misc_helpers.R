export_docx <- \() {
  withr::with_options(
    list(hebstr.docx = TRUE, easy_out.export = TRUE, easy_out.quiet = TRUE),
    auto_exec(include = "^tbl")
  )
}

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

get_label <- \(data, ..., sort = TRUE, indent = "  ") {
  vars <- enquos(...)
  recurse <- \(data, vars, depth) {
    var <- vars[[1]]
    pad <- strrep(indent, depth)
    .n <- count(data, !!var, name = ".n", sort = sort)
    lines <- str_glue("{pad}- {.n[[1]]} : {.n$.n}")
    if (length(vars) == 1L) {
      return(str_flatten(lines, "\n"))
    }
    children <- map_chr(
      .n[[1]],
      \(value) recurse(filter(data, !!var == value), vars[-1], depth + 1)
    )
    str_flatten(str_glue("{lines}\n{children}"), "\n")
  }
  recurse(data, vars, 0)
}

get_var_distrib <- \(data, title = NULL, ...) {
  data |>
    discard(is.Date) |>
    use_vars() |>
    tbl_summary(
      statistic = opts$vars$stat,
      digits = opts$digits,
      missing = "ifany",
      missing_text = opts$labs$row_missing
    ) |>
    add_stat_label(label = opts$vars$label) |>
    tbl_format(
      title = title,
      title_align = "center",
      ...
    )
}

auto_ceiling <- \(time, by) {
  seq(0, ceiling(time / by - 1e-3) * by, by = by)
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
