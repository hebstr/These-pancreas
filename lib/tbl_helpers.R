binary_value <- \(data, labels = c("Non", "Oui")) {
  data |>
    select(where(~ setequal(levels(.x), labels))) |>
    map(~ levels(.x)[2])
}

strip_label <- \(
  data,
  pattern,
  replacement = "",
  to_upper = TRUE
) {
  new <- imap(
    var_label(data),
    \(label, name) {
      if (is.null(label)) {
        NULL
      } else {
        label <- label |> str_replace_all(pattern, replacement)
        if (to_upper) {
          label <- label |> str_replace("^\\p{L}", toupper)
        }
        label
      }
    }
  )
  set_variable_labels(data, .labels = new, .strict = FALSE)
}

tbl_cell <- \(tbl, variable, level = NULL, column = "stat_0") {
  body <- tbl[["_data"]]

  keep <- body$variable %in%
    variable &
    if (is.null(level)) body$row_type %in% "label" else body$label %in% level

  cell <- body[[column]][keep]

  if (length(cell) != 1 || is.na(cell)) {
    cli::cli_abort(c(
      "No single {.val {column}} cell for {.val {variable}}.",
      i = "Matched {length(cell)} row{?s} in {.arg tbl}."
    ))
  }

  str_match(cell, "^(.+?)\\s*\\((.+)\\)$")[1, 2:3]
}

tbl_n_pct <- \(tbl, variable, level = NULL, column = "stat_0") {
  parts <- tbl_cell(tbl, variable, level, column)

  lst(n = parts[1], pct = pct_suffix(parts[2]))
}

tbl_med_iqr <- \(tbl, variable, level = NULL, column = "stat_0") {
  parts <- tbl_cell(tbl, variable, level, column)

  lst(med = parts[1], iqr = str_replace(parts[2], "\\p{Pd}", " à "))
}

style_strata <- \(data, palette) {
  .style <- \(tbl, level, color) {
    tbl_row_color(tbl, rows = label == level, color = color)
  }

  reduce2(names(palette), palette, .style, .init = data)
}
