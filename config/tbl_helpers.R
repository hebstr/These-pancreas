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

style_strata <- \(data, palette) {
  .style <- \(tbl, level, color) {
    tbl_row_color(tbl, rows = label == level, color = color)
  }

  reduce2(names(palette), palette, .style, .init = data)
}
