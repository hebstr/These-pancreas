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

col_missing <- \(x, prefix = "", empty = "0", header = "**DM**", align = "center") {
  all_stat <- x$table_body |> select(matches("^stat_\\d+$")) |> names()
  group_cols <- setdiff(all_stat, "stat_0")
  stat_cols <- if (length(group_cols) > 0) group_cols else all_stat
  first_stat <- all_stat[1]
  x |>
    modify_table_body(\(body) {
      dm <- body |>
        filter(row_type == "missing") |>
        mutate(across(all_of(stat_cols), \(s) replace_na(s, empty))) |>
        rowwise() |>
        mutate(dm = str_glue("{prefix} {str_c(c_across(all_of(stat_cols)), collapse = '/')}")) |>
        ungroup() |>
        select(variable, dm)
      body |>
        left_join(dm, by = "variable") |>
        mutate(dm = if_else(row_type == "label", as.character(dm), NA_character_)) |>
        relocate(dm, .before = all_of(first_stat)) |>
        filter(!row_type %in% "missing")
    }) |>
    modify_header(dm = header) |>
    modify_column_alignment(columns = dm, align = align)
}

style_strata <- \(data, palette) {
  .tab_style <- \(gt, level, color) {
    tab_style(
      data = gt,
      style = cell_text(color = color),
      locations = cells_body(rows = label == level)
    )
  }

  reduce2(names(palette), palette, .tab_style, .init = data)
}
