check_fun <- \(
  .data,
  ...,
  .prefix = "check_",
  .vars = c()
) {
  data_check <- .data |>
    transmute(rowname = rowname, ...) |>
    pivot_longer(cols = starts_with(.prefix), names_to = "check") |>
    filter(value) |>
    select(-value)

  .data |>
    select(rowname, all_of(.vars)) |>
    inner_join(data_check, by = "rowname") |>
    mutate(check = str_remove(check, .prefix)) |>
    relocate(check, .after = 1)
}

check_distrib <- \(data) {
  data |>
    discard(is.Date) |>
    use_vars() |>
    tbl_summary(
      statistic = opts$vars$stat,
      digits = opts$digits,
      missing = "ifany",
      missing_text = opts$labs$missing
    ) |>
    add_stat_label(label = opts$vars$label) |>
    tbl_format()
}
