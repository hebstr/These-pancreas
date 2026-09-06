.ttt <- lst(
  data = df |> filter(n_cures > 0),
  n = nrow(data),
  vars = str_subset(names(df), "^total_adapt_c") |> set_names(),
  reached = map(vars, \(v) {
    cure <- as.numeric(str_extract(v, "\\d+$"))
    data$induc_nb >= cure | data$adj_nb >= cure
  }),
  n_reached = map_int(reached, sum)
)

.ttt_dm <- imap(
  .ttt$reached,
  \(reached, v) {
    missed <- reached & is.na(.ttt$data[[v]])
    strata <- split(missed, .ttt$data$groupe) |> map_int(sum)
    tibble(
      variable = v,
      stat_0 = as.character(sum(missed)),
      stat_1 = as.character(strata[[1]]),
      stat_2 = as.character(strata[[2]])
    )
  }
) |>
  list_rbind()

tbl_ttt <- .ttt$data |>
  select(
    groupe,
    matches("total_dose"),
    total_adapt,
    total_adapt_pct,
    all_of(.ttt$vars)
  ) |>
  strip_label(str_glue("^{.grp_lab$dose} : ")) |>
  use_vars() |>
  tbl_summary(
    by = groupe,
    statistic = opts$vars$stat,
    type = matches("total_dose") ~ "continuous",
    value = binary_value(df),
    digits = opts$digits,
    missing = "ifany",
    missing_text = opts$labs$row_missing
  ) |>
  add_stat_label(label = opts$vars$label) |>
  add_p(
    pvalue_fun = opts$pvalue$format,
    test.args = .test_args
  ) |>
  gtsum_format() |>
  modify_table_body(\(body) {
    body |>
      mutate(
        across(
          c(stat_0, stat_1, stat_2),
          \(x) {
            fix <- .ttt_dm[[cur_column()]][match(variable, .ttt_dm$variable)]
            if_else(row_type %in% "missing" & !is.na(fix), fix, x)
          }
        )
      ) |>
      filter(
        !(row_type %in% "missing" & variable %in% .ttt_dm$variable[.ttt_dm$stat_0 == "0"])
      )
  }) |>
  add_variable_group_header(
    header = .grp_lab$dose,
    variables = matches("total_dose")
  ) |>
  add_note(
    vars = "total_adapt_pct",
    note = "Adaptation maximale parmi les trois molécules, à la dernière cure."
  ) |>
  reduce(
    .ttt$vars,
    \(tbl, v) {
      add_note(
        tbl,
        vars = v,
        note = str_glue("Cure atteinte par {.ttt$n_reached[[v]]} patients.")
      )
    },
    .init = _
  ) |>
  tbl_format(
    note_global = str_glue(
      "Patients ayant reçu au moins une cure de chimiothérapie d'induction \\
      ou adjuvante ({sum(df$n_cures > 0)} sur {nrow(df)} patients inclus)."
    ),
    width = 800
  )

easy_out(tbl_ttt)

### QMD ------------------------------------------------------------------------

.ttt <- c(
  .ttt,
  qmd_only(lst(
    adapt = tbl_n_pct(tbl_ttt, "total_adapt"),
    adapt_sup = tbl_n_pct(tbl_ttt, "total_adapt_pct", level = ">20%")
  ))
)
