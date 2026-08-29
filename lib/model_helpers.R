set_model_vars <- \(y, x_uv, x_mv_exclude = character(0)) {
  unknown <- setdiff(x_mv_exclude, x_uv)
  if (length(unknown)) {
    cli::cli_abort(
      "{.arg x_mv_exclude} : variables absentes de {.arg x_uv} : {.val {unknown}}."
    )
  }
  lst(
    y = y,
    x = lst(
      uv = x_uv,
      mv = setdiff(x_uv, x_mv_exclude)
    )
  )
}

check_model_vars <- \(data, by = character(0)) {
  data |>
    use_vars() |>
    tbl_summary(
      by = all_of(by),
      missing = "ifany",
      missing_text = opts$labs$row_missing
    ) |>
    add_stat_label(label = opts$vars$label) |>
    gtsum_format() |>
    tbl_format()
}

get_surv_model <- \(
  data,
  time_to_event,
  event,
  strata = 1,
  estimate_label
) {
  strata <- enexpr(strata)

  formula <- expr(
    Surv(!!enexpr(time_to_event), !!enexpr(event)) ~ !!strata
  )

  fit <- list(formula = formula, data = enexpr(data))

  cox_model <- \() {
    do.call("coxph", fit) |>
      tidy(exp = TRUE, conf.int = TRUE) |>
      merge_estim_ci(ci_data = opts$ci$data) |>
      mutate(
        str = str_glue("Hazard ratio pour {estimate_label}"),
        p.value = style_pvalue(p.value, digits = 2, pre = TRUE)
      )
  }

  logrank_test <- \() {
    do.call("survdiff", fit) |>
      glance() |>
      transmute(
        str = "Log rank",
        p.value = style_pvalue(p.value, digits = 2, pre = TRUE)
      )
  }

  get_tte_median <- \(model, median_na = "non atteinte") {
    tbl <- summary(model)$table

    tbl <- if (is.matrix(tbl)) {
      as_tibble(tbl, rownames = "strata")
    } else {
      as_tibble_row(tbl)
    }

    tbl <- tbl |>
      rename_with(~"conf.low", ends_with("LCL")) |>
      rename_with(~"conf.high", ends_with("UCL")) |>
      merge_estim_ci(estim_col = "median", digit = 1, keep = TRUE) |>
      mutate(estimate_ci = if_else(is.na(median), median_na, estimate_ci))

    tbl <- if ("strata" %in% names(tbl)) {
      strata <- tbl |> pull("strata") |> str_remove("\\S+=")
      tbl <- tbl |> pull("estimate_ci") |> set_names(strata)
    } else {
      pull(tbl, "estimate_ci")
    }
  }

  has_strata <- !identical(strata, 1)

  lst(
    vars = data |> select(any_of(all.vars(formula))),
    fit = fit,
    tte = lst(
      model = do.call("survfit2", fit),
      obs = tidy_survfit(model),
      median = get_tte_median(model)
    ),
    cox = if (has_strata) cox_model() else NULL,
    logrank = if (has_strata) logrank_test() else NULL
  )
}

surv_times <- c(12, 24, 36, 48)

get_tbl <- \(x, tbl_label, at_risk = TRUE) {
  label_risk <- if (at_risk) " (n à risque)" else ""

  tbl_survfit(
    x = x,
    times = surv_times,
    statistic = paste0(
      "{estimate} {str_glue(opts$ci$data)}",
      if (at_risk) " ({n.risk})"
    ),
    estimate_fun = label_style_percent(digits = 1),
    label_header = "**{time} mois**"
  ) |>
    modify_table_body(
      ~ .x |>
        mutate(across(starts_with("stat_"), \(v) str_replace(v, ",0\\)$", ")")))
    ) |>
    modify_spanning_header(
      all_stat_cols() ~ str_glue("**{tbl_label}, % {opts$ci$label}{label_risk}**")
    )
}

build_model <- \(data, tte, event, strata, estimate_label, tbl_label) {
  data <- enexpr(data)
  tte <- enexpr(tte)
  event <- enexpr(event)
  strata <- enexpr(strata)

  total_model <- inject(get_surv_model(
    data = !!data,
    time_to_event = !!tte,
    event = !!event
  ))

  strata_model <- inject(get_surv_model(
    data = !!data,
    time_to_event = !!tte,
    event = !!event,
    strata = !!strata,
    estimate_label = estimate_label
  ))

  map(
    list(total = total_model, strata = strata_model),
    ~ lst(
      data = .x,
      tbl_label = tbl_label,
      tbl = get_tbl(x = .x$tte$model, tbl_label = tbl_label)
    )
  )
}
