anova_logistf <- \(model, ...) {
  # method = "nested" (default) shares the Jeffreys penalty across both models;
  # method = "PLR" relates each to its own null and yields a negative statistic here
  attr(terms(model), "term.labels") |>
    map(\(v) tibble(term = v, p.value = as.numeric(anova(model, formula = reformulate(v))$pval))) |>
    list_rbind()
}

anova_wald <- \(model, ...) {
  a <- tryCatch(
    car::Anova(model, test.statistic = "Wald", type = "II"),
    error = \(e) NULL
  )
  if (is.null(a)) {
    tibble(term = attr(terms(model), "term.labels"), p.value = NA_real_)
  } else {
    tibble(term = rownames(a), p.value = a[["Pr(>Chisq)"]])
  }
}

check_model_vars <- \(data, by = character(0)) {
  data |>
    use_vars() |>
    tbl_summary(
      by = all_of(by),
      missing = "ifany",
      missing_text = opts$labs$missing
    ) |>
    add_stat_label(label = opts$vars$label) |>
    gtsum_format() |>
    col_missing() |>
    gt_format()
}

get_surv_model <- \(
  data,
  time_to_event,
  event,
  strata = 1,
  estimate_label,
  type = c("surv", "cmp")
) {
  type <- arg_match(type)
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

  cuminc_model <- \() {
    op <- options(OutDec = ".")
    on.exit(options(op))
    do.call("cuminc", fit)
  }

  gray_test <- \(model) {
    glance(model) |>
      pivot_longer(
        cols = everything(),
        names_to = c(".value", "id"),
        names_pattern = "(.+)_(\\d+)"
      ) |>
      transmute(
        outcome,
        str = str_glue("Test de Gray pour {tolower(outcome)}"),
        p.value = style_pvalue(p.value, digits = 2, pre = TRUE)
      )
  }

  fg_model <- \() {
    events <- tail(levels(data[[all.vars(formula)[[2]]]]), 2)
    map(events, \(e) {
      do.call("crr", c(fit, failcode = e)) |>
        tidy(exp = TRUE, conf.int = TRUE) |>
        mutate(outcome = e)
    }) |>
      list_rbind() |>
      merge_estim_ci(ci_data = opts$ci$data) |>
      mutate(
        str = str_glue("sHR pour {tolower(outcome)}"),
        p.value = style_pvalue(p.value, digits = 2, pre = TRUE)
      )
  }

  get_tte_median <- \(model) {
    tbl <- summary(model)$table

    tbl <- if (is.matrix(tbl)) {
      as_tibble(tbl, rownames = "strata")
    } else {
      as_tibble_row(tbl)
    }

    tbl <- tbl |>
      rename_with(~"conf.low", ends_with("LCL")) |>
      rename_with(~"conf.high", ends_with("UCL")) |>
      merge_estim_ci(estim_col = "median", digit = 1)

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
      median = if (type == "surv") get_tte_median(model) else NULL,
    ),
    cox = if (type == "surv" && has_strata) cox_model() else NULL,
    logrank = if (type == "surv" && has_strata) logrank_test() else NULL,
    crr = if (type == "cmp") cuminc_model() else NULL,
    crr_test = if (type == "cmp" && has_strata) gray_test(crr) else NULL,
    crr_fg = if (type == "cmp" && has_strata) fg_model() else NULL
  )
}

get_tbl <- \(x, fun = c("tbl_survfit", "tbl_cuminc"), tbl_label) {
  fun <- arg_match(fun)

  args <- list(
    x = x,
    times = c(12, 24, 36, 48),
    statistic = "{estimate} {str_glue(opts$ci$data)}",
    estimate_fun = label_style_percent(digits = 1),
    label_header = "**{time} months**"
  )

  do.call(fun, args) |>
    modify_spanning_header(
      all_stat_cols() ~ str_glue("**{tbl_label}, % {opts$ci$label}**")
    )
}

build_model <- \(data, tte, event, strata, estimate_label, tbl_label, type) {
  data <- enexpr(data)
  tte <- enexpr(tte)
  event <- enexpr(event)
  strata <- enexpr(strata)

  total_model <- inject(get_surv_model(
    data = !!data,
    time_to_event = !!tte,
    event = !!event,
    type = type
  ))

  strata_model <- inject(get_surv_model(
    data = !!data,
    time_to_event = !!tte,
    event = !!event,
    strata = !!strata,
    estimate_label = estimate_label,
    type = type
  ))

  make_tbl <- \(model, ...) {
    if (type == "surv") {
      get_tbl(x = model$tte$model, fun = "tbl_survfit", ...)
    } else {
      get_tbl(x = model$crr, fun = "tbl_cuminc", ...)
    }
  }

  map(
    list(total = total_model, strata = strata_model),
    ~ lst(data = .x, tbl = make_tbl(.x, tbl_label = tbl_label))
  )
}
