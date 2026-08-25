get_tbl_surv <- \(outcome, at_risk = FALSE) {
  build <- \(part) {
    model <- .surv[[part]][[outcome]]
    get_tbl(
      x = model$data$tte$model,
      fun = "tbl_survfit",
      tbl_label = model$tbl_label,
      at_risk = at_risk
    )
  }

  surv_def <- df[[str_glue("{outcome}_tte")]] |>
    var_label() |>
    str_remove(",\\s*mois$") |>
    str_replace("^.", str_to_lower)

  list(
    total = build("total"),
    strata = build("strata") |> add_p(pvalue_fun = opts$pvalue$format)
  ) |>
    tbl_stack() |>
    modify_header(p.value = "**p**") |>
    modify_footnote_header(
      footnote = "Test du log-rank",
      columns = c(p.value, statistic),
      replace = TRUE
    ) |>
    tbl_format(
      note_global = paste(
        str_glue("Le critère de jugement est le {surv_def}."),
        "Les patients sans évènement sont censurés à la date de point du centre."
      ),
      row_strip = FALSE,
      width = if (at_risk) 900 else 750
    ) |>
    style_strata(
      set_names(
        .fig_palette,
        levels(.surv$strata[[outcome]]$data$tte$obs$strata)
      )
    )
}

tbl_surv <- map(set_names(names(.surv$total)), get_tbl_surv)

.surv_cox <- map(.surv$strata, ~ .x$data$cox)

.surv_logrank <- map(.surv$strata, ~ .x$data$logrank)

.surv_median <- {
  add_unit <- \(x) if_else(x == "non atteinte", x, paste(x, "mois"))

  map(set_names(names(.surv$total)), \(outcome) {
    strata <- .surv$strata[[outcome]]$data$tte$median

    lst(
      total = add_unit(.surv$total[[outcome]]$data$tte$median),
      strata = str_flatten(
        str_glue(
          "{add_unit(strata)} dans le groupe {str_to_lower(names(strata))}"
        ),
        collapse = ", ",
        last = " contre "
      )
    )
  })
}

easy_out_map(tbl_surv)
