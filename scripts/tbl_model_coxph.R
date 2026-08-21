### DATA ------------------------------------------------------------------------

.model$coxph <- lst(
  data = .model$default$data,
  vars = set_model_vars(
    y = c("os_tte", "os_event"),
    x_uv = .model$default$vars$x$uv,
    x_mv_exclude = NULL
  ),
  args = .model$default$args,
  descr = \() {
    check_model_vars(
      data = data[, c(vars$y[2], vars$x$uv)],
      by = vars$y[2]
    )
  },
  y_fun = call2("Surv", rlang::splice(syms(vars$y))),
  fit = coxph(reformulate(vars$x$mv, y_fun), data)
)

### TBL ------------------------------------------------------------------------

.model$coxph$tbls <- lst(
  uv = tbl_uvregression(
    data = .model$coxph$data,
    y = !!.model$coxph$y_fun,
    method = coxph,
    hide_n = TRUE,
    include = .model$coxph$vars$x$uv,
    !!!.model$coxph$args
  ),
  mv = tbl_regression(
    x = .model$coxph$fit,
    !!!.model$coxph$args
  )
)

tbl_coxph <- .model$coxph$tbls |>
  map(
    ~ . |>
      gtsum_format() |>
      add_global_p()
  ) |>
  tbl_merge(tab_spanner = str_glue("**{opts$labs$spanner}**")) |>
  add_note(
    vars = "age_incr",
    note = "Incrémentation par tranches de 5 ans."
  ) |>
  tbl_format(
    note_global = str_glue(
      "Le critère de jugement est la survie globale, définie par le délai entre
      le diagnostic et le décès toutes causes. Les patients vivants sont
      censurés à la date de point du centre."
    ),
    note_pvalue = str_glue(
      "Modèle de regression de Cox multivariable incluant
      {.model$coxph$fit$nevent} évènements pour {.model$coxph$fit$n}
      observations complètes ({nrow(.model$coxph$data) - .model$coxph$fit$n}
      observations supprimées pour cause de données manquantes)."
    ),
    width = 680
  )

### CHECK -------------------------------------------------------------------

.model$coxph$check <- cox.zph(.model$coxph$fit)

.coxph_check_label <- .model$coxph$vars$x$mv |>
  set_names() |>
  map_chr(~ var_label(df[[.x]])) |>
  c(GLOBAL = "Ensemble du modèle")

.coxph_check_tbl <- .model$coxph$check$table |>
  as_tibble(rownames = "term") |>
  transmute(
    variable = .coxph_check_label[term],
    chi2 = round(chisq, 2),
    ddl = df,
    p = style_pvalue(p, digits = 2)
  )

### OUTPUT ---------------------------------------------------------------------

easy_out(tbl_coxph)
