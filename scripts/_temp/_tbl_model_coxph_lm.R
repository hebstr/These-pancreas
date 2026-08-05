### DATA ------------------------------------------------------------------------

.landmark <- 9

.model <- lst(
  data = df |>
    filter(os_tte >= .landmark) |>
    mutate(os_tte_lm = os_tte - .landmark),
  y = c("os_tte_lm", "os_event"),
  x = lst(
    uv = c(
      "n_cures_outcome",
      "groupe",
      "centre",
      "chir_hosp",
      "age",
      # "ps_diag",
      # "ca_diag",
      "chir_resec"
    ),
    mv = uv
  ),
  args = lst(
    pvalue_fun = opts$pvalue$format,
    exponentiate = TRUE
  ),
  descr = check_model_vars(
    data = data[, c(y, x$uv)],
    by = y[2]
  )
)

.model$coxph <- lst(
  y = expr(Surv(!!!syms(.model$y))),
  fit = coxph(reformulate(.model$x$mv, y), .model$data)
)

### TBL ------------------------------------------------------------------------

.tbl_coxph_models <- lst(
  uv = tbl_uvregression(
    data = .model$data[c(.model$y, .model$x$uv)],
    y = !!.model$coxph$y,
    method = coxph,
    !!!.model$args,
    hide_n = TRUE
  ),
  mv = tbl_regression(
    x = .model$coxph$fit,
    !!!.model$args
  )
)

tbl_coxph_lm <- .tbl_coxph_models |>
  map(
    ~ . |>
      gtsum_format(
        ci = opts$ci,
        model_mv = .model$coxph$fit,
        label_header = opts$labs$header,
        estim_sep = opts$sep$int
      ) |>
      add_global_p()
  ) |>
  tbl_merge(tab_spanner = str_glue("**{opts$labs$spanner}**")) |>
  tbl_format(
    note_pvalue = str_glue(
      "Analyse landmark à {.landmark} mois du diagnostic : seuls les patients à
      risque au landmark sont inclus et l'horloge de suivi est remise à zéro au
      landmark, en conditionnant sur la survie jusqu'à ce point pour supprimer
      le temps immortel. Le modèle de régression de Cox multivariable incluait
      {.model$coxph$fit$nevent} évènements pour {.model$coxph$fit$n} observations
      complètes ({nrow(.model$data) - .model$coxph$fit$n} observations
      supprimées pour cause de données manquantes). n_cures_outcome est classée
      sur le total final de cures : ce landmark retire les décès précoces mais
      pas la mauvaise attribution du person-time, il sous-corrige le biais (voir
      le modèle à covariable dépendante du temps)."
    )
  )

coxph_lm_ph <- cox.zph(.model$coxph$fit)
coxph_lm_ph_plot <- ggcoxzph(coxph_lm_ph)

easy_out(tbl_coxph_lm, width = 680)
