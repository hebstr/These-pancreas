### DATA ------------------------------------------------------------------------

.model_plr <- lst(
  data = df |>
    mutate(ps_diag = ps_diag |> fct_collapse("1-2" = c(1, 2))),
  y = "n_cures_outcome",
  x = lst(
    uv = c(
      "groupe",
      "centre",
      "age_incr",
      "ps_diag",
      "chir_resec"
    ),
    mv = setdiff(uv, "chir_resec")
  ),
  args = lst(
    pvalue_fun = opts$pvalue$format,
    exponentiate = TRUE,
    tidy_fun = broom.helpers::tidy_parameters
  ),
  sep = glm(
    formula = reformulate(x$mv, y),
    family = binomial,
    data = data,
    method = detectseparation::detect_separation
  ),
  fit = logistf::logistf(
    formula = reformulate(x$mv, y),
    data = data
  )
)

### TBL ----------------------------------------------------------------------

.plr_models <- lst(
  uv = tbl_uvregression(
    data = .model_plr$data[c(.model_plr$y, .model_plr$x$uv)],
    y = .model_plr$y,
    method = logistf::logistf,
    !!!.model_plr$args,
    hide_n = TRUE
  ),
  mv = tbl_regression(
    x = .model_plr$fit,
    !!!.model_plr$args
  )
)

tbl_model_plr <- .plr_models |>
  map(
    ~ . |>
      gtsum_format(
        ci = opts$ci,
        model_mv = .model_plr$fit,
        label_header = opts$labs$header,
        estim_sep = opts$sep$int
      ) |>
      add_global_p(anova_fun = anova_logistf)
  ) |>
  tbl_merge(tab_spanner = str_glue("**{opts$labs$spanner}**")) |>
  gt_format(
    note_global = str_glue(
      "Le critère de jugement est la réalisation d'un schéma incomplet de
      chimiothérapie, défini par un nombre total de cures reçues
      strictement inférieur à 12."
    ),
    note_pvalue = str_glue(
      "Modèle de régression logistique multivariable pénalisé par la méthode
      de Firth, incluant {sum(.model_plr$fit$y)} évènements pour
      {nobs(.model_plr$fit)} observations complètes
      ({nrow(.model_plr$data) - nobs(.model_plr$fit)} observations supprimées
      pour cause de données manquantes). Les intervalles de confiance à 95%
      et les p-values sont issus de la vraisemblance profilée pénalisée."
    )
  ) |>
  add_note(
    vars = "age_incr",
    note = "Incrémentation par tranches de 5 ans."
  )

easy_out(tbl_model_plr, width = 680)
