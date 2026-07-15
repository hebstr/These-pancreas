get_fig_cuminc <- \(data) {
  .cmp_events <- levels(data[["vars"]][[2]])

  .cmp_tests <- data$crr_fg |>
    left_join(
      select(data$crr_test, outcome, gray = p.value),
      by = "outcome"
    ) |>
    summarise(
      label = str_flatten(
        str_glue("{str} {estimate_ci} Gray {gray}"),
        collapse = "\n"
      )
    )

  data$crr |>
    ggcuminc(outcome = tail(.cmp_events, 2)) +
    add_confidence_interval(alpha = 0.05) +
    add_censor_mark(shape = "\U0131", size = 3) +
    add_risktable(
      risktable_stats = "{n.risk} ({cum.event}/{cum.censor})",
      stats_label = "Nombre à risque (évènements/censures)",
      risktable_group = "risktable_stats",
      size = 2,
      family = opts$font$alpha,
      theme = theme_risktable()
    ) +
    add_risktable_strata_symbol(symbol = "\U2014", size = 8) +
    geom_text(
      data = .cmp_tests,
      mapping = aes(label = label, x = 0, y = 1),
      size = 2.5,
      hjust = 0,
      vjust = 1,
      family = opts$font$alpha,
      inherit.aes = FALSE
    ) +
    scale_ggsurvfit(
      x_scales = list(
        name = .surv_labs$pfs$x,
        breaks = seq(0, max(data$tte$obs$time), by = 6)
      ),
      y_scales = list(
        name = "Incidence cumulée (%)",
        breaks = seq(0, 1, by = 0.2),
        labels = label_style_percent()
      )
    ) +
    scale_color_manual(
      values = .fig_palette,
      aesthetics = c("color", "fill")
    ) +
    guides(fill = "none") +
    theme_tte() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.02, 0.85),
      legend.justification = c(0, 1),
      legend.key.width = unit(18, "pt"),
      legend.text = element_text(size = 7)
    )
}

fig_cuminc_strata <- get_fig_cuminc(.cmp$strata$data)

easy_out(fig_cuminc_strata, height = 3.75, width = 7)
