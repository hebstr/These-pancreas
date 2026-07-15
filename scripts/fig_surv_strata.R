get_fig_surv <- \(data, outcome) {
  .surv_data <- data[["data"]]
  .labs <- .surv_labs[[outcome]]
  .surv_time <- .surv_data$tte$obs$time
  .surv_brks <- auto_ceiling(time = max(.surv_time), by = 6)
  .surv_nudge <- 2.5
  .surv_xlim <- range(.surv_brks, .surv_time + .surv_nudge)

  .surv_plot <- .surv_data$tte$model |>
    ggsurvfit() +
    add_confidence_interval(alpha = 0.05) +
    add_censor_mark(shape = "\U0131", size = 3) +
    geom_text(
      data = .surv_data$cox |>
        mutate(logrank = .surv_data$logrank$p.value),
      mapping = aes(
        label = str_glue(
          "{str} {opts$ci$label}{opts$sep$int}{estimate_ci}
          Log rank {logrank}"
        ),
        x = max(.surv_time),
        y = 1
      ),
      size = 2.5,
      hjust = 1,
      vjust = 1,
      family = opts$font$alpha
    ) +
    geom_text_repel(
      data = .surv_data$tte$obs |> slice_tail(n = 1, by = strata),
      mapping = aes(
        x = time,
        y = estimate,
        label = strata,
        color = strata
      ),
      nudge_x = .surv_nudge,
      min.segment.length = Inf,
      size = 2.5,
      family = opts$font$alpha,
      seed = 1
    ) +
    scale_ggsurvfit(
      x_scales = list(
        name = .labs$x,
        breaks = .surv_brks,
        limits = .surv_xlim,
        expand = expansion(mult = 0.05)
      ),
      y_scales = list(
        name = .labs$y,
        breaks = seq(0, 1, by = 0.2),
        labels = label_style_percent()
      )
    ) +
    scale_color_manual(
      values = .fig_palette,
      aesthetics = c("color", "fill")
    ) +
    theme_tte()

  .surv_risktable <- .surv_data$tte$model |>
    tidy_survfit(times = .surv_brks) |>
    mutate(stat = str_glue("{n.risk} ({cum.event}/{cum.censor})")) |>
    ggplot() +
    aes(
      x = time,
      y = strata,
      label = stat,
      color = strata
    ) +
    geom_text(
      size = 2,
      family = opts$font$alpha
    ) +
    scale_color_manual(values = .fig_palette) +
    scale_x_continuous(
      breaks = .surv_brks,
      limits = .surv_xlim
    ) +
    scale_y_discrete(expand = expansion(mult = 0.8)) +
    labs(title = "Nombre à risque (évènements/censures)") +
    theme_risktable() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  list(.surv_plot, .surv_risktable) |>
    ggsurvfit_align_plots() |>
    wrap_plots(ncol = 1, heights = c(1, 0.1))
}

fig_surv_strata <- imap(.surv$strata, get_fig_surv)

easy_out_map(fig_surv_strata, height = 3.75, width = 7)
