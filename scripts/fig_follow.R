.followup_data <- .followup$strata$data$tte
.followup_median <- .followup$total$data$tte$median
.followup_brks <- auto_ceiling(time = max(.followup_data$obs$time), by = 6)
.followup_xlim <- c(0, max(.followup_data$obs$time) * 1.05)

.followup_plot <- .followup_data$model |>
  ggsurvfit() +
  add_confidence_interval(alpha = 0.05) +
  add_censor_mark(shape = "\U0131", size = 3) +
  add_quantile(
    y_value = 0.5,
    linetype = "dashed",
    linewidth = 0.3
  ) +
  annotate(
    geom = "text",
    label = str_glue(
      "Durée médiane de suivi global, mois {opts$ci$label}{opts$sep$int}{.followup_median}"
    ),
    x = 0,
    y = 0.03,
    size = 2.5,
    hjust = 0,
    vjust = 0,
    family = opts$font$alpha
  ) +
  geom_label_repel(
    data = .followup_data$obs |> slice_tail(n = 1, by = strata),
    mapping = aes(
      x = time,
      y = estimate,
      label = strata,
      color = strata
    ),
    direction = "y",
    nudge_x = -1,
    nudge_y = 0.175,
    label.padding = 0.2,
    min.segment.length = Inf,
    size = 2.5,
    family = opts$font$alpha,
    seed = 1
  ) +
  scale_ggsurvfit(
    x_scales = list(
      name = "Durée depuis le diagnostic (mois)",
      breaks = .followup_brks,
      limits = .followup_xlim
    ),
    y_scales = list(
      name = "Probabilité d'être encore suivi (%)",
      breaks = seq(0, 1, by = 0.2),
      labels = label_style_percent()
    )
  ) +
  scale_color_manual(
    values = .fig_palette,
    aesthetics = c("color", "fill")
  ) +
  theme_tte()

.followup_risktable <- .followup_data$model |>
  tidy_survfit(times = .followup_brks) |>
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
    breaks = .followup_brks,
    limits = .followup_xlim
  ) +
  scale_y_discrete(expand = expansion(mult = 0.8)) +
  labs(title = "Nombre à risque (fin de suivi/décès)") +
  theme_risktable() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

fig_follow <- list(.followup_plot, .followup_risktable) |>
  ggsurvfit_align_plots() |>
  wrap_plots(ncol = 1, heights = c(1, 0.1))

easy_out(fig_follow, height = 3.75, width = 7)
