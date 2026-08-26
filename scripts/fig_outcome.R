### DATA -----------------------------------------------------------------------

.outcome_phase <- c(
  induc_nb = "Phase d'induction",
  adj_nb = "Phase adjuvante",
  n_cures = "Total"
)

.outcome_data <- df |>
  select(groupe, all_of(names(.outcome_phase))) |>
  pivot_longer(
    cols = -groupe,
    names_to = "phase",
    values_to = "cures"
  ) |>
  mutate(
    phase = factor(.outcome_phase[phase], levels = .outcome_phase)
  ) |>
  count(phase, groupe, cures, name = "n_pat") |>
  filter(!all(cures == 0), .by = c(phase, groupe))

.outcome_strata <- df |>
  count(groupe) |>
  mutate(
    label = as.character(
      str_glue("{groupe}\n(n={n}, {style_pct(n / sum(n))})")
    )
  ) |>
  pull(label, name = groupe)

.outcome_text <- c("#666666", opts$color$cold[2])

### FIG ------------------------------------------------------------------------

fig_outcome <- .outcome_data |>
  ggplot() +
  aes(x = cures, y = n_pat, fill = groupe) +
  geom_col(alpha = 0.8, width = 0.9) +
  geom_text(
    mapping = aes(label = n_pat, color = groupe),
    vjust = -0.6,
    size = 2.2,
    family = opts$font$alpha
  ) +
  facet_grid(
    rows = vars(phase),
    cols = vars(groupe),
    labeller = labeller(groupe = .outcome_strata)
  ) +
  labs(
    x = "Nombre de cures reçues",
    y = "Nombre de patients"
  ) +
  scale_x_continuous(
    breaks = seq(0, 20, by = 2),
    expand = expansion(mult = c(0.025, 0.075))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = .fig_palette) +
  scale_color_manual(values = .outcome_text) +
  theme_bar(
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey95"),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(size = 8, face = "bold"),
    axis.text = element_text(size = 7)
  )

### OUTPUT ---------------------------------------------------------------------

easy_out(fig_outcome, height = 5, width = 6)
