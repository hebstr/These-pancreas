### DATA -----------------------------------------------------------------------

.base <- df |>
  mutate(
    pt = row_number(),
    induc = coalesce(induc_nb, 0),
    adj = coalesce(adj_nb, 0),
    total = induc + adj
  ) |>
  filter(total > 0) |>
  mutate(pt = fct_reorder(factor(pt), total))

.lanes <- bind_rows(
  .base |> transmute(pt, groupe, phase = "Induction", x = 0, xend = induc),
  .base |> transmute(pt, groupe, phase = "Adjuvant", x = induc, xend = induc + adj)
) |>
  filter(xend > x) |>
  mutate(phase = factor(phase, c("Induction", "Adjuvant")))

.markers <- .base |>
  select(pt, groupe, induc, adj, matches("_adapt_c\\d+$")) |>
  pivot_longer(
    matches("_adapt_c\\d+$"),
    names_to = c("phase", "cp"),
    names_pattern = "(induc|adj)_adapt_c(\\d+)",
    values_to = "mag"
  ) |>
  filter(mag %in% c("<=20%", ">20%")) |>
  mutate(
    cp = as.numeric(cp),
    x = if_else(phase == "induc", cp, induc + cp),
    error = cp > if_else(phase == "induc", induc, adj),
    mark = factor(
      case_when(
        error ~ "post dernière cure",
        mag == "<=20%" ~ "≤ 20 %",
        mag == ">20%" ~ "> 20 %"
      ),
      levels = c("≤ 20 %", "> 20 %", "post dernière cure")
    )
  )

### FIG ------------------------------------------------------------------------

fig_ttt <- ggplot() +
  geom_swim_lane(
    data = .lanes,
    mapping = aes(x = x, xend = xend, y = pt, colour = phase),
    linewidth = 1.8
  ) +
  geom_swim_marker(
    data = .markers,
    mapping = aes(x = x, y = pt, marker = mark),
    size = 5
  ) +
  scale_color_manual(
    name = "Phase",
    values = c(Induction = opts$color$cold[2], Adjuvant = opts$color$base)
  ) +
  scale_marker_discrete(
    name = "Adaptation de dose",
    glyphs = c("x", "x", "◇"),
    colours = c("#FF9900", opts$color$warm[2], "black"),
    limits = c("≤ 20 %", "> 20 %", "post dernière cure")
  ) +
  facet_wrap(~groupe, scales = "free_y", space = "free_y") +
  scale_x_continuous(
    breaks = seq(0, max(.lanes$xend), 2),
    expand = expansion(c(0, 0.02))
  ) +
  labs(x = "Cure", y = NULL) +
  theme(
    text = element_text(family = opts$font$alpha),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

easy_out(fig_ttt, height = 8)
