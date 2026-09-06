### DATA -----------------------------------------------------------------------

n_screening <- 374

motif_non_evalue <- NULL

df_flow <- sheets$exclusions |>
  select(centre, motif_exclusion) |>
  bind_rows(sheets$inclusions[, "centre"]) |>
  mutate(
    across(c(centre, motif_exclusion), as.numeric),
    motif_exclusion = replace_na(motif_exclusion, 0)
  ) |>
  set_value_labels(!!!dict$val, .strict = FALSE) |>
  modify_if(is.labelled, labelled::to_factor)

label_centre <- get_label(df, centre)

label_exclus <- df_flow |>
  filter(motif_exclusion != 0) |>
  get_label(motif_exclusion)

n_groupe <- fct_count(df$groupe)

n <- lst(
  screening = n_screening,
  eligible = nrow(df_flow),
  non_evalue = screening - eligible,
  exclus = sum(df_flow$motif_exclusion != 0),
  inclus = eligible - exclus,
  chimio_adj = n_groupe$n[1],
  chimio_periop = n_groupe$n[2],
  pct_exclus = style_pct(exclus / eligible),
  pct_inclus = style_pct(inclus / eligible),
  pct_chimio_adj = style_pct(chimio_adj / inclus),
  pct_chimio_periop = style_pct(chimio_periop / inclus)
)

stopifnot(n$non_evalue >= 0)

### LABELS ---------------------------------------------------------------------

.flow_labels <- list(
  screening = str_glue(
    "Screening : {n$screening} dossiers
    - Patients >= 18 ans
    - Diagnostic d’adénocarcinome canalaire pancréatique
    - Indication chirurgicale retenue en RCP
    - Date opératoire programmée entre 01/2022 et 12/2025"
  ),
  non_evalue = str_c(
    str_glue(
      "Dossiers non évalués : {n$non_evalue}
      - Patients opérés hors période
      - Doublons"
    ),
    motif_non_evalue,
    sep = "\n"
  ),
  eligible = str_glue("Patients évalués pour éligibilité : {n$eligible} (100 %)"),
  exclus = str_glue(
    "Patients exclus : {n$exclus} ({n$pct_exclus})\n{label_exclus}"
  ),
  inclus = str_glue(
    "Patients inclus : {n$inclus} ({n$pct_inclus})\n{label_centre}"
  ),
  groupe_adjuvant = str_glue(
    "Chimiothérapie adjuvante seule\nn = {n$chimio_adj}\n({n$pct_chimio_adj} des inclus)"
  ),
  groupe_periop = str_glue(
    "Chimiothérapie péri-opératoire\nn = {n$chimio_periop}\n({n$pct_chimio_periop} des inclus)"
  )
)

### GROBS ----------------------------------------------------------------------

fig_size <- lst(width = 11.5, height = 8)

col_blue <- "#08F"
col_dark <- "#111"
col_grey <- "grey95"
base_fontsize <- 10
base_textcolor <- "#333"
side_left <- 0.423
clear_side <- 0.021
gap_trunk <- 0.06

options(
  connectGrob = gpar(col = "#09F", lwd = 1.5),
  connectGrobArrow = arrow(length = unit(0, "mm"))
)

txt_gp <- \(fontsize = base_fontsize, col = base_textcolor) {
  gpar(
    fontsize = fontsize,
    col = col,
    fontfamily = opts$font$alpha
  )
}

flow_box <- \(
  label,
  x,
  y,
  just = "center",
  bjust = "center",
  col = col_blue,
  fill = "#FFF",
  lwd = 1.5,
  txt = txt_gp()
) {
  boxGrob(
    label = label,
    x = x,
    y = y,
    just = just,
    bjust = bjust,
    box_gp = gpar(fill = fill, col = col, lwd = lwd),
    txt_gp = txt
  )
}

side_box <- \(label, y) {
  flow_box(
    label = label,
    x = side_left,
    y = y,
    just = "left",
    bjust = "left",
    col = col_grey,
    fill = col_grey,
    lwd = 1.2,
    txt = txt_gp(base_fontsize - 1.5, col = "#555")
  )
}

box_height <- \(box) {
  coords_box <- coords(box)
  convertY(coords_box$top, "npc", valueOnly = TRUE) -
    convertY(coords_box$bottom, "npc", valueOnly = TRUE)
}

stack_y <- \(heights, gaps) {
  margin <- (1 - sum(heights) - sum(gaps)) / 2
  stopifnot(margin > 0)
  offset <- cumsum(c(0, head(heights, -1) + gaps))
  1 - margin - offset - heights / 2
}

side_connect <- \(from, to) {
  coords_to <- coords(to)
  anchor <- boxGrob(
    label = "",
    x = coords_to$left,
    y = coords_to$y,
    width = unit(0, "mm"),
    height = unit(0, "mm"),
    box_gp = gpar(col = NA, fill = NA)
  )
  connectGrob(
    from,
    anchor,
    type = "L",
    lty_gp = gpar(col = "grey90", lwd = 2.5)
  )
}

### FIG ------------------------------------------------------------------------

.flow_grob <- \() {
  h_trunk <- c(
    box_height(flow_box(.flow_labels$screening, x = 0.4, y = 0.5)),
    box_height(flow_box(.flow_labels$eligible, x = 0.4, y = 0.5)),
    box_height(flow_box(.flow_labels$inclus, x = 0.4, y = 0.5)),
    max(
      box_height(flow_box(.flow_labels$groupe_adjuvant, x = 0.4, y = 0.5)),
      box_height(flow_box(.flow_labels$groupe_periop, x = 0.4, y = 0.5))
    )
  )
  h_side <- c(
    box_height(side_box(.flow_labels$non_evalue, y = 0.5)),
    box_height(side_box(.flow_labels$exclus, y = 0.5))
  )

  gaps <- c(h_side + 2 * clear_side, gap_trunk)
  y_trunk <- stack_y(h_trunk, gaps)
  y_side <- y_trunk[1:2] - h_trunk[1:2] / 2 - gaps[1:2] / 2

  screening_box <- flow_box(
    label = .flow_labels$screening,
    x = 0.4,
    y = y_trunk[1],
    just = "left",
    col = col_dark
  )

  non_evalue_box <- side_box(.flow_labels$non_evalue, y = y_side[1])
  non_evalue_connect <- side_connect(screening_box, non_evalue_box)

  eligible_box <- flow_box(
    label = .flow_labels$eligible,
    x = 0.4,
    y = y_trunk[2],
    just = "left",
    col = col_dark
  )
  eligible_connect <- connectGrob(
    screening_box,
    eligible_box,
    type = "vertical",
    lty_gp = gpar(col = base_textcolor, lwd = 1.5)
  )

  exclus_box <- side_box(.flow_labels$exclus, y = y_side[2])
  exclus_connect <- side_connect(eligible_box, exclus_box)

  inclus_box <- flow_box(
    .flow_labels$inclus,
    x = 0.4,
    y = y_trunk[3],
    just = "left"
  )
  inclus_connect <- connectGrob(
    eligible_box,
    inclus_box,
    type = "vertical",
    lty_gp = gpar(col = base_textcolor, lwd = 1.5)
  )

  adjuvant_box <- flow_box(
    label = .flow_labels$groupe_adjuvant,
    x = 0.25,
    y = y_trunk[4]
  )
  adjuvant_connect <- connectGrob(inclus_box, adjuvant_box, type = "N")

  periop_box <- flow_box(
    label = .flow_labels$groupe_periop,
    x = 0.55,
    y = y_trunk[4]
  )
  periop_connect <- connectGrob(inclus_box, periop_box, type = "N")

  grobTree(
    screening_box,
    non_evalue_box,
    eligible_box,
    exclus_box,
    inclus_box,
    adjuvant_box,
    periop_box,
    non_evalue_connect,
    eligible_connect,
    exclus_connect,
    inclus_connect,
    adjuvant_connect,
    periop_connect
  )
}

fig_flowchart_screening <- with_fig_device(
  width = fig_size$width,
  height = fig_size$height,
  code = .flow_grob()
)

### OUT ------------------------------------------------------------------------

easy_out(
  x = fig_flowchart_screening,
  width = fig_size$width,
  height = fig_size$height,
  pptx = TRUE
)
