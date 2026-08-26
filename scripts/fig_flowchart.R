### DATA -----------------------------------------------------------------------

df_flow <- sheets$exclusions |>
  select(centre, motif_exclusion) |>
  bind_rows(sheets$inclusions[, "centre"]) |>
  mutate(
    across(c(centre, motif_exclusion), as.numeric),
    motif_exclusion = replace_na(motif_exclusion, 0)
  ) |>
  set_value_labels(!!!dict$val, .strict = FALSE) |>
  modify_if(is.labelled, labelled::to_factor)

label_centre <- get_label(df_label, centre)

label_exclus <- df_flow |>
  filter(motif_exclusion != 0) |>
  get_label(motif_exclusion)

n_groupe <- fct_count(df_label$groupe)

n <- lst(
  eligible = nrow(df_flow),
  exclus = sum(df_flow$motif_exclusion != 0),
  inclus = eligible - exclus,
  pct_exclus = style_pct(exclus / eligible),
  pct_inclus = style_pct(inclus / eligible),
  chimio_adj = n_groupe$n[1],
  chimio_periop = n_groupe$n[2],
  pct_chimio_adj = style_pct(chimio_adj / inclus),
  pct_chimio_periop = style_pct(chimio_periop / inclus)
)

### GROBS ----------------------------------------------------------------------

fig_size <- lst(width = 11.5, height = 8)

col_blue <- "#08F"
col_dark <- "#111"
col_grey <- "grey95"
base_fontsize <- 11
base_textcolor <- "#333"

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
    box_gp = gpar(fill = fill, col = col, lwd = lwd),
    txt_gp = txt
  )
}

### FIG ------------------------------------------------------------------------

fig_flowchart <- with_fig_device(
  width = fig_size$width,
  height = fig_size$height,
  code = {
    eligible_box <- flow_box(
      str_glue(
        "Éligibilité (N = {n$eligible})
        - Patients >= 18 ans
        - Diagnostic d’adénocarcinome pancréatique
        - Indication chirurgicale retenue en RCP
        - Date opératoire entre 01/2022 et 12/2025"
      ),
      x = 0.4,
      y = 0.88,
      just = "left",
      col = col_dark
    )

    exclus_box <- flow_box(
      str_glue("Exclusion (n = {n$exclus}, {n$pct_exclus})\n{label_exclus}"),
      x = 0.77,
      y = 0.74,
      just = "left",
      col = col_grey,
      fill = col_grey,
      lwd = 1.2,
      txt = txt_gp(base_fontsize - 1.5, col = "#555")
    )

    coords_exclus_box <- coords(exclus_box)
    anchor_exclus_box <- boxGrob(
      label = "",
      x = coords_exclus_box$left,
      y = coords_exclus_box$y * 1,
      width = unit(0, "mm"),
      height = unit(0, "mm"),
      box_gp = gpar(col = NA, fill = NA)
    )
    anchor_exclus_connect <- connectGrob(
      eligible_box,
      anchor_exclus_box,
      type = "L",
      lty_gp = gpar(col = "grey90", lwd = 2.5)
    )

    inclus_box <- flow_box(
      str_glue("Inclusion (n = {n$inclus}, {n$pct_inclus})\n{label_centre}"),
      x = 0.4,
      y = 0.6,
      just = "left"
    )
    inclus_connect <- connectGrob(
      eligible_box,
      inclus_box,
      type = "vertical",
      lty_gp = gpar(col = base_textcolor, lwd = 1.5)
    )

    adjuvant_box <- flow_box(
      label = str_glue(
        "Chimiothérapie adjuvante seule\n(n = {n$chimio_adj}, {n$pct_chimio_adj} des inclus)"
      ),
      x = 0.25,
      y = 0.35
    )
    adjuvant_connect <- connectGrob(inclus_box, adjuvant_box, type = "N")

    periop_box <- flow_box(
      label = str_glue(
        "Chimiothérapie péri-opératoire\n(n = {n$chimio_periop}, {n$pct_chimio_periop} des inclus)"
      ),
      x = 0.55,
      y = 0.35
    )
    periop_connect <- connectGrob(inclus_box, periop_box, type = "N")

    grobTree(
      eligible_box,
      exclus_box,
      inclus_box,
      adjuvant_box,
      periop_box,
      anchor_exclus_connect,
      inclus_connect,
      adjuvant_connect,
      periop_connect
    )
  }
)

easy_out(
  x = fig_flowchart,
  width = fig_size$width,
  height = fig_size$height
)

export_office(
  x = fig_flowchart,
  width = fig_size$width,
  height = fig_size$height
)
