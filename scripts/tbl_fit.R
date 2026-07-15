### TBL FIT SURV ---------------------------------------------------------------

tbl_surv <- list(
  total = .surv$total$os$tbl,
  strata = .surv$strata$os$tbl |> add_p()
) |>
  tbl_stack() |>
  gt_format(row_strip = FALSE) |>
  style_strata(
    set_names(.fig_palette, levels(.surv$strata$os$data$tte$obs$strata))
  )

easy_out(tbl_surv, width = 750)

### TBL FIT CUMINC -------------------------------------------------------------

# tbl_cuminc <- list(
#   .cmp$total$tbl,
#   .cmp$strata$tbl |> add_p()
# ) |>
#   tbl_stack() |>
#   modify_indent(columns = label, rows = !row_type %in% 'label') |>
#   gt_format(row_strip = FALSE) |>
#   style_strata(
#     set_names(.fig_palette, levels(.surv$strata$os$data$tte$obs$strata))
#   )

# easy_out(tbl_cuminc, width = 750)
