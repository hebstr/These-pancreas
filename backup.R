### GOOGLE SHEETS BACKUP -------------------------------------------------------

stem <- "backup"

backup_dir <- paste0(".", stem)
backup_subdir <- paste(stem, Sys.time()) |> stringr::str_to_kebab()
backup_subdir <- fs::path(backup_dir, backup_subdir)

fs::dir_create(backup_subdir)

purrr::iwalk(
  sheets,
  ~ openxlsx2::write_xlsx(
    .x,
    fs::path(backup_subdir, stringr::str_glue("{.y}.xlsx"))
  )
)
