### GOOGLE SHEETS BACKUP -------------------------------------------------------

backup_dir <- paste("backup", Sys.time()) |> str_to_kebab()
backup_dir <- paste0(".", backup_dir)

fs::dir_create(backup_dir)

iwalk(
  sheets,
  ~ openxlsx2::write_xlsx(.x, fs::path(backup_dir, str_glue("{.y}.xlsx")))
)
