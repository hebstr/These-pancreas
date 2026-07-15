### GOOGLE SHEETS BACKUP -------------------------------------------------------

backup_dir <- paste("backup", Sys.time()) |> str_to_kebab()
backup_dir <- paste0(".", backup_dir)

fs::dir_create(backup_dir)

openxlsx2::write_xlsx(sheets$variables, fs::path(backup_dir, "variables.xlsx"))
openxlsx2::write_xlsx(
  sheets$inclusions,
  fs::path(backup_dir, "inclusions.xlsx")
)
openxlsx2::write_xlsx(
  sheets$exclusions,
  fs::path(backup_dir, "exclusions.xlsx")
)
