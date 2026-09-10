output_dir <- here::here("docs", "output")

if (fs::dir_exists(output_dir)) {
  fs::dir_delete(output_dir)
}
