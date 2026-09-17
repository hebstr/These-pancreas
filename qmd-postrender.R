output_dir <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR")

if (!nzchar(output_dir)) {
  cli::cli_abort(
    "{.envvar QUARTO_PROJECT_OUTPUT_DIR} is unset: this runs as a post-render script."
  )
}

project_dir <- Sys.getenv("QUARTO_PROJECT_DIR", unset = here::here())
at_root <- fs::path_real(output_dir) == fs::path_real(project_dir)

copied <- fs::path(output_dir, "output")

if (fs::dir_exists(copied)) {
  if (at_root) {
    cli::cli_abort(c(
      "{.file {copied}} is the project's own output tree, not a Quarto copy.",
      i = "The render wrote to the project root: check {.field output-dir} in {.file _quarto.yml}."
    ))
  }

  fs::dir_delete(copied)
}

nojekyll <- fs::path(output_dir, ".nojekyll")

if (!at_root && fs::file_exists(nojekyll)) {
  fs::file_delete(nojekyll)
}
