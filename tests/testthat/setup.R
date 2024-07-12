is_CI <- isTRUE(as.logical(Sys.getenv("CI")))

# validation (S)
vdoc <- local({
  #                      ##########
  # package_name is used # INSIDE # the sourced file below
  #                      ##########
  package_name <- "dv.bookman" # hardcoded because devtools::check appears to use a different working directory
  utils_file_path <- system.file("validation", "utils-validation.R", package = package_name, mustWork = TRUE)
  source(utils_file_path, local = TRUE)[["value"]]
})
specs <- vdoc[["specs"]]
#  validation (F)

set_up_mock_bookmark_folder <- function(bookmark_names) {
  bookmark_folder <- tempfile(pattern = "bookmarks_")
  dir.create(bookmark_folder)

  # bookmarks added in reverse to make first in bookmark_names most recent
  for (i in rev(seq_along(bookmark_names))) {
    path <- file.path(bookmark_folder, bookmark_names[[i]])
    dir.create(path, recursive = TRUE)
    file.create(file.path(path, "input.rds"))
    if (i > 1) Sys.sleep(0.05)
  }
  return(bookmark_folder)
}

abc_bookmark_folder <- set_up_mock_bookmark_folder(bookmark_names = c("a", "b", "c"))
