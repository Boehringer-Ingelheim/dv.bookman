test_that(
  "save/load",
  {
    contents <- data.frame(
      description = character(0), visible = logical(0)
    )
    contents <- rbind(contents, list(description = "B", visible = FALSE))
    contents <- rbind(contents, list(description = "A", visible = TRUE))

    path <- tempfile(fileext = ".rds")
    dv.bookman:::save_bookmark_file(contents = contents, path = path)

    contents_loaded <- dv.bookman:::load_bookmark_file(path)

    contents_sorted <- contents[order(contents$visible, decreasing = TRUE), ]

    expect_identical(contents_sorted, contents_loaded)
  }
)

test_that(
  "initial listing with no bookmarks" |>
    vdoc[["add_spec"]](c(specs$storage)),
  {
    bookmark_folder <- set_up_mock_bookmark_folder(bookmark_names = c())

    info <- dv.bookman:::list_bookmarks(bookmark_folder, "bookmark_info.rds")
    expect_identical(nrow(info$visible), 0L)
    expect_identical(nrow(info$hidden), 0L)
  }
)

test_that(
  "initial listing with bookmarks" |>
    vdoc[["add_spec"]](c(specs$storage)),
  {
    bookmark_folder <- abc_bookmark_folder
    unlink(file.path(bookmark_folder, "bookmark_info.rds"))

    info <- dv.bookman:::list_bookmarks(bookmark_folder, "bookmark_info.rds")
    expect_identical(nrow(info$visible), 0L)
    expect_identical(nrow(info$hidden), 3L)
    expect_identical(info$hidden$COL_id, c("a", "b", "c"))
  }
)

test_that(
  "bookmark description and visibility updates" |>
    vdoc[["add_spec"]](c(specs$description, specs$visibility)),
  {
    bookmark_folder <- abc_bookmark_folder
    unlink(file.path(bookmark_folder, "bookmark_info.rds"))

    info <- dv.bookman:::list_bookmarks(abc_bookmark_folder, "bookmark_info.rds")
    dv.bookman:::update_record(bookmark_folder, "a", description = "this is A")
    dv.bookman:::update_record(bookmark_folder, "b", description = "this is B", toggle_visible = TRUE)
    info <- dv.bookman:::list_bookmarks(bookmark_folder, "bookmark_info.rds")
    expect_identical(info$visible$COL_id, c("b"))
    expect_identical(info$visible$COL_description, c("this is B"))
    expect_identical(info$hidden$COL_id, c("a", "c"))
    expect_identical(info$hidden$COL_description, c("this is A", ""))
  }
)

test_that(
  "bookmark reordering" |>
    vdoc[["add_spec"]](c(specs$order)),
  {
    bookmark_folder <- abc_bookmark_folder
    unlink(file.path(bookmark_folder, "bookmark_info.rds"))

    info <- dv.bookman:::list_bookmarks(bookmark_folder, "bookmark_info.rds")
    dv.bookman:::update_record(bookmark_folder, "a", toggle_visible = TRUE)
    dv.bookman:::update_record(bookmark_folder, "b", toggle_visible = TRUE)
    info <- dv.bookman:::list_bookmarks(bookmark_folder, "bookmark_info.rds")
    expect_identical(info$visible$COL_id, c("a", "b"))
    dv.bookman:::move_record(bookmark_folder, row_id = "a", delta = 1)
    info <- dv.bookman:::list_bookmarks(bookmark_folder, "bookmark_info.rds")
    expect_identical(info$visible$COL_id, c("b", "a"))
  }
)
