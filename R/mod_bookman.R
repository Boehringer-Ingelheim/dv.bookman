#' dv.bookman module UI.
#'
#' @param id Unique shiny string ID. Should match the ID used in \code{bookman_server()}.
#'
#' @export
bookman_UI <- function(id = "_bookman") {
  ns <- shiny::NS(id)

  td_css <- "table td.with_placeholder:empty:before { content: '<missing description>'; color: gray; }"

  res <- shiny::tabsetPanel(
    id = ns("tabset"), type = "tabs",
    shiny::tabPanel(
      "List",
      shiny::br(),
      shiny::div(style = "width:50%;margin:auto", DT::DTOutput(ns("ID_list_table")))
    ),
    shiny::tabPanel("Configure", shiny::tagList(
      fontawesome::fa_html_dependency(),
      shiny::tags$head(
        shiny::tags$style(shiny::HTML(td_css))
      ),
      shiny::br(),
      shiny::div(style = "width:70%;margin:auto", DT::DTOutput(ns("ID_configure_table_visible"))),
      shiny::br(), shiny::br(),
      shiny::div(style = "width:70%;margin:auto", DT::DTOutput(ns("ID_configure_table_hidden")))
    ))
  )

  return(res)
}

BOOKMARK_FILE_VERSION <- 1
BOOKMARK_INFO_FILENAME <- "bookmark_info.rds"

save_bookmark_file <- function(contents, path) {
  decorated_contents <- list(version = BOOKMARK_FILE_VERSION, data = contents)

  tmp_path <- tempfile(tmpdir = dirname(path), fileext = ".tmp")
  saveRDS(decorated_contents, file = tmp_path)
  file.rename(from = tmp_path, to = path) # ensure atomic operation
}

load_bookmark_file <- function(path) {
  decorated_contents <- readRDS(path)

  if (decorated_contents$version != BOOKMARK_FILE_VERSION) {
    # map contents to new file format
    if (decorated_contents$version == 0) {
      # change 'hidden' column to 'visible'
      colnames(decorated_contents$data) <- c("description", "visible")
      decorated_contents$data$visible <- !decorated_contents$data$visible
    } else {
      shiny::validate(
        shiny::need(
          decorated_contents$version == BOOKMARK_FILE_VERSION,
          "stored bookmark description file format incompatible with this bookmark management module"
        )
      )
    }
  }

  # sink hidden items to the bottom because it:
  # - keeps visible items contiguous, which simplifies row reorder operations
  # - makes newly visible bookmarks be the last on the list when saving
  data <- decorated_contents$data
  decorated_contents$data <- data[order(data$visible, decreasing = TRUE), ]

  return(decorated_contents$data)
}

list_bookmarks <- function(bookmark_dir, file_name) {
  # List all subfolders that contain an 'input.rds'
  suffix <- "input.rds"
  if (!dir.exists(bookmark_dir)) {
    return(errorCondition(paste("bookmark folder", bookmark_dir, "does not exist")))
  }
  fnames <- list.files(path = bookmark_dir, pattern = paste0("\\<", suffix, "$"), recursive = TRUE)
  bmk_names <- dirname(fnames)

  # Build table of bookmarks
  full_paths <- file.path(bookmark_dir, fnames)
  all <- data.frame(COL_id = bmk_names)
  all["COL_date"] <- file.info(full_paths)$ctime

  # Load descriptions
  bookman_fname <- file.path(bookmark_dir, file_name)
  if (!file.exists(bookman_fname)) {
    empty_description_file_contents <- data.frame(
      description = character(0), visible = logical(0)
    )
    save_bookmark_file(contents = empty_description_file_contents, path = bookman_fname)
  }
  info <- load_bookmark_file(bookman_fname)

  # Sort table so that:
  # - Visible items appear in the same order as listed in description file
  # - Rest of items appear below, sorted from newer to older
  shown_info <- info[info[["visible"]], ]
  featured_indices <- match(rownames(shown_info), all[["COL_id"]])
  featured_indices <- featured_indices[!is.na(featured_indices)]
  nonfeatured_indices <- setdiff(seq_len(nrow(all)), featured_indices)

  featured <- all[featured_indices, ]
  rest <- all[nonfeatured_indices, ]
  rest <- rest[order(rest$COL_date, decreasing = TRUE), ]
  res <- rbind(featured, rest)

  row_count <- nrow(res)
  # Write description and visible info
  # initial values
  res["COL_description"] <- rep("", row_count)
  res["COL_visible"] <- rep(FALSE, row_count)
  # values recovered from bookmark info file
  mapping <- match(rownames(info), res[["COL_id"]])
  res[mapping, ][["COL_description"]] <- info$description
  res[mapping, ][["COL_visible"]] <- info$visible

  # Add time creation column in human-readable format
  res["COL_elapsed_time"] <- character(0)
  time_diff <- difftime(Sys.time(), res[["COL_date"]], units = "mins")
  for (i in seq_along(time_diff)) {
    if (time_diff[i] < 60) {
      elapsed <- paste(round(time_diff[i]), "minutes ago")
    } else if (time_diff[i] < 24 * 60) {
      elapsed <- paste(round(time_diff[i] / 60), "hours ago")
    } else if (time_diff[i] < 30 * 24 * 60) {
      elapsed <- paste(round(time_diff[i] / (24 * 60)), "days ago")
    } else if (time_diff[i] < 365 * 24 * 60) {
      elapsed <- paste(round(time_diff[i] / (30 * 24 * 60)), "months ago")
    } else {
      elapsed <- paste(round(time_diff[i] / (365 * 24 * 60)), "years ago")
    }
    res[i, ][["COL_elapsed_time"]] <- elapsed
  }

  res <- list(
    visible = utils::head(res, n = length(featured_indices)),
    hidden = utils::tail(res, n = length(nonfeatured_indices))
  )

  return(res)
}

update_record <- function(bookmark_dir, row_id, description = NULL, toggle_visible = FALSE) {
  bookman_fname <- file.path(bookmark_dir, BOOKMARK_INFO_FILENAME)
  info <- load_bookmark_file(bookman_fname)

  visible <- FALSE
  if (row_id %in% rownames(info)) {
    row <- info[row_id, ]
    if (is.null(description)) description <- row[["description"]]
    visible <- row[["visible"]]
  } else {
    if (is.null(description)) description <- ""
  }

  if (toggle_visible) visible <- !visible

  info[row_id, ] <- list(description = description, visible = visible)
  save_bookmark_file(contents = info, path = bookman_fname)
}

move_record <- function(bookmark_dir, row_id, delta) {
  bookman_fname <- file.path(bookmark_dir, BOOKMARK_INFO_FILENAME)
  info <- load_bookmark_file(bookman_fname)

  row <- which(rownames(info) == row_id)
  shiny::req(length(row) == 1) # just in case bookmark files decide to disappear

  described_row_count <- nrow(info)

  clamp <- function(min, v, max) min(max(min, v), max)
  row_i <- clamp(1, row, described_row_count)
  row_j <- clamp(1, row + delta, described_row_count)
  if (row_i != row_j) {
    # swap rows
    info[c(row_i, row_j), ] <- info[c(row_j, row_i), ]
    rownames(info)[c(row_i, row_j)] <- rownames(info)[c(row_j, row_i)]
    save_bookmark_file(contents = info, path = bookman_fname)
  }
}




#' dv.bookman module server.
#'
#' @param id Unique shiny string ID. Should match the ID used in \code{bookman_UI()}.
#' @param bookmark_dir Path to the bookmark folder of the application. Autodetected if `NULL`
#'
#' @export
bookman_server <- function(id = "_bookman", bookmark_dir = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session[["ns"]]

      if (is.null(bookmark_dir)) {
        bookmark_dir <- get_shiny_bookmark_dir()
        message(paste("[bookman]: Autodetected bookmark directory:", bookmark_dir))
      }

      # Failsafe mechanism ----
      # Prevent losing bookmark descriptions in case of programming error.
      # Makes a copy of the bookmark description file that will be removed at the end of
      # the session unless descriptions are modified.
      # The copy should allow to roll back to a known state in case of catastrophic failure.
      # Feel free to remove it if you trust this code more than I do.
      local({
        bookman_complete_path <- file.path(bookmark_dir, BOOKMARK_INFO_FILENAME)
        if (file.exists(bookman_complete_path)) {
          session_timestamp <- strftime(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")
          bookmark_backup_fname <- file.path(
            bookmark_dir,
            paste0(BOOKMARK_INFO_FILENAME, "-", session$token, ".", session_timestamp)
          )
          file.copy(from = bookman_complete_path, to = bookmark_backup_fname)

          remove_redundant_bookmark_info_file <- function() {
            if (file.mtime(bookman_complete_path) < file.mtime(bookmark_backup_fname)) {
              file.remove(bookmark_backup_fname)
            }
          }
          shiny::onSessionEnded(remove_redundant_bookmark_info_file)
        }
      })

      url_bookmark_prefix_rv <- get_shiny_bookmark_url_prefix(session)

      # Main data reactive ----
      reload_data <- shiny::reactiveVal(FALSE)
      data <- shiny::reactive({
        reload_data()
        input[["ID_refresh"]]
        res <- list_bookmarks(bookmark_dir, BOOKMARK_INFO_FILENAME)

        # clumsy attempt at avoiding exceptions in internally developed routines
        if (inherits(res, "condition")) {
          shiny::validate(
            shiny::need(
              !inherits(res, "condition"),
              res$message
            )
          )
        }

        # add the restore bookmark button column
        for (i_table in seq_along(res)) {
          row_count <- nrow(res[[i_table]])
          res[[i_table]][["COL_restore_links"]] <- rep("", row_count)
          restore_urls <- paste0(url_bookmark_prefix_rv(), res[[i_table]][["COL_id"]])
          for (i_row in seq_len(row_count)) {
            res[[i_table]][i_row, ][["COL_restore_links"]] <- as.character(
              shiny::tags$button(
                list(shiny::icon("arrow-up-right-from-square"), "Load"),
                class = "btn btn-default",
                onclick = paste0('window.open("', restore_urls[[i_row]], '", "_blank")')
              )
            )
          }
        }

        res
      })

      # Bookmark listing ----
      output[["ID_list_table"]] <- DT::renderDT({
        caption <- "Featured bookmarks"
        data <- data()[["visible"]]

        empty_table_help_message <-
          '&mdash; There are no featured bookmarks yet. Visit the "Configure" tab above to select some &mdash; '

        cols <- c(` ` = "COL_description", ` ` = "COL_restore_links")
        data <- data[cols]

        res <- DT::datatable(
          data,
          colnames = names(cols),
          selection = "none", rownames = FALSE, escape = FALSE,
          options = list(
            dom = "t", processing = FALSE, ordering = FALSE,
            language = list(zeroRecords = empty_table_help_message),
            columnDefs = list(
              list(targets = 0, class = "with_placeholder"),
              list(targets = 1, class = "dt-right")
            )
          ),
          caption = shiny::tags$caption(style = "caption-side: top; text-align: center; font-size:150%;", caption)
        )
        return(res)
      })

      # Featured bookmark configuration table ----
      visible_table_data <- shiny::reactive({
        caption <- "Featured bookmarks"
        data <- data()[["visible"]]
        row_count <- nrow(data)

        data[["COL_reorder_buttons"]] <- rep("", row_count)
        data[["COL_hide_button"]] <- rep("", row_count)

        for (i in seq_len(row_count)) {
          # reorder columns
          up <- shiny::tags$button(
            shiny::icon("caret-up"),
            class = "btn btn-default",
            onclick = ssub("Shiny.setInputValue('INPUT_ID', {delta: -1, row_id: 'ROW_ID'}, {priority: 'event'});",
              INPUT_ID = ns("ID_action_reorder"), ROW_ID = data[["COL_id"]][[i]]
            )
          )

          down <- shiny::tags$button(
            shiny::icon("caret-down"),
            class = "btn btn-default",
            onclick = ssub("Shiny.setInputValue('INPUT_ID', {delta: +1, row_id: 'ROW_ID'}, {priority: 'event'});",
              INPUT_ID = ns("ID_action_reorder"), ROW_ID = data[["COL_id"]][[i]]
            )
          )
          data[i, ][["COL_reorder_buttons"]] <- paste(as.character(shiny::tagList(up, down)))

          # hide column
          data[i, ][["COL_hide_button"]] <- as.character(
            shiny::tags$button(
              shiny::icon("angles-down"),
              class = "btn btn-default",
              onclick = ssub("Shiny.setInputValue('INPUT_ID', {row_id: 'ROW_ID'}, {priority: 'event'});",
                INPUT_ID = ns("ID_toggle_visibility"), ROW_ID = data[["COL_id"]][[i]]
              )
            )
          )
        }

        cols <- c("COL_elapsed_time", "COL_description", "COL_reorder_buttons", "COL_hide_button", "COL_restore_links")
        data <- data[cols]
        return(data)
      })

      output[["ID_configure_table_visible"]] <- DT::renderDT({
        caption <- "Featured bookmarks"

        cols <- character(0)
        cols[["Created"]] <- "COL_elapsed_time"
        cols[["Description" |> set_hover_info("Double-click to edit")]] <- "COL_description"
        cols[[
          "Reorder" |> set_hover_info("Order of bookmarks is reflected on the 'List' tab ")
        ]] <- "COL_reorder_buttons"
        cols[["Hide" |> set_hover_info("Exclude bookmark from the 'List' tab")]] <- "COL_hide_button"
        cols[[" "]] <- "COL_restore_links"

        data <- shiny::isolate(visible_table_data())

        empty_table_help_message <- paste(
          "&mdash; No bookmark selected for display.",
          "Use the 'Feature' buttons on the bottom table to select some &mdash;"
        )

        res <- DT::datatable(
          data,
          colnames = names(cols),
          selection = "none", rownames = FALSE, escape = FALSE,
          editable = list(target = "cell", disable = list(columns = c(0, 2, 3, 4))),
          options = list(
            dom = "t", ordering = FALSE, processing = FALSE,
            language = list(zeroRecords = empty_table_help_message),
            columnDefs = list(
              list(targets = 1, class = "with_placeholder"),
              list(targets = c(2, 3), class = "dt-center"),
              list(targets = 4, class = "dt-right")
            )
          ),
          caption = shiny::tags$caption(style = "caption-side: top; text-align: center; font-size:150%;", caption)
        )
        return(res)
      })

      visible_table_proxy <- DT::dataTableProxy("ID_configure_table_visible")
      shiny::observe({
        data <- visible_table_data()
        DT::replaceData(visible_table_proxy, data, rownames = FALSE)
      })

      # Hidden bookmark configuration table ----
      hidden_table_data <- shiny::reactive({
        caption <- "Hidden bookmarks"
        data <- data()[["hidden"]]
        row_count <- nrow(data)

        # feature column
        data[["COL_show_button"]] <- rep("", row_count)
        for (i in seq_len(nrow(data))) {
          data[i, ][["COL_show_button"]] <- as.character(
            shiny::tags$button(
              shiny::icon("angles-up"),
              class = "btn btn-default",
              onclick = ssub("Shiny.setInputValue('INPUT_ID', {row_id: 'ROW_ID'}, {priority: 'event'});",
                INPUT_ID = ns("ID_toggle_visibility"), ROW_ID = data[["COL_id"]][[i]]
              )
            )
          )
        }

        cols <- c("COL_elapsed_time", "COL_description", "COL_show_button", "COL_restore_links")
        data <- data[cols]
        return(data)
      })

      output[["ID_configure_table_hidden"]] <- DT::renderDT({
        caption <- "Hidden bookmarks"

        cols <- character(0)
        cols[["Created"]] <- "COL_elapsed_time"
        cols[["Description" |> set_hover_info("Double-click to edit")]] <- "COL_description"
        cols[["Feature" |> set_hover_info("Include bookmark in the 'List' tab")]] <- "COL_show_button"
        cols[[" "]] <- "COL_restore_links"

        data <- shiny::isolate(hidden_table_data())

        empty_table_help_message <- paste(
          "&mdash; There are no hidden bookmarks.",
          "Create a new bookmark and it will appear here &mdash;"
        )

        res <- DT::datatable(
          data,
          colnames = names(cols),
          selection = "none", rownames = FALSE, escape = FALSE,
          editable = list(target = "cell", disable = list(columns = c(0, 2, 3))),
          options = list(
            dom = "t", ordering = FALSE, processing = FALSE,
            language = list(zeroRecords = empty_table_help_message),
            columnDefs = list(
              list(targets = 1, class = "with_placeholder"),
              list(targets = 2, class = "dt-center"),
              list(targets = 3, class = "dt-right")
            )
          ),
          caption = shiny::tags$caption(style = "caption-side: top; text-align: center; font-size:150%;", caption)
        )
        return(res)
      })

      hidden_table_proxy <- DT::dataTableProxy("ID_configure_table_hidden")
      shiny::observe({
        data <- hidden_table_data()
        DT::replaceData(hidden_table_proxy, data, rownames = FALSE)
      })

      # Action callbacks ----
      ## Cell edit ----
      DT_hardcoded_cell_edit_suffix <- "_cell_edit"
      visible_cell_edit_id <- paste0("ID_configure_table_visible", DT_hardcoded_cell_edit_suffix)
      shiny::observeEvent(input[[visible_cell_edit_id]], {
        edit_info <- input[[visible_cell_edit_id]]
        row_id <- data()[["visible"]][["COL_id"]][[edit_info$row]]
        update_record(bookmark_dir, row_id, description = edit_info$value)
        reload_data(shiny::isolate(reload_data()) + 1)
      })
      hidden_cell_edit_id <- paste0("ID_configure_table_hidden", DT_hardcoded_cell_edit_suffix)
      shiny::observeEvent(input[[hidden_cell_edit_id]], {
        edit_info <- input[[hidden_cell_edit_id]]
        row_id <- data()[["hidden"]][["COL_id"]][[edit_info$row]]
        update_record(bookmark_dir, row_id, description = edit_info$value)
        reload_data(shiny::isolate(reload_data()) + 1)
      })

      ## Row reorder ----
      shiny::observeEvent(input[["ID_action_reorder"]], {
        row_id <- input[["ID_action_reorder"]][["row_id"]]
        delta <- input[["ID_action_reorder"]][["delta"]]
        move_record(bookmark_dir, row_id, delta)
        reload_data(reload_data() + 1)
      })

      ## Row visibility toggle ----
      shiny::observeEvent(input[["ID_toggle_visibility"]], {
        row_id <- input[["ID_toggle_visibility"]][["row_id"]]
        update_record(bookmark_dir, row_id, toggle_visible = TRUE)
        reload_data(reload_data() + 1)
      })

      # Module bookmark behavior ----
      # Remove this module's inputs from bookmarking (even those starting with '.')
      shiny::observe(
        shiny::setBookmarkExclude(names(shiny::reactiveValuesToList(input, all.names = TRUE)))
      )

      # refresh listing after local bookmark
      shiny::onBookmark(function(state) reload_data(reload_data() + 1))
    }
  )
}

#' dv.bookman DaVinci module.
#'
#' See this vignette for more details: \code{vignette("manual", package = "dv.bookman")}
#'
#' @param module_id Unique shiny string ID.
#' @param bookmark_dir Path to the bookmark folder of the application. Autodetected if `NULL`.
#'
#' @export
mod_bookman <- function(module_id = "_bookman", bookmark_dir = NULL) {
  mod <- list(
    ui = function(mod_id) bookman_UI(mod_id),
    server = function(afmm) bookman_server(module_id, bookmark_dir),
    module_id = module_id
  )
  return(mod)
}
