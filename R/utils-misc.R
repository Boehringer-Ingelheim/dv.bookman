# string substitution (poor man's glue)
# > ssub("substitute THIS and THAT", THIS="_this_", THAT="_that_")
#   "substitute _this_ and _that_"
ssub <- function(s, ...) {
  pairs <- list(...)
  for (orig in names(pairs)) {
    dest <- pairs[[orig]]
    s <- gsub(paste0("\\<", orig, "\\>"), dest, s)
  }
  s
}

"%||%" <- function(x, y) if (is.null(x)) y else x # TODO: Remove once R >= 4.4.0 is widespread

set_hover_info <- function(title, text) {
  paste(
    title,
    shiny::tags$i(
      class = "glyphicon glyphicon-info-sign",
      style = "color:#0072B2;",
      title = text
    ) |> as.character()
  )
}

import_internal_and_avoid_an_R_CMD_check_warning <- `:::`
