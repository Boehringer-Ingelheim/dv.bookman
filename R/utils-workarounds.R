# Shiny does not expose the location of the bookmark directory, except through indirect methods and
# in reactive time (see https://community.rstudio.com/t/get-directory-of-shiny-bookmark/115616)
# Here's a quick hack to get that information jumping through a minor undocumented hoop.
# This could be hardened by checking for the existence of shiny:::saveInterfaceLocal and making sure
# it is never employed when running on a server (i.e. when nchar(Sys.getenv('SHINY_PORT')) == 0)
get_shiny_bookmark_dir <- function() {
  res <- NULL

  default_save_interface <- import_internal_and_avoid_an_R_CMD_check_warning("shiny", "saveInterfaceLocal")
  save_interface <- shiny::getShinyOption("save.interface", default = default_save_interface)
  temp_dummy_folder <- "__bookman__" # empty string not enough because shiny server complains if folder exists
  save_interface(
    id = temp_dummy_folder,
    callback = function(location) {
      try(file.remove(location))
      res <<- dirname(location)
    }
  )
  return(res)
}

# request$session does not expose the HTTP request path, so we have to recover
# something that should be known at initialization time in reactive time instead
get_shiny_bookmark_url_prefix <- function(session) {
  url_bookmark_prefix <- shiny::reactiveVal()
  url_bookmark_capture_observe <- shiny::observe({
    client_data <- shiny::reactiveValuesToList(session$clientData)
    shiny_url <- paste0(client_data$url_protocol, "//", client_data$url_hostname)
    if (!is.null(client_data$url_port) && nchar(client_data$url_port) > 0) {
      shiny_url <- paste0(shiny_url, ":", client_data$url_port)
    }

    url_bookmark_prefix(paste0(shiny_url, client_data$url_pathname, "?_state_id_="))

    url_bookmark_capture_observe$destroy()
  })
  return(url_bookmark_prefix)
}
