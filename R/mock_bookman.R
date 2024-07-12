module_UI <- function(prefix) {
  ns <- shiny::NS(prefix)
  result <- shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput(ns("letters"), "Columns", choices = c("A", "B", "C"), multiple = TRUE)
    ),
    shiny::mainPanel(
      shiny::textOutput(ns("text"))
    )
  )

  return(result)
}

module_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output[["text"]] <- shiny::renderText(paste(input[["letters"]], collapse = ", "))
    }
  )
}

mod_mock <- function(module_id) {
  mod <- list(
    ui = function(mod_id) {
      module_UI(mod_id)
    },
    server = function(afmm) {
      module_server(module_id)
    },
    module_id = module_id
  )
  return(mod)
}

mock_app <- function() {
  app_UI <- function(request) {
    shiny::fluidPage(
      module_UI("first"),
      module_UI("second"),
      shiny::bookmarkButton(),
      shiny::hr(),
      bookman_UI("book")
    )
  }

  app_server <- function(input, output, session) {
    module_server("first")
    module_server("second")
    bookman_server("book", bookmark_dir = NULL)
  }

  app <- shiny::shinyApp(
    ui = app_UI,
    server = app_server,
    enableBookmarking = "server"
  )

  shiny::runApp(app)
}

mock_app_mm <- function() {
  dataset <- list(adsl = data.frame(USUBJID = c("a", "b")))

  module_list <- list(
    "Tab Label" = mod_mock(module_id = "mod1"),
    "Tab Label 2" = mod_mock(module_id = "mod2"),
    "Bookmarks" = mod_bookman()
  )

  dv.manager::run_app(
    data = list("DS" = dataset),
    module_list = module_list,
    filter_data = "adsl"
  )
}
