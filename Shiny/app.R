# Entry point for shiny::runApp() on this directory.
# ui.R is the complete app (data load + ui + server + shinyApp call);
# source() returns its last expression = the shiny app object.
# NOTE: do not add a server.R here - its presence would switch shiny to the
# ui.R/server.R convention and serve a blank page.
source("ui.R", local = TRUE)$value
