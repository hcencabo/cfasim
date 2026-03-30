# =============================================================================
# app.R — Entry point
# Run with: shiny::runApp("path/to/cfa_samplesize_app")
#
# Required packages (install once):
#   install.packages(c(
#     "shiny", "simsem", "lavaan", "ggplot2", "DT",
#     "shinycssloaders", "shinyjs",
#     "future", "promises", "furrr"   # <-- parallel execution
#   ))
# =============================================================================

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
