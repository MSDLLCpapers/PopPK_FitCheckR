# Entry point for shinyapps.io / Posit Connect / shiny::runApp()
library(PopPKFitCheckR)

Sys.setenv(TZ = "America/New_York")

shiny::shinyApp(ui = app_ui(), server = app_server)
