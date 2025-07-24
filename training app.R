### training app script - demo-ing module use in shiny app
# Source the UI and server components
source("Training/training ui.R")
source("Training/training server.R")

# Run the app
shinyApp(ui = ui, server = server)