# Source the UI and server components
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)