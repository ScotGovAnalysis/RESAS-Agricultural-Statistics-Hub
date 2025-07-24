### training app script - demo-ing module use in shiny app
# Source the UI and server components
library(here)
source(here("Training", "training ui.R"))
source(here("Training", "training server.R"))

# Run the app
shinyApp(ui = ui, server = server)