### training server script - demo-ing module use in shiny app

###----------------------------
### Define main app server and call in server from training module
server <- function(input, output, session) {
  trainingServer("cattle_test")}