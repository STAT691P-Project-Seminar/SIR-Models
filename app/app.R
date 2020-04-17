#runApp(paste(getwd() ,"/app/core", sep=""))

source("core/ui.R")
source("core/server.R")

shinyApp(ui = ui, server = server)