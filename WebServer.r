#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-25
# The web server based GUI for the Trust model Simulator

library(shiny)

source("TrustModel.r")

ui <- fluidPage(
    titlePanel("Trust Model Simulator")
)

server <- function(input, output) {

}

shinyApp(ui=ui, server=server)
