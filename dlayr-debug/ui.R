library(shiny)
library(leaflet)
library(RColorBrewer)

bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("route", "Route",
                            c("1","60", "All")),
                selectInput("direction", "Direction",
                            c("Inbound" = "I", "Outbound" = "O", "All")))                            
)