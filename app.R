#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(leaflet)
library(shiny)
library(dplyr)
library(sf)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(HTML("<b>Datathon Submission</b>")),
  titlePanel(h4(HTML("<b>Team Name</b><p>3 November 2023</p>"))),

  navlistPanel(
    widths = c(2,10),
    "Table of Content:",
    tabPanel("Problem Statement",
             h1("Problem Statement"),
             p("Place text here..Lorem Ipsum is simply dummy text of the printing and typesetting industry.
               Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,
               when an unknown printer took a galley of type and scrambled it to make a type specimen book.
               It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged.
               It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages,
               and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.")),
    tabPanel("Methodology",
             h1("Problem Statement"),
             p("Place text here..Lorem Ipsum is simply dummy text of the printing and typesetting industry.
               Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,
               when an unknown printer took a galley of type and scrambled it to make a type specimen book.
               It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged.
               It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages,
               and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."),
             img(src = "methods.png")),
    tabPanel("Results & Map",
             h1("Result & Map"),
             p("Discussion here..Lorem Ipsum is simply dummy text of the printing and typesetting industry.
               Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,
               when an unknown printer took a galley of type and scrambled it to make a type specimen book.
               It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged.
               It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages,
               and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."),
             br(),
             p("Put dashboards here..."),
             leafletOutput("dashboard")),
    tabPanel("About us",
             h1("About us"),
             p("Discussion here..Lorem Ipsum is simply dummy text of the printing and typesetting industry.
               Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,
               when an unknown printer took a galley of type and scrambled it to make a type specimen book.
               It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged.
               It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages,
               and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session){
  # tab panel 1
  output$dashboard <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
  })
}

# Run the application
shinyApp(ui=ui, server=server)
