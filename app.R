#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(leaflet)
library(shiny)
library(dplyr)
library(sf)
library(MASS)
library(psych)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(HTML("<b>Datathon Submission</b>")),
  titlePanel(h4(HTML("<b>Team Name</b><p>3 November 2023</p>"))),

  navlistPanel(
    widths = c(2,10),
    "Table of Content:",
    tabPanel("Problem Statement & Intro",
             h1("Introduction"),
             p("Agriculture is a critical sector of economic development and stability, offering more than just their traditional role of food production and resource provision. Agriculture comprises a wide range of activities such as farming, forestry, and livestock production, all of which not only supply food and resources to the population but also contribute to the economy in a variety of ways. First, agriculture generates employment opportunities, as it involves multiple sectors, including cultivation, processing, and transportation. An increase in employment opportunities would result in higher levels of goods and services being created, leading to increased production and economic activity. This, in turn, leads to the growth of Gross Domestic Product (GDP) by stimulating both the supply and demand sides of the economy. Second, agriculture brings a significant contribution to GDP. This is because, the revenues obtained from the production of crops, livestock, and other agricultural products is counted as part of the country’s GDP. Furthermore, as technology and modern farming practices have advanced, agriculture has become more efficient, resulting in larger yields and more productive use of resources. This, in turn, leads to an increase of GDP growth, which is critical to a country's economic progress. Hence, realizing the importance of the agriculture sector to economic growth of a country, this study aims to study the relationship between three agricultural aspects which are food, crops and vegetable and fruit production and the primary indicators of economic growth, namely the Gross Domestic Product (GDP)."),
             br(),
             h1("Problem Statement"),
             p("The problem statement revolves around investigating the correlation between the Gross Domestic Product (GDP) and three key agricultural aspects - food production, crops production, and vegetable & fruits production - in a selection of countries: China, Indonesia, Japan, Malaysia, Myanmar, Singapore, Sri Lanka, Thailand, the United Kingdom (UK), United Arab Emirates (UAE), United States of America (USA), and Vietnam from year 1985 to 2020."),
             tags$figure(class = "centerFigure", tags$img(src = "probstat.png", height = 300, width = 750, alt = "Objective is to see whether food production drives GDP."), tags$figcaption("Source: Google Images")),
             br(),
             p("The objective of this project is to explore and analyze the relationship between the economic output (GDP) of these countries and their agricultural sectors' performance in producing food, various crops, and fruits & vegetables. Hence, this project aims to determine whether a correlation exists between a country's GDP and the volume or quality of food produced, different types of crops cultivated, and the production of fruits and vegetables within the specified countries."),
             tags$figure(class = "centerFigure", tags$img(src = "sdg.png", height = 300, width = 750, alt = "Evidence on agricultural contribution helps policymaker establish sound decision."), tags$figcaption("Source: Google Images")),
             br(),
             p("Understanding these correlations can provide insights into the agricultural contribution to the economic wealth of these nations, potentially identifying patterns or associations between GDP growth and agricultural productivity. This study could be instrumental in guiding policies, strategies, and investment decisions to improve agricultural output and its impact on the overall economic performance of the listed countries."),
             ),

    tabPanel("Methodology",
             h1("Methodology"),
             p("When investigating the correlation between GDP and gross production in a methodology section, several key elements should be included to ensure the clarity and reproducibility of the research. Here are the specific points to address in the methodology section:"),
             tags$div(tags$ol(
               tags$li(HTML("<b>Research Objective and Hypotheses:</b><p>Clearly state the research aim, which is to explore the correlation between GDP and gross production. Define any specific hypotheses or expectations regarding the relationship between these variables.</p>")),
               tags$li(HTML("<b>Data Sources and Collection:</b><p>Detail the specific sources from which GDP and gross production data were obtained. This may include government reports, national statistical agencies, international databases (such as the World Bank, IMF, or other reputable sources), or any other relevant sources. Mention the time frame or period covered by the data.</p>")),
               tags$li(HTML("<b>Variables and Measurements:</b><p>Clearly define GDP and gross production as variables, specifying how they were measured or calculated. This might involve detailing the components included in GDP (consumption, investment, government spending, and net exports) and explaining what's encompassed in the gross production metric.</p>")),
               tags$li(HTML("<b>Country Selection and Sample:</b><p>Justify the selection of countries for the study, outlining the criteria for inclusion. This might include factors like geographic representation, economic diversity, or data availability. Detail the list of countries included and why they are pertinent to the study.</p>")),
               tags$li(HTML("<b>Data Analysis Techniques:</b><p>Explain the statistical methods used to analyze the correlation between GDP and gross production. For correlation analysis, mention the specific statistical tools used (such as the Pearson correlation coefficient) and justify the choice of these methods.</p>")),
               tags$li(HTML("<b>Data Processing and Preprocessing:</b><p>Describe how the raw data were processed, organized, and cleaned to ensure accuracy and consistency. Mention any adjustments or transformations applied to the data before conducting the correlation analysis.</p>")),
               tags$li(HTML("<b>Assumptions and Limitations:</b><p>Address any assumptions made during the analysis, and discuss the limitations of the study. These might include data constraints, potential biases, assumptions in the correlation analysis, or other factors that might affect the interpretation of results.</p>")),
               tags$li(HTML("<b>Validity and Reliability:</b><p>Discuss the steps taken to ensure the validity and reliability of the analysis, such as cross-verification of data sources, sensitivity analysis, or other validation techniques.</p>")),
               tags$li(HTML("<b>Ethical Considerations:</b><p>Address any ethical considerations taken into account during data collection and analysis, especially when using sensitive or proprietary data.</p>")),
               tags$li(HTML("<b>Research Procedure Timeline:</b><p>Provide a timeline indicating the sequence of steps in the research process, from data collection to analysis and interpretation.</p>"))
             )),
             tags$figure(class = "centerFigure", tags$img(src = "methods.png", height = 300, width = 750), tags$figcaption("Source: Google Images"))),

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
             navbarPage("Graphs",
                        tabPanel("Map",
                                 leafletOutput("dashboard")),
                        tabPanel("Charts",
                                 plotOutput("corr")),
                        tabPanel("Tables",
                                 tableOutput("dataset")))),

    tabPanel("Datasets & References",
             withMathJax(),
             h1('Datasets & References'),
             p("Lists below are the datasets & references that we used to produced the map & report:"),
             br(),
             p(class = 'hangingindent', "Food Production Indicators, Food and Agricultural Organization of the United Nations", tags$a(href = "https://www.fao.org/faostat/en/#data/QI", "[link]")),
             p(class = 'hangingindent', "DOSM (Department of Statistics Malaysia) ", tags$a(href = "https://www.dosm.gov.my/", "[link]"))),

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

  # dashboard
  output$dashboard <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
  })

  # correlation
  output$corr <- renderPlot({
    test <- mvrnorm(200, mu = c(2,5,3), Sigma = matrix(c(10,5,2,10,5,2,4,4,1), ncol=3))
    colnames(test) <- c("x","y","z")
    test %>% pairs
  })

  # table
  output$dataset <- renderTable({
    df <- read.csv("data/corrGDP_FPI.csv")
    colnames(df) <- c("Country","Production","Correlation")
    df
  })
}

# Run the application
shinyApp(ui=ui, server=server)
