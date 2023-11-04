#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(leaflet)
library(shiny)
library(dplyr)
library(sf)
library(FAOSTAT)
library(wbstats)
library(tmaptools)
library(ggplot2)
library(ggrepel)
library(data.table)
library(plotly)

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
             h3(HTML("<b>a) Interpretations of positive correlation between GDP and agricultural:</b>")),
             p("A positive correlation between GDP and food, crops, and vegetable production in selected countries would imply a direct relationship where higher economic output (GDP) aligns with increased agricultural productivity. Here are some potential indications or implications of a positive correlation:"),
             br(),
             tags$div(tags$ol(
               tags$li(HTML("<b>Economic Contribution:</b> It could suggest that the agricultural sector significantly contributes to the overall economic growth of these countries. Higher agricultural output might be a fundamental driver of economic expansion.")),
               tags$li(HTML("<b>Employment and Income Generation:</b> A positive correlation might indicate that agriculture is a significant source of employment and income for these countries. A thriving agricultural sector could mean more jobs and increased income for the population.")),
               tags$li(HTML("<b>Food Security:</b> Higher agricultural production linked to GDP might imply increased food security within these countries. It suggests a nation's ability to meet its population's nutritional needs and potentially export surplus produce to other regions.")),
               tags$li(HTML("<b>Diversification and Resilience:</b> A positive correlation may indicate that these countries have a more diversified and resilient economy, as they do not solely rely on non-agricultural sectors for growth. A strong agricultural base can stabilize the economy against fluctuations in other industries.")),
               tags$li(HTML("<b>Technology and Innovation:</b> High-GDP countries with a positive correlation might be using technological advancements and innovation in agriculture, leading to increased productivity and higher-quality produce.")),
               tags$li(HTML("<b>Export Potential:</b> It could also mean that these countries have the capacity to export surplus agricultural products, contributing to international trade and potentially improving their balance of trade."))
             )),
             br(),
             p("These indications might not be applicable universally to all the countries in the dataset. A positive correlation between GDP and agricultural production signifies a strong relationship, indicating the significance of the agricultural sector in the economic development of these countries. However, a comprehensive analysis is necessary to comprehend the specifics and reasons behind these relationships within each country."),
             br(),
             h3(HTML("<b>b) Interpretations of negative correlation between GDP and agricultural:</b>")),
             p("A negative correlation between GDP and food, crops, and vegetables production in selected countries would suggest an inverse relationship between the economic output (GDP) and the volume or quality of agricultural production."),
             p("Here are a few potential indications or implications of a negative correlation:"),
             tags$div(tags$ol(
               tags$li(HTML("<b>Economic Structure:</b> It might suggest that countries with higher GDP levels are less reliant on agriculture for their economic growth. Instead, they might have more diversified economic structures with a greater emphasis on manufacturing, services, or other industries.")),
               tags$li(HTML("<b>Import Dependency:</b> A negative correlation could indicate that high-GDP countries might be relying more on imports for their food supply rather than producing it domestically. They may prioritize other sectors over agriculture due to higher labor costs, land scarcity, or technological advancements in other industries.")),
               tags$li(HTML("<b>Technological Advancements:</b> Wealthier nations might use technological advancements and innovations to streamline agricultural processes, leading to increased productivity with fewer resources. As a result, they could produce more with less land and labor compared to lower GDP countries.")),
               tags$li(HTML("<b>Market Dynamics:</b> High-GDP countries may have different consumption patterns. They might focus more on higher-value agricultural products, such as specialty crops or organic foods, which could result in smaller quantities produced but with higher market value.")),
               tags$li(HTML("<b>Environmental Impact:</b> In some cases, a negative correlation might suggest that high-GDP countries have shifted focus away from traditional agriculture due to environmental concerns, urbanization, or land conservation efforts, impacting the overall agricultural production levels."))
             )),
             br(),
             p("These indications can vary and might not apply universally to all the countries in the dataset. Negative correlations between GDP and agricultural production can point towards complex socio-economic, technological, and market-driven dynamics, and further in-depth analysis is essential to understand the specific reasons behind such relationships within each country."),
             navbarPage("Graphs",
                        tabPanel("Map",
                                 leafletOutput("dashboard")),
                        tabPanel("Correlation",
                                 plotOutput("corr")),
                        tabPanel("Time Series",
                                 plotlyOutput("fpiseries"),
                                 plotlyOutput("gdpseries")),
                        tabPanel("Tables",
                                 tableOutput("dataset"))),
             flowLayout(
               radioButtons("item",label = h4("Food Production"), choices = c("Crops"="Crops", "Food"="Food", "Vegetables and Fruit Primary"="Vegetables and Fruit Primary"), selected="Crops"),
               sliderInput("year_select",label = h4("Year Selection"), min = 1985, max = 2020, value = c(1985,2020)),
               checkboxGroupInput("cc", label = h4("Country"), inline = TRUE, choices = c("United States"="United States", "United Kingdom"="United Kingdom",
                                                                            "United Arab Emirates"="United Arab Emirates", "China"="China", "Indonesia"="Indonesia",
                                                                            "Japan"="Japan", "Malaysia"="Malaysia", "Myanmar"="Myanmar", "Singapore"="Singapore",
                                                                            "Sri Lanka"="Sri Lanka", "Thailand"="Thailand", "Viet Nam"="Viet Nam"),selected = "United States")
             ),
             br(),
             actionButton("refresh_data", "Refresh datasets")),

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
  vec_breaks <- c(     0,        1)
  vec_rgb    <- c("#b51224", "#005c37")

  corr.df <- reactive({
    case_when(
      input$item == "Crops" ~ corr_table(input$year_select[1], input$year_select[2])$crop,
      input$item == "Food" ~ corr_table(input$year_select[1], input$year_select[2])$food,
      input$item == "Vegetables and Fruit Primary" ~ corr_table(input$year_select[1], input$year_select[2])$veg,
      .default = NA)
  })

  ts.df <- reactive({
    ts_table(input$year_select[1], input$year_select[2], input$cc, input$item)
  })

  # dashboard
  output$dashboard <- renderLeaflet({
    locs <- readRDS("data/coord.rds")
    df <- corr.df()
    for(i in 1:length(df)){
      df$colour[i] <- vec_rgb[min(which(vec_breaks > df$correlation[i]))]
    }
    df <- merge(df, locs, by = "ISO3", all.x = TRUE) %>% st_as_sf()
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Pearson Coeff.",
      df$Country, round(df$correlation,2)
    ) %>% lapply(HTML)
    leaflet(df) %>% addTiles() %>% setView( lat=10, lng=0 , zoom=2) %>%
      addPolygons(fillColor = ~colour, weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "2",
                  fillOpacity = 0.85,
                  highlightOptions = highlightOptions(
                    weight = 1.3,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 1,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
  })

  # correlation
  output$corr <- renderPlot({
    ggplot(corr.df(), aes(x=correlation, y=avg_gdp, fill = Country)) +
      geom_point(aes(size = avg_gdp), shape = 21) +
      geom_text_repel(aes(label=Country), size=5) +
      theme(legend.position = "none")
  })

  # time series
  output$fpiseries <- renderPlotly({
    p1 <- ggplot(ts.df(), aes(x=Year, y=FPI, color = Country)) +
      geom_line() +
      ggtitle("Time Series of Indices") +
      theme(legend.position = "top")
    ggplotly(p1)
  })

  output$gdpseries <- renderPlotly({
    p2 <- ggplot(ts.df(), aes(x=Year, y=GDP, color = Country)) +
      geom_line() +
      theme(legend.position = "top")
    ggplotly(p2)
  })

  # table
  output$dataset <- renderTable({
    corr.df()
  })

  # refresh datasets
  observeEvent(input$refresh_data,{
    update_fao_data()
    clean_fao_data()
    update_wdi_data()
    update_main_data()
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Datasets are downloading')
  })
}

# Run the application
shinyApp(ui=ui, server=server)
