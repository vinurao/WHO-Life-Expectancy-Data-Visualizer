library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(RColorBrewer)

data <- read_csv("C:/Users/vinam/Downloads/archive/Life Expectancy Data.csv")

colnames(data) <- make.names(colnames(data))

# Function to get top or lowest 20 countries by life expectancy
get_countries <- function(data, type = "top") {
  if (type == "top") {
    countries <- data %>%
      group_by(Country) %>%
      summarize(mean_life_expectancy = mean(Life.expectancy, na.rm = TRUE)) %>%
      top_n(20, mean_life_expectancy) %>%
      pull(Country)
  } else {
    countries <- data %>%
      group_by(Country) %>%
      summarize(mean_life_expectancy = mean(Life.expectancy, na.rm = TRUE)) %>%
      top_n(-20, mean_life_expectancy) %>%
      pull(Country)
  }
  return(countries)
}

# Define UI for application
ui <- fluidPage(
  titlePanel("Life Expectancy Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Variable:",
                  choices = c("GDP", "Schooling", "Alcohol", "BMI", "Adult.Mortality")),
      sliderInput("year", "Year:",
                  min = 2000, max = 2015,
                  value = c(2000, 2015)),
      selectInput("plotType", "Plot Type:",
                  choices = c("Scatter Plot", "3D Scatter Plot")),
      selectInput("countryType", "Country Type:",
                  choices = c("Top 20", "Lowest 20")),
      checkboxInput("showReg", "Show Regression Analysis", value = TRUE)
    ),
    mainPanel(
      plotlyOutput("plot"),
      uiOutput("regressionOutput")
    )
  )
)

# Define server logic
server <- function(input, output) {
  filteredData <- reactive({
    country_type <- ifelse(input$countryType == "Top 20", "top", "lowest")
    selected_countries <- get_countries(data, country_type)
    
    data %>%
      filter(Year >= input$year[1] & Year <= input$year[2], Country %in% selected_countries)
  })
  
  output$plot <- renderPlotly({
    if (input$plotType == "Scatter Plot") {
      p <- ggplot(filteredData(), aes_string(x = input$variable, y = "Life.expectancy")) +
        geom_point(aes(color = Country), size = 3) +
        geom_smooth(method = "lm") +
        scale_color_brewer(palette = "Set3") +
        labs(title = paste("Life Expectancy vs", input$variable)) +
        theme(legend.position = "bottom", legend.title = element_blank())
      ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0, y = -0.2))
    } else if (input$plotType == "3D Scatter Plot") {
      plot_ly(data = filteredData(), x = ~get(input$variable), y = ~Life.expectancy, z = ~Adult.Mortality,
              color = ~Country, type = "scatter3d", mode = "markers", marker = list(size = 5)) %>%
        layout(title = paste("Life Expectancy vs", input$variable, "in 3D"))
    }
  })
  
  output$regressionOutput <- renderUI({
    if (input$showReg) {
      reg_model <- lm(Life.expectancy ~ get(input$variable), data = filteredData())
      reg_summary <- summary(reg_model)
      
      verbatimTextOutput("regressionSummary")
    } else {
      NULL
    }
  })
  
  output$regressionSummary <- renderPrint({
    reg_model <- lm(Life.expectancy ~ get(input$variable), data = filteredData())
    summary(reg_model)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)