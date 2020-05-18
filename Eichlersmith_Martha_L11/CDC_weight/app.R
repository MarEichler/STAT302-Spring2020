library(shiny)
library(tidyverse)

cdc <- read_delim(file = "cdc.txt", delim = "|") %>%
  mutate(genhlth = factor(genhlth,
                          levels = c("excellent", "very good", "good", "fair", "poor"),
                          labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")
  ))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(NULL),
  
  sidebarLayout(
    position = "right", 
    
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    
    ggplot(cdc, aes(weight, fill = gender)) + 
      geom_histogram(
          color = "black"
        , bins = input$bins
      ) +
      scale_fill_discrete(
          name = "Gender"
        , labels = c("Female", "Male")
      ) +
      labs(
          x = "Weight in Pounds"
        , title = "CDC BRFSS Histogram of Weight Grouped by Gender"
      ) +
      theme_minimal() + 
      theme(
          plot.title = element_text(size = 20)
        , legend.position = c(0.55, 0.6)
        , legend.justification = c(1, 0)
        , legend.background = element_rect(fill = NA, color = NA)
      )
    

    
  })
  
}

shinyApp(ui = ui, server = server)