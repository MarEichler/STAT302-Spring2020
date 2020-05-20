#test
library(shiny)
library(tidyverse)

shinyApp(
  ui = fluidPage(
    varSelectInput("variable", "Variable:", mtcars),
    plotOutput("data")
  ),
  server = function(input, output) {
    output$data <- renderPlot({
      ggplot(mtcars, aes(!!input$variable)) + geom_histogram()
    })
)

shinyApp(ui, server)