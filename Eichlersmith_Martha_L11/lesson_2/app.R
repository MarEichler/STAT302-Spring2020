library(shiny)
library(tidyverse)



# Define UI ----
ui <- fluidPage(
  titlePanel("Lesson 2"),
  
  sidebarLayout(position = "left",
                sidebarPanel("Check out my Guinea Pigs"),
                mainPanel(
                  h1("Daffodil and Blossom", align = "center"),
                  p("These are my two guinea pigs.  I adopted them in January 2019.", style = "font-family: 'times'; font-si16pt"), 
                  strong("They are are very cute."),
                  br(),
                  code("They have no idea how to code."),
                  div("They do know how to run around the cage and wheek for food.", style = "color:blue"),
                  br(),
                  p("Their favorite foods are",
                    span("cilantro, parsely,", style = "color:green"),
                    "and", 
                    span("carrots", style = "color:orange"), 
                    "."), 
                  img(src = "peegs.jpg", width = "80%")
                )
  )
)
# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)