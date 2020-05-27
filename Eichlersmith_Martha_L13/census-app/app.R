#lesson 5
library(maps)
library(tidyverse)
source("func_statemap.R")

counties <- readRDS("data/counties.rds")  %>%
    separate(name, c("state", "county"), ",")

counties_map <- map_data("county") %>% 
    select(long, lat, group, county = subregion, state = region) %>% 
    left_join(., counties, by = c("county" = "county",  "state" = "state"))


# User interface ----
ui <- fluidPage(
    titlePanel("censusVis"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Create demographic maps with 
        information from the 2010 US Census."),
            
            selectInput("state", label = "Select a state or the contiguous 48 states to display", 
                        choices = state.name, selected = "South Dakota"),
            
            selectInput("var", 
                        label = "Choose a variable to display",
                        choices = c("Percent White", "Percent Black",
                                    "Percent Hispanic", "Percent Asian"),
                        selected = "Percent White"),
            
            sliderInput("range", 
                        label = "Range of interest:",
                        min = 0, max = 100, value = c(0, 100))
        ),
        
        mainPanel(plotOutput("map"))
    )
)

# Server logic ----
server <- function(input, output) {
    
    dataInput <- reactive({
        counties_map %>%
            filter(state == tolower(input$state))
    })
    
    output$map <- renderPlot({
        args <- switch(input$var,
                       "Percent White" = list(dataInput()$white, "darkgreen", "% White"),
                       "Percent Black" = list(dataInput()$black, "dodgerblue", "% Black"),
                       "Percent Hispanic" = list(dataInput()$hispanic, "darkorange", "% Hispanic"),
                       "Percent Asian" = list(dataInput()$asian, "darkviolet", "% Asian"))
        
        args$perc_min <- input$range[1]
        args$perc_max <- input$range[2]
        args$map_data <- dataInput() 
        args$state_name <- input$state
        
        
        do.call(state_map, args)
    })
}

# Run app ----
shinyApp(ui, server)