#lesson 5
library(shiny)
library(maps)
library(tidyverse)
library(tools)
library(rgeos)
library(maptools)
library(statebins)
source("func_statemap.R")
source("func_countymap.R")
source("func_binmap.R")

counties <- readRDS("data/counties.rds")  %>%
    separate(name, c("state", "county"), ",")

counties_map <- map_data("county") %>% 
    select(long, lat, group, county = subregion, state = region) %>% 
    left_join(., counties, by = c("county" = "county",  "state" = "state"))

bin_data <- counties %>% 
    mutate(
        white.pop = total.pop*white
        , black.pop = total.pop*black
        , hispanic.pop = total.pop*hispanic
        , asian.pop = total.pop*asian
    ) %>% 
    select(state, total.pop, white.pop, black.pop, hispanic.pop, asian.pop) %>%
    drop_na() %>%
    group_by(state) %>%
    summarize_all(sum) %>%
    mutate(
        white = white.pop / total.pop
        , black = black.pop / total.pop
        , hispanic = hispanic.pop / total.pop
        , asian = asian.pop / total.pop
        , state = toTitleCase(state)
    ) %>%
    select(state, white, black, hispanic, asian)

choices <- c(state.name, "Contiguous 48 States", "Contiguous 48 States, Counties")

# User interface ----
ui <- fluidPage(
    titlePanel("censusVis"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Create demographic maps with 
        information from the 2010 US Census."),
            
            selectInput("area", label = "Select a state or the contiguous 48 states to display", 
                        choices = choices, selected = choices[51]),
            
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
        if (input$area %in% state.name) { 
            counties_map %>%
            filter(state == tolower(input$area))
        }
        else if (input$area == choices[52]) {counties_map}
        else {bin_data}
    }) 
    
    genArgs <- reactive({
        
        args <- switch(input$var,
                       "Percent White" = list(dataInput()$white, "darkgreen", "% White"),
                       "Percent Black" = list(dataInput()$black, "dodgerblue", "% Black"),
                       "Percent Hispanic" = list(dataInput()$hispanic, "darkorange", "% Hispanic"),
                       "Percent Asian" = list(dataInput()$asian, "darkviolet", "% Asian"))
        args$perc_min <- input$range[1]
        args$perc_max <- input$range[2]
        args$area_name <- input$area
        args$map_data <- dataInput() 
        args
    })
    
    output$map <- renderPlot({
        if ( input$area %in% state.name ) { do.call(state_map, genArgs()) } 
        else if (input$area == choices[52]) {do.call(county_map, genArgs())}
        else {do.call(bin_map, genArgs())}
    })
}

# Run app ----
shinyApp(ui, server)

## rsconnect::deployApp('Eichlersmith_Martha_L13/census-app', account = "mareichler-nw")