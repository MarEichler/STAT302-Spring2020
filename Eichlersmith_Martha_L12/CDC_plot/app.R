library(shiny)
library(tidyverse)
library(skimr)

# https://stackoverflow.com/questions/48106504/r-shiny-how-to-display-choice-label-in-selectinput 
# https://shiny.rstudio.com/reference/shiny/1.4.0/tabsetPanel.html

cdc <- read_delim(file = "cdc.txt", delim = "|") %>%
  mutate(
    genhlth = factor(
      genhlth
      , levels = c("excellent", "very good", "good", "fair", "poor")
      , labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")
    )
    , hlthplan = factor(
      hlthplan
      , levels = c(0, 1)
      , labels = c("No", "Yes")
    )
    , exerany = factor(
      exerany
      , levels = c(0, 1)
      , labels = c("No", "Yes")
    )
    , smoke100 = factor(
      smoke100
      , levels = c(0, 1)
      , labels = c("No", "Yes")
    )
    , gender = factor(
      gender
      , levels = c("f", "m")
      , labels = c("Female", "Male")
    )
  )

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("CDC BRFSS Histograms"),
  
  sidebarLayout(
    position = "right", 
    
    sidebarPanel(
      
      selectInput("x_var", label = "Select Variable:",
                  choices = list(
                      "Actual Weight" = "weight"
                    , "Desired Weight" = "wtdesire"
                    , "Height" = "height"
                  ),  selected = "weight"
          ), #end of select input 
      
      
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30,
                  animate = animationOptions(interval = 1000, loop = FALSE)
        ),  #end of slider input 
      
      radioButtons("fill_var", label = "Select Fill/Legend Variable", 
                   choices = list(
                       "General Health" = "genhlth"
                     , "Health Coverage" = "hlthplan"
                     , "Exercised in Past Month" = "exerany"
                     , "Smoked 100 Cigarettes" = "smoke100"
                     , "Gender" = "gender"
                     , "None" = "None"
                     ), selected = "genhlth"
          ) #end of radio buttons 
      
    ), #end of side bar panel 
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Summary", verbatimTextOutput("summary_x"), verbatimTextOutput("summary_fill")) 
      ) #end tabsetPanel 
    ) #end mainPanel
  ) #end sidebar layout 
) # end fluid page 

fill_names <- c(
  "General Health" = "genhlth"
  , "Health Coverage" = "hlthplan"
  , "Exercised in Past Month" = "exerany"
  , "Smoked 100 Cigarettes" = "smoke100"
  , "Gender" = "gender"
)

x_names <- c(
  "Actual Weight in Pounds" = "weight"
  , "Desired Weight in Pounds" = "wtdesire"
  , "Height in Inches" = "height"
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

#PLOT OUTPUT   
  output$plot <- renderPlot({
    
    #other option 
    #var_hist <- case_when(input$var_x == "Actual Weight" ~ pull(cdc, weight))
    
    #x_labe. <- case_when(input$var_x == "Actual Weight" ~ "Actual Weight in Pounds")
    
    fill <- cdc[[input$fill_var]]
    fill_name <- names(fill_names)[fill_names == input$fill_var]
    
    
    x <- cdc[[input$x_var]]
    x_name <- names(x_names)[x_names == input$x_var]
    
    
      base_plot <- ggplot(cdc, aes(x)) + 
        scale_fill_discrete(name = NULL) +
        labs(
          x =  x_name 
          , y = "Count"
          , subtitle = fill_name
        ) +
        theme_minimal() +
        theme(
          plot.subtitle = element_text(hjust = 0.5)
          , legend.position = "top"
          , plot.background = element_rect(fill = "grey95", color = NA)
          , panel.grid.major = element_line(color = "grey80")
        ) 
      
      if (input$fill_var == "None") {
        base_plot + geom_histogram(
            fill = "skyblue"
          , color = "black"
          , bins = input$bins
        )
      } else {
        base_plot + geom_histogram(
          aes(fill = fill)
          , color = "black"
          , bins = input$bins
        )
      } #end of if else 
      
  }) #end of renderplot
  
############ END OF PLOT OUTPUT
  
# SUMMARY X OUTPUT
  output$summary_x <- renderPrint({ 
    print(skim(cdc, input$x_var), include_summary = FALSE, strip_metadata = TRUE)
  }) #end of render
  
########### END OF SUMMARY X OUTPUT 
  
  # SUMMARY FILL OUTPUT
  output$summary_fill <- renderPrint({
    if (input$fill_var == "None") {
      X <- NULL 
    } else {
    print(skim(cdc, input$fill_var), include_summary = FALSE, strip_metadata = TRUE)
    } #end of if else 
  }) #end of render
  
########### END OF SUMMARY FILL  OUTPUT 
  
} #end of server 

shinyApp(ui = ui, server = server)

# rsconnect::deployApp('Eichlersmith_Martha_L12/CDC_plot')
 