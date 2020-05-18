#MINNESOTA MAP 
library(shiny)
library(tidyverse)
library(maps)

mn <- map_data("county", "minnesota") %>% 
  select(long, lat, group, id = subregion)

# Define UI ----
ui <- fluidPage(
  titlePanel("Minnesota"),
  
  sidebarLayout(position = "left",
                sidebarPanel(
                    h3("Fun Facts:")
                  , strong("State Nickname:"), " Land of 10,000 Lakes",  br()
                  , strong("State Motto:"), " The Star of the North", br()
                  , strong("Population:"), "5.576 millsion (2017 est.)", br()
                  , strong("State Flag:")
                  , img(src = "flag.png", width = "80%"), br()
                  , strong("State Bird:"), " Loon"
                  , img(src = "loon.jpg", width = "80%")
                ),
                mainPanel(
                    plotOutput(outputId = "statemap")
                  , tags$ul(
                    tags$li("Minnesota was admitted as the 32nd state on May 11, 1858"), 
                    tags$li("Minnesota is ranked 12th in largest area and 22nd in population; nearly 55% of residents live in the Twin Cities metro area"), 
                    tags$li("Minnesota is known for a population that enjoys playing and watching hockey"), 
                    tags$li( "Minnesota still has a Virginia confederate flag that was caputred during the Battle of Gettysburg in 1863; Virigina has asked for the flag to be returned multiple times but ",
                        tags$a(href = "https://www.twincities.com/2017/08/20/minnesota-has-a-confederate-symbol-and-it-is-going-to-keep-it/"
                                      , "Minnesota has refused")
                              ), 
                    tags$li("Learn more on ", tags$a(href="https://en.wikipedia.org/wiki/Minnesota", "Wikipedia"))
                  )
                )
  )
)
# Define server logic ----
server <- function(input, output) {
  output$statemap <-  renderPlot({
  ggplot(mn, aes(x = long, y = lat)) +
    geom_polygon(aes(group = group), fill = "#ADD8E6" , color = "grey35") +
    coord_quickmap() +
    theme_void()
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)