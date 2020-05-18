#MINNESOTA MAP 
library(shiny)
library(tidyverse)
library(rvest) #for scraping data from mn web 
library(maps)


mn <- map_data("county", "minnesota") %>% 
  select(long, lat, group, id = subregion)


url <- "https://www.minnesota-demographics.com/counties_by_population"
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")

tbls_ls <- webpage %>%
  html_nodes("table") %>%
  html_table()

pop_mn_counties <- tbls_ls[[1]][-88, ] %>%
  mutate(
    Population = as.numeric(gsub(",", "",  Population))
    , County = tolower(gsub(" County", "", County))
    , County = ifelse(County == "st. louis", "st louis", County)
    , perc_pop = Population / sum(Population)
  ) %>%
  janitor::clean_names() 

mn_pop_map <- mn %>%
  left_join(., pop_mn_counties, by = c("id" = "county"))

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
    ggplot(mn_pop_map, aes(x = long, y = lat, fill = perc_pop)) +
      geom_polygon(aes(group = group) , color = "grey35") +
      scale_fill_continuous(
        name = "Percent of Population"
        , breaks = c(0.05, 0.10, 0.15, 0.20)
        , labels = c("5%", "10%", "15%", "20%")
      ) +
    coord_quickmap() +
      theme_void()
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)