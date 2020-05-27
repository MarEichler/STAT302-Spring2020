library(tidyverse)
library(statebins)
library(tools)
## DATA 
counties <- readRDS("Eichlersmith_Martha_L13/census-app/data/counties.rds")  %>%
  separate(name, c("state", "county"), ",")


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


glimpse(bin_data)

## VARIABLES 

map_data <- bin_data
fill_var <- bin_data$white 
legend_name <- "White %"
fill_color = "darkgreen"

perc_min <- 0
perc_max <- 70

area_name = "Contiguous 48 States, Counties"



#################



perc_breaks <- seq(perc_min, perc_max, (perc_max-perc_min)/4)
perc_labels <- paste(perc_breaks, "%", sep = "")
if(perc_max < 100){perc_labels[5] <- paste(perc_labels[5], "or more")}

ggplot(map_data, aes(state = state, fill = fill_var)) +
  geom_statebins() + 
  ggtitle(area_name) + 
  scale_fill_gradient(
      name = legend_name
    , high = fill_color
    , low = "white"
    , na.value = "grey50"
    , limits = c(perc_min, perc_max)
    , breaks = perc_breaks 
    , labels = perc_labels 
    , oob = scales::squish
    , guide = guide_legend(reverse = TRUE)
  ) +
  theme_void() +
  coord_equal() + 
  theme(
      plot.title = element_text(size = 24, hjust = 0.5)
    , legend.title = element_text(size = 16)
    , legend.text = element_text(size = 12)
  )



