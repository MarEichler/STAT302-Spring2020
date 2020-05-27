library(tidyverse)
## DATA 
counties <- readRDS("Eichlersmith_Martha_L13/census-app/data/counties.rds")  %>%
  separate(name, c("state", "county"), ",")






map_data <- map_data("county") %>% 
  select(long, lat, group, county = subregion, state = region) %>% 
  left_join(., counties, by = c("county" = "county",  "state" = "state"))

state_name <- "Georgia" 

map_data <-  map_data %>%
  filter(state == tolower(state_name))


## VARIABLES 

perc_min <- 0
perc_max <- 50



fill_var <- map_data$white 
legend_name <- "White %"
fill_color = "darkgreen"

#################




perc_breaks <- seq(perc_min, perc_max, (perc_max-perc_min)/4)
perc_labels <- paste(perc_breaks, "%", sep = "")
if(perc_max < 100){perc_labels[5] <- paste(perc_labels[5], "or more")}


ggplot(map_data, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = white)) +
  ggtitle(state_name) + 
  borders("state", state_name, colour = "black") +
  scale_fill_gradient(
      name = legend_name
    , high = fill_color
    , low = "white"
    , na.value = "grey50"
    , limits = c(perc_min, perc_max)
    , breaks = perc_breaks 
    , labels = perc_labels 
    , oob = scales::squish
  ) +
  coord_quickmap() +
  theme_void() +
  theme(
      plot.title = element_text(size = 24, hjust = 0.5)
    , legend.title = element_text(size = 16)
    , legend.text = element_text(size = 12)
  )



