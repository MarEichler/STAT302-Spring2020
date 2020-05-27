library(tidyverse)
## DATA 
counties <- readRDS("Eichlersmith_Martha_L13/census-app/data/counties.rds")  %>%
  separate(name, c("state", "county"), ",")


counties_map <- map_data("county") %>% 
  select(long, lat, group, county = subregion, state = region) %>% 
  left_join(., counties, by = c("county" = "county",  "state" = "state"))




## VARIABLES 

map_data <- counties_map
fill_var <- counties_map$white 
legend_name <- "White %"
fill_color = "darkgreen"

perc_min <- 0
perc_max <- 100

area_name = "Contiguous 48 States, Counties"



#################




perc_breaks <- seq(perc_min, perc_max, (perc_max-perc_min)/4)
perc_labels <- paste(perc_breaks, "%", sep = "")
if(perc_max < 100){perc_labels[5] <- paste(perc_labels[5], "or more")}




ggplot(map_data, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = fill_var)) +
  borders("state", colour = "white") +
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
  ) +
  coord_map("conic", lat0=30) +
  theme_void() +
  theme(
      plot.title = element_text(size = 24, hjust = 0.5)
    , legend.title = element_text(size = 16)
    , legend.text = element_text(size = 12)
  )



