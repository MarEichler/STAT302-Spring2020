county_map <- function(map_data, fill_var, fill_color, legend_name, perc_min, perc_max, area_name){

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
      plot.title = element_text(size = 30, hjust = 0.5)
      , legend.title = element_text(size = 20)
      , legend.text = element_text(size = 14)
      , legend.key.width = unit(.8, "cm")
      , legend.key.height = unit(.8, "cm")
    )
}


