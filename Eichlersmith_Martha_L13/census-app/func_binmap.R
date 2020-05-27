bin_map <- function(map_data, fill_var, fill_color, legend_name, perc_min, perc_max, area_name){
  
  perc_breaks <- seq(perc_min, perc_max, (perc_max-perc_min)/4)
  perc_labels <- paste(perc_breaks, "%", sep = "")
  if(perc_max < 100){perc_labels[5] <- paste(perc_labels[5], "or more")}
  
  ggplot(map_data, aes(state = state, fill = fill_var)) +
    geom_statebins(border_size = 0.5) + 
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
      , guide = guide_legend(reverse = TRUE, override.aes = list(color = "black"))
    ) +
    theme_statebins() +
    coord_equal() + 
    theme(
      plot.title = element_text(size = 30, hjust = 0.5)
      , legend.position = "right"
      , legend.title = element_text(size = 20)
      , legend.text = element_text(size = 14)
      , legend.key.height = unit(.8, "cm")
      , legend.key.width = unit(.8, "cm")
    )

}


