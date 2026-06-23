make_interaction_plots <- function(interaction_results){
  
  partial_interaction_spatial_df_g1 <- interaction_results[[1]]
  
  partial_interaction_spatial_df_g2 <- interaction_results[[2]]
  
  # interaction_1 <- plot_ly(data = partial_interaction_spatial_df_g1 %>% filter(pair_num == 1), 
  #         x = ~var1, y = ~var2, z = ~expm1(yhat),
  #         type = 'scatter3d', mode = 'line', split = ~group,
  #         line = list(width = 1, color = "black" #~expm1(yhat), colorscale = 'Viridis'
  #         )) %>% 
  #   add_lines(data = partial_interaction_spatial_df_g2 %>% filter(pair_num == 1), 
  #             x = ~var1, y = ~var2, z = ~expm1(yhat),
  #             type = 'scatter3d', mode = 'line', split = ~group,
  #             line = list(width = 1, color = "black" #~expm1(yhat), colorscale = 'Viridis'
  #             )) %>%
  #   add_lines(data = partial_interaction_spatial_df_g1 %>% filter(pair_num == 1,
  #                                                                 group %in% c(1, 50)), 
  #             x = ~var1, y = ~var2, z = ~expm1(yhat),
  #             type = 'scatter3d', mode = 'line', split = ~group,
  #             line = list(width = 7, color = "#4575b4" #~expm1(yhat), colorscale = 'Viridis'
  #             )) %>%
  #   add_lines(data = partial_interaction_spatial_df_g2 %>% filter(pair_num == 1,
  #                                                                 group %in% c(1, 50)), 
  #             x = ~var1, y = ~var2, z = ~expm1(yhat),
  #             type = 'scatter3d', mode = 'line', split = ~group,
  #             line = list(width = 7, color = "#d73027" #~expm1(yhat), colorscale = 'Viridis'
  #             )) %>%
  #   layout(#title =  "A)",
  #     scene = list(
  #       xaxis = list(title = "sediment C"),
  #       yaxis = list(title = "littoral fraction"),
  #       zaxis = list(title = "CH<sub>4</sub> emission rate"),
  #       camera = list(
  #         eye = list(x = -1.5, y = -1, z = 1) # Adjust x, y, z for different angles/zoom
  #       )),
  #     showlegend = FALSE)
  
  annotations_depth <- data.frame("x" = c(0.55, 0.4),
                            "y" = c(0.81, 0.30),
                            "z" = c(1, 1),
                            "text" = c("<b>shallow</b>", "<b>deep</b>"),
                            "angle" = c(15, 25))
  
  annotations_sedC <- data.frame("x" = c(0.39, 0.6),
                                  "y" = c(0.47, 0.17),
                                  "z" = c(1, 1),
                                  "text" = c("<b>high C</b>", "<b>low C</b>"),
                                  "angle" = c(-20, -40))
  
  
  interaction_2 <- plot_ly(data = partial_interaction_spatial_df_g1 %>% filter(pair_num == 1), 
                           x = ~var1, y = ~var2, z = ~expm1(yhat),
                           type = 'scatter3d', mode = 'line', split = ~group,
                           line = list(width = 1, color = "black" #~expm1(yhat), colorscale = 'Viridis'
                           )) %>% 
    add_lines(data = partial_interaction_spatial_df_g2 %>% filter(pair_num == 1), 
              x = ~var1, y = ~var2, z = ~expm1(yhat),
              type = 'scatter3d', mode = 'line', split = ~group,
              line = list(width = 1, color = "black" #~expm1(yhat), colorscale = 'Viridis'
              )) %>%
    add_lines(data = partial_interaction_spatial_df_g1 %>% filter(pair_num == 1,
                                                                  group %in% c(1, 50)), 
              x = ~var1, y = ~var2, z = ~expm1(yhat),
              type = 'scatter3d', mode = 'line', split = ~group,
              line = list(width = 7, color = "#4575b4" #~expm1(yhat), colorscale = 'Viridis'
              )) %>%
    add_lines(data = partial_interaction_spatial_df_g2 %>% filter(pair_num == 1,
                                                                  group %in% c(1, 50)), 
              x = ~var1, y = ~var2, z = ~expm1(yhat),
              type = 'scatter3d', mode = 'line', split = ~group,
              line = list(width = 7, color = "#d73027" #~expm1(yhat), colorscale = 'Viridis'
              )) %>%
    add_annotations(data = annotations_depth, x = ~x, y = ~y, z = ~z, text = ~text, textangle = ~angle,
                    font = list(color = "#d73027", size = 14), showarrow = F) %>%
    add_annotations(data = annotations_sedC, x = ~x, y = ~y, z = ~z, text = ~text, textangle = ~angle,
                    font = list(color = "#4575b4", size = 14), showarrow = F) %>%
    add_annotations(text = "Total CH<sub>4</sub> emission rate<br>     (mg CH<sub>4</sub> m<sup>-2</sup> d<sup>-1</sup>)",
                    x = 0.21,
                    y = 0.5,
                    z = 0.3,
                    font = list(size = 15),
                    textangle = 265,
                    showarrow = FALSE) %>%
    plotly::layout(#title =  "B)",
      scene = list(
        xaxis = list(title = "   sediment C<br>(% dry weight)"),
        yaxis = list(title = "mean depth<br>     (m)"),
        zaxis = list(title = ""),
        camera = list(
          eye = list(x = -1.75, y = 2, z = 1) # Adjust x, y, z for different angles/zoom
        )),
      showlegend = FALSE)
  
  # interaction_3 <- plot_ly(data = partial_interaction_spatial_df_g1 %>% filter(pair_num == 3), 
  #                          x = ~var1, y = ~var2, z = ~expm1(yhat),
  #                          type = 'scatter3d', mode = 'line', split = ~group,
  #                          line = list(width = 1, color = "black" #~expm1(yhat), colorscale = 'Viridis'
  #                          )) %>% 
  #   add_lines(data = partial_interaction_spatial_df_g2 %>% filter(pair_num == 3), 
  #             x = ~var1, y = ~var2, z = ~expm1(yhat),
  #             type = 'scatter3d', mode = 'line', split = ~group,
  #             line = list(width = 1, color = "black" #~expm1(yhat), colorscale = 'Viridis'
  #             )) %>%
  #   add_lines(data = partial_interaction_spatial_df_g1 %>% filter(pair_num == 3,
  #                                                                 group %in% c(1, 50)), 
  #             x = ~var1, y = ~var2, z = ~expm1(yhat),
  #             type = 'scatter3d', mode = 'line', split = ~group,
  #             line = list(width = 7, color = "#4575b4" #~expm1(yhat), colorscale = 'Viridis'
  #             )) %>%
  #   add_lines(data = partial_interaction_spatial_df_g2 %>% filter(pair_num == 3,
  #                                                                 group %in% c(1, 50)), 
  #             x = ~var1, y = ~var2, z = ~expm1(yhat),
  #             type = 'scatter3d', mode = 'line', split = ~group,
  #             line = list(width = 7, color = "#d73027" #~expm1(yhat), colorscale = 'Viridis'
  #             )) %>%
  #   layout(#title =  "C)",
  #     scene = list(
  #       xaxis = list(title = "mean depth"),
  #       yaxis = list(title = "littoral fraction"),
  #       zaxis = list(title = "CH<sub>4</sub> emission rate"),
  #       camera = list(
  #         eye = list(x = 1.5, y = -1.5, z = 1) # Adjust x, y, z for different angles/zoom
  #       )),
  #     showlegend = FALSE)
  
  
  # htmlwidgets::saveWidget(interaction_1, "interaction_1.html")
  # webshot2::webshot("interaction_1.html", "interaction_1.png", cliprect = c(100, 100, 750, 650))
  # interaction_1_png <- magick::image_read("interaction_1.png")
  
  htmlwidgets::saveWidget(interaction_2, "interaction_2.html")
  webshot2::webshot("interaction_2.html", "interaction_2.png", zoom = 4, cliprect = c(225, 100, 525, 650))
  interaction_2_png <- magick::image_read("interaction_2.png", density = 1000)
  
  # htmlwidgets::saveWidget(interaction_3, "interaction_3.html")
  # webshot2::webshot("interaction_3.html", "interaction_3.png", cliprect = c(200, 125, 600, 600))
  # interaction_3_png <- magick::image_read("interaction_3.png")
  
  
  # interaction_plots <- patchwork::wrap_elements(grid::rasterGrob(interaction_1_png)) + 
  #   patchwork::wrap_elements(grid::rasterGrob(interaction_2_png)) +
  #   patchwork::wrap_elements(grid::rasterGrob(interaction_3_png))
  
  interaction_plots <- interaction_2_png %>% 
    magick::image_ggplot(interpolate = TRUE)   # patchwork::wrap_elements(grid::rasterGrob(interaction_2_png))
  
  low_sedC_range = interaction_results[[1]] %>% 
    filter(group %in% c(1, 50)) %>% 
    group_by(group, var1_name) %>% 
    summarize(range_ch4 = expm1(max(yhat)) - expm1(min(yhat))) %>% 
    pivot_wider(values_from = range_ch4, names_from = group) %>% 
    pull(2) %>% 
    round(., 1)
  
  high_sedC_range = interaction_results[[1]] %>% 
    filter(group %in% c(1, 50)) %>% 
    group_by(group, var1_name) %>% 
    summarize(range_ch4 = expm1(max(yhat)) - expm1(min(yhat))) %>% 
    pivot_wider(values_from = range_ch4, names_from = group) %>% 
    pull(3) %>% 
    round(., 1)
  
  low_meanZ_range = interaction_results[[2]] %>% 
    filter(group %in% c(1, 50)) %>% 
    group_by(group, var1_name) %>% 
    summarize(range_ch4 = expm1(max(yhat)) - expm1(min(yhat))) %>% 
    pivot_wider(values_from = range_ch4, names_from = group) %>% 
    pull(2) %>% 
    round(., 1)
  
  high_meanZ_range = interaction_results[[2]] %>% 
    filter(group %in% c(1, 50)) %>% 
    group_by(group, var1_name) %>% 
    summarize(range_ch4 = expm1(max(yhat)) - expm1(min(yhat))) %>% 
    pivot_wider(values_from = range_ch4, names_from = group) %>% 
    pull(3) %>% 
    round(., 1)
  
  interaction_plot_final <- interaction_plots +
    geom_segment(mapping = aes(x = 1860, xend = 1900, y = 950, yend = 1450), 
                 color = "#4575b4", arrow = arrow(ends = "both", length = unit(0.25, "cm"))) +
    geom_segment(mapping = aes(x = 600, xend = 540, y = 1350, yend = 2250), 
                 color = "#4575b4", arrow = arrow(ends = "both", length = unit(0.25, "cm"))) +
    geom_segment(mapping = aes(x = 1880, xend = 1935, y = 1500, yend = 2200), 
                 color = "#d73027", arrow = arrow(ends = "both", length = unit(0.25, "cm"))) +
    geom_segment(mapping = aes(x = 1000, xend = 1000, y = 700, yend = 1000), 
                 color = "#d73027", arrow = arrow(ends = "both", length = unit(0.25, "cm"))) +
    annotate(geom = "text", label = paste0("Δ", c(low_sedC_range, high_sedC_range, low_meanZ_range, high_meanZ_range)),
             color = c("#4575b4", "#4575b4", "#d73027", "#d73027"), x = c(1990, 700, 2005, 890),
             y = c(1200, 1900, 1800, 850), size = 3.25)
  
  
  return(interaction_plot_final)
  
}
