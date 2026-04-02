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
    layout(#title =  "B)",
      scene = list(
        xaxis = list(title = "   sediment C<br>(% dry weight)"),
        yaxis = list(title = "mean depth<br>     (m)"),
        zaxis = list(title = "Total CH<sub>4</sub> emission rate<br>     (mg CH<sub>4</sub> m<sup>-2</sup> h<sup>-1</sup>)"),
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
  webshot2::webshot("interaction_2.html", "interaction_2.png", zoom = 2, cliprect = c(225, 100, 525, 650))
  interaction_2_png <- magick::image_read("interaction_2.png")
  
  # htmlwidgets::saveWidget(interaction_3, "interaction_3.html")
  # webshot2::webshot("interaction_3.html", "interaction_3.png", cliprect = c(200, 125, 600, 600))
  # interaction_3_png <- magick::image_read("interaction_3.png")
  
  
  # interaction_plots <- patchwork::wrap_elements(grid::rasterGrob(interaction_1_png)) + 
  #   patchwork::wrap_elements(grid::rasterGrob(interaction_2_png)) +
  #   patchwork::wrap_elements(grid::rasterGrob(interaction_3_png))
  
  interaction_plots <- patchwork::wrap_elements(grid::rasterGrob(interaction_2_png))
  
  return(interaction_plots)
  
}
