make_interaction_plots <- function(random_forest_object,
                                   dat_trans){

  
  int_var_combos = data.frame("model_name" = c(#"ch4_total.local_vars_all",
                                               "ch4_total.local_vars_all")
                                               #"ch4_total.local_vars_all")
                                               ,
                              
                              "int_var_1" = c(#"nla17_sed_c",
                                              "nla17_sed_c")
                                              #"mean_depth"
                                              ,
                              
                              "int_var_2" = c(#"littoral_fraction",
                                              "mean_depth")
                                              #"littoral_fraction"
                                              )
  
  library(ranger)
  partial_interaction_all = c()
  for(i in 1:nrow(int_var_combos)){
     pdp_data = pdp::partial(object = #random_forest_object_rangerOnly[[int_var_combos$model_name[i]]],
                              random_forest_object[[int_var_combos$model_name[i]]]$ranger,
                            pred.var = c(int_var_combos$int_var_1[i], int_var_combos$int_var_2[i]),
                            type = "regression", train = dat_trans, grid.resolution = 50)
    
    partial_interaction_spatial = pdp_data %>%
      rename(var1 = 1,
             var2 = 2) %>%
      mutate(var1_name = int_var_combos$int_var_1[i],
             var2_name = int_var_combos$int_var_2[i],
             model_name = int_var_combos$model_name[i],
             pair_num = i)
    
    partial_interaction_all = rbind(partial_interaction_all,
                                    partial_interaction_spatial)
  }
  
  
  partial_interaction_spatial_df_g1 = partial_interaction_all %>%
    as.data.frame() %>%
    group_by(pair_num, var1_name, var2_name, model_name) %>%
    mutate(pair = paste(var1_name, var2_name, sep = "__")) %>%
    mutate(group = as.numeric(factor(var1))) %>%
    select(pair, pair_num, var1_name, var2_name, model_name, yhat, group, var1, var2) %>%
    ungroup()
  
  
  
  partial_interaction_spatial_df_g2 = partial_interaction_all %>%
    as.data.frame() %>%
    group_by(pair_num, var1_name, var2_name, model_name) %>%
    mutate(pair = paste(var1_name, var2_name, sep = "__")) %>%
    mutate(group = as.numeric(factor(var2))) %>%
    select(pair, pair_num, var1_name, var2_name, model_name, yhat, group, var1, var2) %>%
    ungroup()
  
  
  
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
        xaxis = list(title = "sediment C"),
        yaxis = list(title = "mean depth"),
        zaxis = list(title = "CH<sub>4</sub> emission rate"),
        camera = list(
          eye = list(x = -1, y = 2, z = 1) # Adjust x, y, z for different angles/zoom
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
  webshot2::webshot("interaction_2.html", "interaction_2.png", cliprect = c(225, 100, 525, 650))
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
