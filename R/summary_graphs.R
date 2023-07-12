summary_graph_sensitivity <- function(sensitvity = NA) {
  
  non_list_reason_perc = sensitvity |> 
    tabyl(wohnflaeche_e_o,non_list_reason) |>  
    adorn_percentages("row") |> 
    adorn_pct_formatting(digits = 2)
  non_list_reason_perc
  
  # ggplot(non_list_reason_perc, aes(x = wohnflaeche_e_o)) + 
  #   geom_line(aes(y = Miss, color = "Miss"), lwd = 0.8) + 
  #   geom_line(aes(y = Sold, color = "Sold"), lwd = 0.8) +
  #   geom_line(aes(y = Update, color = "Update"), lwd = 0.8)
  
  group_sizes <- sensitvity[,
    .("size" = .N),
    by = c("wohnflaeche_e_o", "parent")
  ]

  # group_counts <- group_sizes[,
  #   .("counts" = .N),
  #   by = c("wohnflaeche_e_o", "size")
  # ]
  # ggplot(group_counts, aes( x= size, y = counts)) + 
  #   geom_bar(position = "dodge", stat = "identity") +
  #   facet_wrap(~wohnflaeche_e_o)
  
  summary_group_sizes <- group_sizes[,
    .(
      "Count" = .N,
      "Mean" = mean(size),
      "SD" = sd(size),
      "Min" = min(size),
      "Med" = median(size),
      "Max" = max(size)
    ),
    by = c("wohnflaeche_e_o")
  ]
  
  summary_group_sizes |> 
    htmlTable(rnames = F) |> 
    kableExtra::save_kable(paste0(output_path,"/summary_group_sizes.png"))
  
  return(NULL)
}
