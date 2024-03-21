# summary_table_sensitivity <- function(sensitivity) {
#   
#   # subset to only sold listings and calculate number of parents
#   sensitivity_parent_counts <- sensitivity[
#     "Sold", .("Count" = .N),
#     by = c("parent", "wohnflaeche_e_o"), on = "non_list_reason"
#   ] |> unique()
# 
#   summary_sensitivity_parent_counts <- sensitivity_parent_counts[,
#     .(
#       "Unique" = uniqueN(parent),
#       "Count" = .N,
#       "Mean" = round(mean(Count), 2),
#       "SD" = round(sd(Count), 2),
#       "Min" = min(Count),
#       "Med" = median(Count),
#       "Max" = max(Count)
#       ),
#     by = c("wohnflaeche_e_o")
#   ]
#   
#   summary_sensitivity_parent_counts |>
#     htmlTable(rnames = F) |>
#     kableExtra::save_kable(paste0(output_path, "/summary_sensitivity_parent_counts.png"))
#   
# 
#   return(NULL)
# }
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# 
# ## calc percentages of non_list_reason for each wohnflaeche_e_o
# non_list_reason_perc <- sensitivity |>
#   tabyl(wohnflaeche_e_o, non_list_reason) |>
#   adorn_percentages("row") |>
#   adorn_pct_formatting(digits = 2)
# 
# 
# 
# 
# counts_sensitivity_table <- counts_federal |>
#   filter(Count <= 5) |>
#   tabyl(wohnflaeche_e_o, Count) |>
#   adorn_totals("row") |>
#   adorn_percentages("row") |>
#   adorn_pct_formatting(digits = 2) |>
#   adorn_ns()
# 
# counts_non_list_reason_perc <- non_list_reason_perc |>
#   merge(counts_sensitivity_table, by = "wohnflaeche_e_o")
# 
# 
# # -------------------------------------------------------------------------
# 
# ## measure ratio of floors relative to parents on those floors
# # intuition: as wohnflaeche_e_o rises, number of parents decreases
# # as cluster cluster_sizes increase. This should lead to a ratio that converges
# # to somewhere close to but less than 1 (meaning one parent per floor)
# # but likely never exactly one 1, since number of rooms still hard divides
# floor_sensitivity <- sensitivity[
#   , .("parent_floor_ration" = (uniqueN(etage) / uniqueN(parent))),
#   by = c("wohnflaeche_e_o", "latlon_utm")
# ] |> unique()
# 
# avg_floor_sensitivity <- floor_sensitivity[, mean(parent_floor_ration), by = "wohnflaeche_e_o"]
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# 
# 
# 
# 
