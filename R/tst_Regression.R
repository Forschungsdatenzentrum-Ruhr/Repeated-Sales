# tst_Regression <- function(classification = NA) {
#   # repeated sales regression
# 
#   classification <- classification[
#     "Sold",
#     on = "non_list_reason"
#   ]
#   upper_percentile <- quantile(classification[price_var >= 0, price_var], 1 - (0.5 / 100))
#   lower_percentile <- quantile(classification[price_var >= 0, price_var], (0.5 / 100))
# 
# 
# 
#   tst <- classification[
#     price_var <= upper_percentile & 
#       price_var >= lower_percentile &,
#     ln_pvar := log(price_var)
#   ]
# }
