# test area
library(targets)
tar_load_globals()
tar_load(WK_classified)
# latlon = WM_req_data[counting_id == "11613257",.(latlon_utm)][[1]]
# latlon = "5453877.51572261354963.167174795"
# geo_grouped_data = WM_req_data[latlon_utm == latlon]
# geo_grouped_data = WK_req_data[blid == 10]
# tst = make_classification(geo_grouped_data)

ss = prepare_hedonic(WK_classified, "WK")
