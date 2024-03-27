# Known Issues: ----------------------------------------------

# test area
library(targets)
tar_load_globals()
tar_load(WM_req_data)
latlon = WM_req_data[counting_id == "11954069",.(latlon_utm)][[1]]
#latlon = "5453877.51572261354963.167174795"
geo_grouped_data = WM_req_data[latlon_utm == latlon]
tst = make_classification(geo_grouped_data)

