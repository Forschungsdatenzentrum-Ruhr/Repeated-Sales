# Known Issues: ----------------------------------------------

# test area
library(targets)
tar_load_globals()
tar_load(WM_req_data)
latlon = WM_req_data[counting_id == "13001783",.(latlon_utm)][[1]]
#latlon = "5265949.75796946401504.471474233"
geo_grouped_data = WM_req_data[latlon_utm == latlon]
tst = make_classification(geo_grouped_data)

