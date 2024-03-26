# Known Issues: ----------------------------------------------

# test area
library(targets)
tar_load_globals()
tar_load(WM_req_data)
latlon = WM_req_data[counting_id == "13001783",.(latlon_utm)][[1]]
#latlon = "5266426.10693358402257.692470619"
geo_grouped_data = WM_req_data[latlon_utm == latlon]
tst = make_classification(geo_grouped_data)

