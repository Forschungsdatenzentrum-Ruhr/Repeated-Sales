# Known Issues: ----------------------------------------------
# odd behavior at 5914513.11605105604957.285071247 for WM -> why are they not parent/child but seperat?
# odd behavior at 5914585.94429017603205.12213582 for WM -> should be part of bigger parent there?

# test area
library(targets)
tar_load_globals()
tar_load(WM_full_data)

geo_grouped_data = WM_full_data[latlon_utm == "5914513.11605105604957.285071247"]
tst = make_classification(geo_grouped_data)
tst$parent
# 
# # # 50 k -> 5
# size = nrow(geo_grouped_data)/10000
# #size = 7
# for(i in 1:size){
#     # for 1,2,3,4,5
#     print(i)
#     start = (i-1) * 10000
#     end = (i) * 10000
#     sliced_data = geo_grouped_data[start:end]
#     for(id in unique(geo_grouped_data$latlon_utm)){
#         print(id)
#         id_data = geo_grouped_data[latlon_utm == id,]
#         tst = make_classification(id_data)
#     }
#     }
# 
