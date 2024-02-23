# # test area
# library(targets)
# tar_load_globals()
# tar_load(RED_req_columns)
#   
# geo_grouped_data = RED_req_columns[.(11), on = "blid"]
# # faulty ? 5826811.92605104791704.891827032
# # stuck? 5829381.74206909796408.450095488
# geo_grouped_data = geo_grouped_data[latlon_utm == "5829381.74206909796408.450095488"]
# geo_grouped_data = geo_grouped_data[1:4000]
# tst = make_classification(geo_grouped_data)
# 
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
