library(targets)
tar_make_future(workers = 20)
tar_visnetwork(targets_only = T)


#data = tar_read(classification_1_c840f946)
data = tar_read(plz_group_1)
data = data %>% filter(latlon_utm == "5984016.0214925618297.400683975")
