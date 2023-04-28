# removal <- function(geo_grouped_data = NA) {
#' @title WIP
#'
#' @description WIP
#' @param WIP
#' @param WIP
#' @note
#'
#' @return WIP
#' @author Thorben Wiebe
#----------------------------------------------
cluster = R6::R6Class("cluster",
                      public = list(
                        options = NULL,
                        distance = NULL,
                        means = NULL,
                        sim_dist = NULL,
                        sequence = NULL,
                        var_names = NULL,
                        subset = NULL,
                        max_ss = NULL,
                        centers = NULL,
                        
                        initialize = function(options = NULL,
                                              distance = NULL,
                                              means = NULL,
                                              var_names = NULL,
                                              sim_dist = data.table(),
                                              sequence = NULL,
                                              subset = NULL,
                                              max_ss = NULL,
                                              centers = data.table()) {
                          
                          self$options <-  unique(apply(!is.na(options),1,which))
                          self$distance = distance
                          self$means = means
                          self$sequence = seq_along(self$means)
                          self$var_names = attr(self$options |> unlist(),"names")
                        },
                        determine_cluster_centers = function(){
                          for(cluster_option in self$options){
                            self$subset = self$sequence[self$var_names %in% names(cluster_option)]
                            self$max_ss = self$subset[which.min(self$means[self$subset])]
                            temp2 = data.table(
                              "sim_dist" = self$distance[self$max_ss,self$subset, with = F]
                            )
                            
                            self$sim_dist = rbind(self$sim_dist, t(temp2))
                            
                            
                            temp = data.table(
                              "counting_ids" = as.numeric(names(cluster_option)),
                              "parent" = self$var_names[self$max_ss]
                            )
                            self$centers = rbind(self$centers, temp)
                            
                          }
                          self$centers$sim_dist = self$sim_dist
                          
                          
                        }
                      )
                      
                      
)










categories <- range_offsets["category"] |> pull()
# backup = geo_grouped_data
# geo_grouped_data = geo_grouped_data[latlon_utm == "5878499.91877856482091.18037048"]


duplicates <- duplicated(geo_grouped_data[, ..categories])

first_occurence_ids <- geo_grouped_data[!duplicates, counting_id]
unique_combinations <- geo_grouped_data[!duplicates, ..categories]

setkey(unique_combinations, wohnflaeche, etage, zimmeranzahl)

similarity_index_list <- similarity_dist_list <- list()

for (i in 1:nrow(unqiue_combinations)) {

  # percentage of rooms scaled values are allowed to be off
  # e.g. what percentage of 8 rooms is 0.5 rooms
  # this feels way to complicated
  scaled_zimmeranzahl_r_o <- unique_combinations[
    ,
    (as.numeric(unique_combinations[i, "zimmeranzahl"]) + zimmeranzahl_r_o)
    / as.numeric(unique_combinations[i, "zimmeranzahl"]) - 1
  ]

  data_to_similarity <- scale(unique_combinations, center = F, scale = unique_combinations[i]) |> as.data.table()

  similarity_index_list[[i]] <- data_to_similarity[, .(fcase(
    ## exact repeat
    # percentage deviation acceptable
    abs(1 - wohnflaeche) <= wohnflaeche_e_o &
      # zimmeranzahl and etage are exact matches
      zimmeranzahl == 1 &
      etage == 1,
    0,

    ## similar repeat
    # percentage deviation acceptable
    abs(1 - wohnflaeche) <= wohnflaeche_r_o &
      # zimmeranzahl deviation acceptable
      abs(1 - zimmeranzahl) <= scaled_zimmeranzahl_r_o,
    1,

    # no matches
    default = NA
  ))] |> as.matrix()

  similarity_dist_list[[i]] <- as.matrix(dist(data_to_similarity, method = "euclidean"))[i, ]
}
# transform to data.tables and set counting ids as column names
similarity_dist_list <- as.data.table(similarity_dist_list)
similarity_index_list <- as.data.table(similarity_index_list)

setnames(similarity_index_list, as.character(first_occurence_ids))
setnames(similarity_dist_list, as.character(first_occurence_ids))

clustering <- cluster$new(
  options = similarity_index_list,
  distance = similarity_dist_list,
  means = rowMeans(similarity_index_list * similarity_dist_list, na.rm = T)
)
cluster
clustering$determine_cluster_centers()
clustering$centers

geo_grouped_data[
  clustering$centers,
  on = "counting_id",
  allow.cartesian = T
]

# }
