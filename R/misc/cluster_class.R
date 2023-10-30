# Cluster Class
cluster <- R6::R6Class("cluster",
  public = list(
    # default values
    cluster_options = NULL,
    distance = NULL,
    index = NULL,
    sim_index = NULL,
    sim_dist = NULL,
    sequence = NULL,
    cluster_names = NULL,
    subset = NULL,
    cluster_combinations = NULL,
    unique_cluster_combinations = NULL,
    centers = NULL,

    # initial value setup
    initialize = function(cluster_options = NULL,
                          distance = NULL,
                          index = NULL,
                          cluster_names = NULL,
                          sim_dist = data.table(),
                          sim_index = data.table(),
                          sequence = NULL,
                          centers = data.table()) {
      # calculation helpers
      self$cluster_names <- attr(cluster_options, "names")
      self$cluster_options <- filter_unique_options(cluster_options, self$cluster_names)
      self$distance <- distance
      self$index <- cluster_options
      self$sequence <- seq_along(self$cluster_names)
    },
    # actual cluster sequence
    determine_cluster_centers = function() {
      # catch single obs cases. keep for now with parent = child
      if (length(self$cluster_options) == 1 & 1 %in% lengths(self$cluster_options)) {
        self$centers <- data.table(
          "sim_dist" = 0,
          "sim_index" = 0,
          "counting_id" = as.numeric(self$cluster_names),
          "parent" = as.numeric(self$cluster_names)
        )
      } else {
        # consider all unique combinations of clusters
        for (cluster_option in self$cluster_options) {
          # subset to current cluster_option
          self$subset <- self$sequence[self$cluster_names %in% names(cluster_option)]

          self$cluster_combinations <- self$distance[self$subset, self$subset, with = F]

          # transpose -> unique -> transpose? trouble keeping counting_id
          unique_cluster_combinations <- filter_unique_options(
            self$cluster_combinations,
            new_names = names(self$cluster_combinations),
            use_which = F
          )
          print(unique_cluster_combinations)
          if (nrow(unique_cluster_combinations) > 1) {

            for (cluster_combination in unique_cluster_combinations) {
              


              ## temp_storage

                # assign respective distance to temp data.table
                # this could be taken from cluster_combination directly
                temp_dist <- data.table(
                  "sim_dist" = as.numeric(self$distance[, self$subset, with = F])
                )

                # assign respective index temp data.table
                temp_index <- data.table(
                  "sim_index" = as.numeric(self$index[, self$subset, with = F])
                )

                # assign temp data.table
                temp_ids <- data.table(
                  "counting_id" = as.numeric(names(cluster_option)),
                  "parent" = as.numeric(names(cluster_combination))
                )

                ## temp self binding
                # bind temp data.table with itself for each iteration
                self$sim_dist <- rbind(self$sim_dist, temp_dist)


                # bind temp data.table with itself for each iteration
                self$sim_index <- rbind(self$sim_index, temp_index)


                # bind temp data.table with itself for each iteration
                self$centers <- rbind(self$centers, temp_ids)
            }
          } else {
          }
        }

        # # overwrite total dist with current rel distance
        # self$centers$sim_dist <- self$sim_dist
        # self$centers$sim_index <- self$sim_index
      }
    }
  )
)
