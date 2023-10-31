# have to treat rows and columns to matrices differently since scaled distance "from to" are not equal to "to from"
# splitting this should also make it way less confusing


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
    names_cluster_combinations  = NULL,
    distance_cluster_combinations = NULL,
    index_cluster_combinations = NULL,
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
    make_cluster = function(unique_option) {
      # assign temp data.table
      parent <- names(unique_option)
      temps <- data.table(
        "counting_id" = as.numeric(self$names_cluster_combinations),
        "parent" = as.numeric(parent),
        "sim_dist" = as.numeric(unique_option),
        "sim_index" = as.numeric(self$index_cluster_combinations[, parent, with = F])
      )
      return(temps)
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

          # consider all combinations within cluster
          self$distance_cluster_combinations <- self$distance[self$subset, self$subset, with = F]
          self$index_cluster_combinations <- self$index[self$subset, self$subset, with = F]
          self$names_cluster_combinations <- names(self$distance_cluster_combinations)
          # transposing is necessary since for scaled values distance AB is not equal to BA
          t_cluster_combinations <- data.table::transpose(self$distance_cluster_combinations) |> setnames(self$names_cluster_combinations)
          # identify exact duplicates in combinations (happens when all inputs are exactly the same)
          dups <- duplicated(self$t_cluster_combinations)
          # and drop those duplicates so we only deal with unique_combinations
          unique_cluster_combinations <- self$distance_cluster_combinations[, !dups, with = F]

          for (cluster_combination in seq_along(unique_cluster_combinations)) {
            self$centers = rbind(self$centers, self$make_cluster(unique_cluster_combinations[[cluster_combination]]))
          }
        }
      }
    }
  )
)
