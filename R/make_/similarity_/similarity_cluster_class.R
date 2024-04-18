# General notes:
# have to treat rows and columns to matrices differently since scaled distance "from to" are not equal to "to from"
# this is class since it has a lot of attributes and constant self-references
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
    names_cluster_combinations = NULL,
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
      self$cluster_options <- self$filter_unique_options(cluster_options)
      self$distance <- distance
      self$index <- cluster_options
      self$sequence <- seq_along(self$cluster_names)

      return(NULL)
    },
    make_cluster = function(unique_option) {
      # assign temp data.table
      parent <- attr(unique_option, "names")
      parent_col <- which(parent == self$names_cluster_combinations)
      temps <- data.table(
        "counting_id" = as.numeric(self$names_cluster_combinations),
        "parent" = as.numeric(parent),
        "sim_dist" = as.numeric(unique_option |> unlist()),
        "sim_index" = as.numeric(self$index_cluster_combinations[[parent_col]])
      )
      return(temps)
    },
    # NOTE: some legacy stuff here, clean up what isnt needed
    filter_unique_options = function(unique_options) {
      if (!is.null(unique_options)) {
        unique_options <- data.table::transpose(unique_options) |>
          setnames(new = self$cluster_names)
        unique_options <- unique(unique_options)

        unique_options <- unique_options |>
          is.na() |>
          not() |>
          apply(
            1,
            which,
            simplify = F
          )

        return(unique_options)
      }
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
          self$names_cluster_combinations <- attr(self$distance_cluster_combinations, "names")
          # identify exact duplicates in combinations (happens when all inputs are exactly the same)
          dups <- base::duplicated(self$distance_cluster_combinations, margin = 0)
          # and drop those duplicates so we only deal with unique_combinations
          unique_cluster_combinations <- self$distance_cluster_combinations[, !dups, with = F]
          for (cluster_combination in seq_along(unique_cluster_combinations)) {
            self$centers <- rbind(self$centers, self$make_cluster(unique_cluster_combinations[, cluster_combination, with = F]))
          }
        }
      }
      return(NULL)
    }
  )
)
