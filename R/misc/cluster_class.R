# Cluster Class
cluster <- R6::R6Class("cluster",
  public = list(

    # default values
    cluster_options = NULL,
    distance = NULL,
    index = NULL,
    means = NULL,
    sim_index = NULL,
    sim_dist = NULL,
    sequence = NULL,
    cluster_names = NULL,
    subset = NULL,
    min_ss = NULL,
    centers = NULL,

    # initial value setup
    initialize = function(cluster_options = NULL,
                          distance = NULL,
                          index = NULL,
                          means = NULL,
                          cluster_names = NULL,
                          sim_dist = data.table(),
                          sim_index = data.table(),
                          sequence = NULL,
                          subset = NULL,
                          min_ss = NULL,
                          centers = data.table()) {
      # calculation helpers
      self$cluster_names <- attr(cluster_options, "names")
      self$cluster_options <- if (!is.null(cluster_options)) {
        unique(
          apply(
            !is.na(
              data.table::transpose(cluster_options) |> setnames(new = self$cluster_names)
            ),
            1,
            which,
            simplify = F
          )
        )
      }
      
      self$distance <- distance
      self$index <- cluster_options
      self$means <- means
      self$sequence <- seq_along(means)
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
          
          # issues occurs when center chosen does not resemble all children.
          # 532145 532135 532155 532136 532156
          # with first and last not being similar (origin being second)
          # fixed during cost-function
          
          # extract minimum mean 
          self$min_ss <- self$subset[which.min(self$means[self$subset])]

          # assign respective distance to temp data.table
          temp_dist <- data.table(
            "sim_dist" = as.numeric(self$distance[self$min_ss,self$subset,  with = F])
          )
          # bind temp data.table with itself for each iteration
          self$sim_dist <- rbind(self$sim_dist, temp_dist)

          # assign respective index temp data.table
          temp_index <- data.table(
            "sim_index" = as.numeric(self$index[self$min_ss,self$subset,  with = F])
          )
          # bind temp data.table with itself for each iteration
          self$sim_index <- rbind(self$sim_index, temp_index)

          # assign temp data.table
          temp_ids <- data.table(
            "counting_id" = as.numeric(names(cluster_option)),
            "parent" = as.numeric(self$cluster_names[self$min_ss])
          )
          # bind temp data.table with itself for each iteration
          self$centers <- rbind(self$centers, temp_ids)
        }
        
        # overwrite total dist with current rel distance
        self$centers$sim_dist <- self$sim_dist
        self$centers$sim_index <- self$sim_index
      }
    }
  )
)
