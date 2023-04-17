similarity_index <- function(data_to_similarity) {
  
  # entries with less than zero a this point are removed later
  data_to_similarity[data_to_similarity < 0] <- NA
  
  # scale values
  data_to_similarity %<>% scale(
    ., 
    center = F, 
    scale = slice(., 1)
  )
  # extract first column
  index_values <- 1 - as.matrix(dist(data_to_similarity, method = "euclidean"))[1, ]
  
  return(index_values)
}