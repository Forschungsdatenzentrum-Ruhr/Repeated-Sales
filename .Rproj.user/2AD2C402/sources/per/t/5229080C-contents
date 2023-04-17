#' Calculate repeated_id and similarity (measures)

#' @description
#' 'repeat_similiarity()' calculates relevant range_offsets and similarity measures used
#' for classification in later steps
#' 
#' @return
#' A n x 2 tibble containing repeated_ids and similarity measures of .data
#' relative to first row (potential parent)
#' 
#' @param .data A data frame or similar
#' @param range_offsets offset values used to calculate repeated_id
#' 
#' 
repeat_similiarity <- function(.data = NA, range_offsets = range_offsets) {
  
  # drop time offset
  categorie_names <- range_offsets$categorie[!range_offsets$categorie == "time"]
  
  ## calculate similarity-ranges of potential parent
  similiarity_range = .data %>% 
    # reduce data to relevant columns
    select(categorie_names) %>% 
    tbl_transpose() %>%
    mutate(
      baseline = V1,
      categorie = categorie_names,
      .keep = "unused"
    ) %>%
    merge(range_offsets, by = "categorie", all.x = T) %>%
    
    # calculate similarity-borders
    mutate(
      ## resembling-similiarity-range
      # left border
      resembling_left = case_when(
        offset_type == "add" ~ baseline - resembling_offset,
        offset_type == "multi" ~ baseline * (1 - resembling_offset)
      ),
      # right border
      resembling_right = case_when(
        offset_type == "add" ~ baseline + resembling_offset,
        offset_type == "multi" ~ baseline * (1 + resembling_offset)
      ),
      
      ## exact-similiarity-range
      # left border
      exact_left = case_when(
        offset_type == "add" ~ baseline - exact_offset,
        offset_type == "multi" ~ baseline * (1 - exact_offset)
      ),
      # right border
      exact_right = case_when(
        offset_type == "add" ~ baseline + exact_offset,
        offset_type == "multi" ~ baseline * (1 + exact_offset)
      ),
      .keep = "unused"
    )
  
  ## calculate repeated ids
  repeated_id <- similiarity_range %>%
    # columnwise checks if .data values are within borders of similiarity-ranges
    # can be thought of as an extend version of dpylr::between()
    summarise(
      across(
        # only consider non-border values
        starts_with("V"),
        ~ case_when(
         
          # value is within exact-similarity-range
          . >= exact_left & exact_right >= . ~ 1,
          
          # value is within resembling-similiarity-range
          . >= resembling_left & resembling_right >= . ~ 0,
          
          # value is not within either border
          TRUE ~ -1
        )
      )
    ) %>%
    # columnwise checks for repeated criteria
    summarise(
      across(
        # only consider non-border values
        starts_with("V"),
        ~ case_when(
          
          # atleast one values is outside of the bounds of similarity-ranges
          # ~ non repeated offering
          -1 %in% . ~ -1,
          
          # all values are atleast within similiar borders 
          # ~ resembling repeated offering
          0 %in% . ~ 0,
          
          # all values are within exact borders 
          # ~ true repeated offering
          TRUE ~ 1
        )
      )
    ) %>%
    # transpose
    tbl_transpose()
  
  ## calculate similarity measure
  similarity <- similiarity_range %>%
    
    # transpose 
    tbl_transpose() %>%
    
    
    ## convert each .data column into fitting type
    # flexibly convert numeric input stored as character values
    # into actual numeric values
    summarise(
      across(
        everything(),
        ~ type.convert(.x, as.is = T)
      )
    ) %>%
    # calculate similarity index
    mutate(
      similarity_index(.)
    ) %>%
    # extract index values
    pull()
  
  
  # bind repeated ids and similarity measures
  out <- cbind(repeated_id, similarity)
  
  # rename to above
  names(out) <- c("repeated_id", "similarity")
  
  return(out)
}
