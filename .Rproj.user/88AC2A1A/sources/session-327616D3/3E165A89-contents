#' Calculate repeated_id and similarity (measures)

#' @description
#' 'repeat_similiarity()' calculates relevant offsets and similarity measures used
#' for classification in later steps
#' 
#' @return
#' A n x 2 tibble containing repeated_ids and similarity measures of .data
#' relative to first row (potential parent)
#' 
#' @param .data A data frame or similar
#' @param offsets offset values used to calculate repeated_id
#' 
#' 
repeat_similiarity <- function(.data, offsets = range_offsets) {
  
  # drop time offset
  col_names <- offsets$rowname[!offsets$rowname == "time"]
  
  # reduce data to relevant columns
  .data %<>% select(col_names)
  
  ## calculate similarity-ranges of potential parent
  similarity_ranges <- tbl_transpose(.data[1, ], rownames = NA) %>%
    set_colnames("baseline") %>%
    rownames_to_column() %>%
    merge(offsets, by = "rowname", all.x = T) %>%
    
    # calculate similarity-borders
    summarise(
      # keep rownames
      rowname = rowname,
      
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
      )
    )
  
  # attach similarity-ranges to .data for comparisons
  between_id <- merge(similarity_ranges, t(.data), by.x = "rowname", by.y = 0)
  
  ## calculate repeated ids
  repeated_id <- between_id %>%
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
  
  
  # find relative start point of non similarity-ranges
  # this is currently always 1:4 since there are 4 borders considered
  cutoff <- 1:(match("exact_right", colnames(between_id)))
  
  ## calculate similarity measure
  similarity <- between_id %>%
    
    # transpose 
    tbl_transpose() %>%
    
    # consider only non-borders
    slice(-cutoff) %>%
    
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
