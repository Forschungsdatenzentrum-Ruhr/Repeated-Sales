# TO DO:
# swap t() for pivot_wider

between_ext <- function(data, offsets = range_offsets) {

  # drop time for now
  col_names <- offsets$rowname[!offsets$rowname == "time"]

  data %<>% select(col_names)

  inital <- t(data[1, ]) %>%
    as_tibble(rownames = NA) %>%
    set_colnames("baseline") %>%
    rownames_to_column() %>%
    merge(offsets, by = "rowname", all.x = T) %>%
    summarise(
      rowname = rowname,
      similar_left = case_when(
        offset_type == "add" ~ baseline - similar_offset,
        offset_type == "multi" ~ baseline * (1 - similar_offset)
      ),
      similar_right = case_when(
        offset_type == "add" ~ baseline + similar_offset,
        offset_type == "multi" ~ baseline * (1 + similar_offset)
      ),
      exact_left = case_when(
        offset_type == "add" ~ baseline - exact_offset,
        offset_type == "multi" ~ baseline * (1 - exact_offset)
      ),
      exact_right = case_when(
        offset_type == "add" ~ baseline + exact_offset,
        offset_type == "multi" ~ baseline * (1 + exact_offset)
      )
    )

  repeated_id <- merge(inital, t(data), by.x = "rowname", by.y = 0) %>%
    summarise(
      across(
        starts_with("V"),
        ~ case_when(
          . >= exact_left & exact_right >= . ~ 1,
          . >= similar_left & similar_right >= . ~ 0,
          TRUE ~ -1
        )
      )
    ) %>%
    summarise(
      across(
        starts_with("V"),
        ~ case_when(
          -1 %in% . ~ -1,
          0 %in% . ~ 0,
          TRUE ~ 1
        )
      )
    ) %>%
    t()

  return(repeated_id)
}

classify_data <- function(latlon_utm) {
  
  ######################
  #Settings
  ######################
  range_offsets <<- tibble::tribble(
    ~rowname, ~similar_offset, ~exact_offset, ~offset_type,
    "wohnflaeche", 0.1, 0.05, "multi",
    "etage", 99, 0, "add",
    "zimmeranzahl", 0.5, 0, "add",
    "time", 0, 6, NA
  )
  
  repeated_offerings <- c()

  latlon_utm %<>% arrange(amonths)

  # extract time offset for readability
  time_offset <- range_offsets$exact_offset[range_offsets$rowname == "time"]

  while (nrow(latlon_utm) > 0) {
    candidates <- latlon_utm %>%
      mutate(repeated_id = between_ext(.)) %>%
      filter(repeated_id != -1) %>%
      mutate(
        # to last date in data
        td_to_end = (as.numeric(data_end_date) - as.numeric(amonths)),

        # of leading offering
        td_of_lead = (lead(as.numeric(amonths)) - as.numeric(emonths)),

        # replace last td_of_lead with td_to_end
        td_of_lead = replace_na(td_of_lead, td_to_end[is.na(td_of_lead)])
      )
    #remove true repeates from options since there can only be one
    ids_to_remove <- candidates$counting_id[candidates$repeated_id == 1]

    latlon_utm %<>% filter(!counting_id %in% ids_to_remove)

    repeated_offerings <- candidates %>%
      filter(td_of_lead >= time_offset) %>%
      mutate(
        obj_parent = counting_id[1]
      ) %>%
      bind_rows(repeated_offerings)
  }
  # throw out similar dups based on jaccard index?
  
  tar_delete(starts_with("classification"))
  return(repeated_offerings)
}
