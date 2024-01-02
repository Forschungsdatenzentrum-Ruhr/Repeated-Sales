prepare_hedonic <- function(RED_classified = NA, data_type = NA) {
  if (data_type == "WK") {
    hedonic <- make_hedonic_WK(RED_classified)
  } else if (data_type == "WM") {
    hedonic <- make_hedonic_WM(RED_classified)
  } else if (data_type == "HK") {
    hedonic <- make_hedonic_HK(RED_classified)
  }

  return(prepared_hedonic)
}