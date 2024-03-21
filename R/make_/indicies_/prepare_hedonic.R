prepare_hedonic <- function(RED_classified, data_type) {
  if (data_type == "WK") {
    prepared_hedonic <- make_hedonic_WK(RED_classified)
  } else if (data_type == "WM") {
    prepared_hedonic <- make_hedonic_WM(RED_classified)
  } else if (data_type == "HK") {
    prepared_hedonic <- make_hedonic_HK(RED_classified)
  }

  return(prepared_hedonic)
}