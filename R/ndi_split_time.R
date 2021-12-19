#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ndi_data
ndi_split_time <- function(ndi_data,
                           interval_size,
                           time_var,
                           event_var,
                           event_start_status) {

  ndi_data |>
    timeSplitter(by = interval_size,
                 time_var = time_var,
                 event_var = event_var,
                 event_start_status = event_start_status) |>
    as_tibble() |>
    mutate(time_factor = factor(Start_time))

}
