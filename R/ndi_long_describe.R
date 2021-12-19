#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ndi_data_list
ndi_long_describe <- function(ndi_data_list) {

  incidence_mort <- map_dfr(
    .x = ndi_data_list,
    .id = 'group',
    .f = function(ndi_data){


      ..ndi_data <-
        ndi_data |>
        mutate(
          time = if_else(trial_phase == 'trial',
                         end_yrs,
                         end_yrs - start_yrs),
          cvd_mortality = as.numeric(cvd_mortality == 1)
        ) |>
        split(f = list(ndi_data$trial_phase,
                       ndi_data$treatment))

      ..ndi_data |>
        map_dfr(cph_incidence,
                time = 'time',
                status = 'mortality',
                .id = 'split_me') |>
        separate(col = 'split_me',
                 into = c('trial_phase', 'treatment'),
                 sep = '\\.')

    }
  )

  # ndi_data |>
  #   split(ndi_data$trial_phase) |>
  #   map()
  # cvd_inc <-
  #   cuminc(ftime   = ..ndi_data$end_yrs,
  #          fstatus = ..ndi_data$cvd_mortality,
  #          cencode = 0,
  #          group = ..ndi_data$treatment)
  #
  # ndi_data <- ndi_data_list[[1]]


}
