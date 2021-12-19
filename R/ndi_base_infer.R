#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ndi_data_list
ndi_base_infer <- function(ndi_data_list) {

  # models and estimates

  map_dfr(
    .x = ndi_data_list,
    .id = 'group',
    .f = function(ndi_data) {

      ndi_intervals <- ndi_split_time(ndi_data,
                                      interval_size = 1.5,
                                      event_var = "acm_event",
                                      event_start_status = 0,
                                      time_var = "acm_years")

      acm_fit_timevarying <- ndi_intervals |>
        mutate(treatment = as.numeric(treatment == 'Intensive')) |>
        cph_fit(
        formula = Surv(Start_time, Stop_time, acm_event) ~
          treatment + treatment:time_factor + strata(randSite)
      )


      cph_estimate(acm_fit_timevarying,
                   k_mat = rbind(c(1,0,0,0,0,0,0),
                                 c(1,1,0,0,0,0,0),
                                 c(1,0,1,0,0,0,0),
                                 c(1,0,0,1,0,0,0),
                                 c(1,0,0,0,1,0,0),
                                 c(1,0,0,0,0,1,0),
                                 c(1,0,0,0,0,0,1))) |>
        mutate(years = levels(ndi_intervals$time_factor),
               .before = 1)

    }

  )



}
