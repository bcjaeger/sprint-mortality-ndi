#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param base_sub
ndi_base_viz_tv_effect <- function(ndi_data_list) {

  map_dfr(
    .x = ndi_data_list,
    .id = 'group',
    .f = function(ndi_data){

      fit <- comp.risk(
        formula = Event(acm_years, cvd_event_cr) ~ treatment + cluster(randSite),
        cause = 1,
        model = 'prop',
        n.sim = 1000,
        data = ndi_data
      )

      fit_var <- as_tibble(fit$var.cum) |>
        transmute(time, sd = sqrt(treatmentIntensive))

      fit_data <- as_tibble(fit$cum) |>
        left_join(fit_var) |>
        transmute(
          time,
          hr_est = exp(treatmentIntensive),
          hr_lwr = exp(treatmentIntensive - 1.96 * sd),
          hr_upr = exp(treatmentIntensive + 1.96 * sd)
        ) |>
        add_row(.data = tibble(time = 0,
                               hr_est = 1,
                               hr_lwr = 1,
                               hr_upr = 1),
                .before = 2)

    }
  )


}
