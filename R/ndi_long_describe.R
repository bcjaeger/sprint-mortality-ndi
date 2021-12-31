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
          .before = start_yrs
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
                 sep = '\\.') |>
        rename(inc_mort_est = incidence_est,
               inc_mort_lwr = incidence_lwr,
               inc_mort_upr = incidence_upr)




    }
  )

  n_events <- map_dfr(
    .x = ndi_data_list,
    .id = 'group',
    .f = function(ndi_data){
      ndi_data |>
        group_by(trial_phase, treatment) |>
        summarize(n_obs = n(),
                  n_acm = sum(mortality),
                  n_cvd = sum(cvd_mortality == 1),
                  n_other = sum(cvd_mortality == 2))
    }
  )


  reduce(
    .x = list(incidence_mort,
              n_events),
    .f = left_join,
    by = c('trial_phase',
           'treatment',
           'group')
  )

}
