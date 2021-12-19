#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ndi_data_list

ndi_long_infer <- function(ndi_data_list, n_sim = 1000) {

  mort_by_phase <- map_dfr(
    .x = ndi_data_list,
    .id = 'group',
    .f = function(ndi_data){

      frmla <-
        Surv(start_yrs, end_yrs, mortality) ~
        treatment +
        trial_phase +
        treatment:trial_phase +
        strata(randSite)

      fit <- cph_fit(ndi_data, frmla)

      k_mat <- rbind(c(1,0,0),
                     c(1,0,1))

      cph_estimate(fit, k_mat) |>
        mutate(trial_phase = levels(ndi_data$trial_phase), .before = 1) |>
        rename(mort_est = est,
               mort_lwr = lwr,
               mort_upr = upr)

    }

  )

  stratify <- all(names(ndi_data_list) == c("lt75_years", "gteq75_years"))

  cvd_by_phase <- map_dfr(
    .x = ndi_data_list,
    .id = 'group',
    .f = function(ndi_data){

      # if the time-varying approach fails to converge or gives
      # impossible hazard ratio estimates, estimate hazard ratios
      # separately in the two phases.

      if(stratify){

        ..ndi_data <- ndi_data |>
          mutate(time = if_else(trial_phase == 'trial',
                                end_yrs,
                                end_yrs - start_yrs),
                 .before = start_yrs)

        fits <- ..ndi_data |>
          split(..ndi_data$trial_phase) |>
          map(
            .f = ~ {
              m <- comp.risk(
                formula = Event(time, cvd_mortality) ~
                  const(treatment) + cluster(randSite),
                cause = 1,
                model = 'prop',
                n.sim = 1000,
                data = .x
              )
              class(m) <- c('bcj_comprisk', class(m))
              m
            }
          )

        return(
          map_dfr(.x = fits,
                  .f = cph_estimate,
                  k_mat = matrix(1),
                  .id = 'trial_phase') |>
            rename(cvd_est = est,
                   cvd_lwr = lwr,
                   cvd_upr = upr)
        )

      }

      # code for time-varying approach;
      # works well for everything except age > 75 years...

      fit <- comp.risk(
        formula = Event(start_yrs, end_yrs, cvd_mortality) ~
          const(treatment)*const(trial_phase) +
          cluster(randSite),
        cause = 1,
        model = 'fg',
        n.sim = 1000,
        data = ndi_data
      )

      class(fit) <- c('bcj_comprisk', class(fit))

      k_mat <- rbind(c(1,0,0),
                     c(1,0,1))

      cph_estimate(fit, k_mat) |>
        mutate(trial_phase = levels(ndi_data$trial_phase), .before = 1) |>
        rename(cvd_est = est,
               cvd_lwr = lwr,
               cvd_upr = upr)

    }

  )

  reduce(
    .x = list(mort_by_phase,
              cvd_by_phase),
    .f = left_join
  )


}

# generic functions to deal with irregular structure of timereg objects.

vcov.bcj_comprisk <- function(object, ...){
  vcov.comp.risk(object, ...)
}

coef.bcj_comprisk <- function(object, ...){
  coef.comprisk(object)[, 'Coef.']
}


















