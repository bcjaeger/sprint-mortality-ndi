#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param base_sub
ndi_base_viz_tv_effect <- function(ndi_data_list) {

  cols_bg <- viridis(3)
  cols_tx <- pal_lancet()(2)

  map_dfr(
    .x = ndi_data_list,
    .id = 'group',
    .f = function(ndi_data){


      fit_acm <- timecox(
        Surv(acm_years, acm_event) ~ treatment + cluster(randSite),
        data = ndi_data
      )

      var_acm <- as_tibble(fit_acm$robvar.cum) |>
        transmute(time, se = sqrt(treatmentIntensive))

      fit_acm_data <- as_tibble(fit_acm$cum) |>
        left_join(var_acm) |>
        transmute(
          time,
          hr_est = exp(treatmentIntensive),
          hr_lwr = exp(treatmentIntensive - 1.96 * se),
          hr_upr = exp(treatmentIntensive + 1.96 * se)
        )

      fig_eff_acm <- ggplot(fit_acm_data) +
        aes(x = time, y = hr_est, ymin = hr_lwr, ymax = hr_upr) +
        geom_ribbon(alpha = 0.2) +
        geom_line(color = cols_tx[2]) +
        scale_y_log10(limits = c(0.15, 2.5), breaks = c(0.15, 0.25, 0.5, 1)) +
        scale_x_continuous(breaks = seq(0,10)) +
        theme_fig() +
        geom_segment(aes(x = 0, xend = max(time),
                         y = 1, yend = 1),
                     linetype = 2,
                     color = 'black') +
        labs(x = 'Years since randomization',
             y = 'Hazard ratio (95% CI)')

      fig_eff_acm <- add_annotations(fig_eff_acm, cols = cols_bg,
                                     ymax = 2.5,
                                     xmax = 10,
                                     ymin = 0.15,
                                     ymult = 0.75)

      times <- ndi_data$acm_years[ndi_data$acm_event>0]
      times <- times[times > 1.5]

      fit_cvd <- comp.risk(
        formula = Event(acm_years, cvd_event_cr) ~ treatment + cluster(randSite),
        cause = 1,
        model = 'prop',
        n.sim = 1000,
        # times = times,
        data = ndi_data
      )

      var_cvd <- as_tibble(fit_cvd$var.cum) |>
        transmute(time, se = sqrt(treatmentIntensive))

      fit_cvd_data <- as_tibble(fit_cvd$cum) |>
        left_join(var_cvd) |>
        transmute(
          time,
          hr_est = exp(treatmentIntensive),
          hr_lwr = exp(treatmentIntensive - 1.96 * se),
          hr_upr = exp(treatmentIntensive + 1.96 * se)
        ) |>
        add_row(.data = tibble(time = 0,
                               hr_est = 1,
                               hr_lwr = 1,
                               hr_upr = 1),
                .before = 2)

      fig_eff_cvd <- ggplot(fit_cvd_data) +
        aes(x = time, y = hr_est, ymin = hr_lwr, ymax = hr_upr) +
        geom_ribbon(alpha = 0.2) +
        geom_line(color = cols_tx[2]) +
        scale_y_log10(limits = c(0.15, 2.5), breaks = c(0.15, 0.25, 0.5, 1)) +
        scale_x_continuous(breaks = seq(0,10)) +
        theme_fig() +
        geom_segment(aes(x = 0, xend = max(time),
                         y = 1, yend = 1),
                     linetype = 2,
                     color = 'black') +
        labs(x = 'Years since randomization',
             y = 'Hazard ratio (95% CI)')

      fig_eff_cvd <- add_annotations(fig_eff_cvd, cols = cols_bg,
                                     ymax = 2.5,
                                     xmax = 10,
                                     ymin = 0.15,
                                     ymult = 0.75)

      tibble(eff_acm = list(fig_eff_acm),
             eff_cvd = list(fig_eff_cvd))

    }
  )


}
