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

      # Using the tt() approach instead of residual analysis
      # this gives model estimates of the time-varying effect
      fit_acm <- coxph(
        formula = Surv(acm_years, acm_event) ~ treatment +
          tt(treatment) + cluster(randSite),
        tt = function(x, t, ...)
          as.numeric(x == 'Intensive') * ns(t, df=6),
        data = ndi_data
      )

      plot_times <- seq(0, 10, length.out = 1000)

      spline_x <- cbind(1, ns(x = plot_times, df=6))

      yhat <- as.numeric(spline_x %*% coef(fit_acm))

      ses <- sqrt(diag(spline_x %*% vcov(fit_acm) %*% t(spline_x)))

      fit_acm_data <- tibble(
        time = plot_times,
        hr_est = exp(yhat),
        hr_lwr = exp(yhat - 1.96 * ses),
        hr_upr = exp(yhat + 1.96 * ses)
      )

      # do not remove the rm() statement.
      # ggplot saves fit_acm in its environment, which causes
      # the file size of the current target to be massive.
      rm(fit_acm)

      fig_eff_acm <- ggplot(fit_acm_data) +
        aes(x = time, y = hr_est, ymin = hr_lwr, ymax = hr_upr) +
        geom_ribbon(alpha = 0.2) +
        geom_line(color = cols_tx[2]) +
        scale_y_log10(limits = c(0.35, 2.5),
                      breaks = c(0.5, 1)) +
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
                                     ymin = 0.35,
                                     ymult = 0.75)

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
