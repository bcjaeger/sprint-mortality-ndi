
# tar_load(base_sub_overall)
# ndi_data_list <- base_sub_overall
# ndi_data <- ndi_data_list$overall

ndi_base_viz_incidence <- function(ndi_data_list) {

  cols_bg <- viridis(3)
  cols_tx <- pal_lancet()(2)

  figs <- map_dfr(
    .x = ndi_data_list,
    .id = 'group',
    .f = function(ndi_data){

      fit_surv <- survfit(Surv(acm_years, acm_event) ~ treatment,
                          data = ndi_data)

      ymax_candidates <- seq(0.05, 1, by = 0.05)

      ymax_true <- max(1-fit_surv$lower)

      ymax <- ymax_candidates[min(which(ymax_candidates > ymax_true))]

      fig_kap <- survminer::ggsurvplot(
        fit_surv,
        data = ndi_data,
        fun = 'event',
        palette = "lancet",
        ggtheme = theme_fig(),
        fontsize = 12,
        conf.int = TRUE,
        risk.table.fontsize = 3.5,
        legend.title = "Treatment",
        legend.labs = c("Standard", "Intensive"),
        risk.table = 'absolute',
        break.x.by = 1,
        ylab = 'Cumulative incidence',
        xlab = 'Years since randomization',
        ylim = c(0, ymax),
        break.y.by = 0.05,
        censor.size = 0,
        surv.scale = 'percent',
        tables.theme = theme_tbl(),
        tables.height = 1/8
      )

      fig_kap$plot <- add_annotations(
        fig_kap$plot,
        cols = cols_bg,
        ymax = ymax,
        mid_label = '\nTrial\nAnd\nObservational\nPhase',
        x_cohort_phase = 7
      )

      fig_kap$plot <- fig_kap$plot +
        scale_x_continuous(expand = c(0, 0, 0, 0),
                           breaks = 1:12)

      fit_cr <- cuminc(ftime   = ndi_data$acm_years,
                       fstatus = factor(ndi_data$cvd_event_cr,
                                        levels = c(0, 1, 2),
                                        labels = c("- Censored",
                                                   "- CVD mortality",
                                                   "- Non-CVD mortality")),
                       group   = ndi_data$treatment,
                       cencode = '- Censored')

      data_cr <-
        map_dfr(.x = fit_cr[1:4],
                .f = as_tibble,
                .id = 'fit') |>
        separate(fit, into = c('treatment','event'), sep = ' - ') |>
        mutate(
          treatment = factor(
            x = treatment,
            levels = c("Standard", "Intensive")
          ),
          lwr = est - 1.96 * sqrt(var),
          upr = est + 1.96 * sqrt(var)
        )

      ymax_true <- max(data_cr$upr)

      ymax <- ymax_candidates[min(which(ymax_candidates > ymax_true))]

      xmax <- max(data_cr$time)

      fig_cr <- ggplot(data_cr) +
        aes(x = time,
            y = est,
            col = treatment,
            group = interaction(treatment, event)) +
        geom_line(linewidth = 1.5) +
        geom_ribbon(
          aes(x = time,
              ymin = est - sqrt(var) * 1.96,
              ymax = est + sqrt(var) * 1.96,
              fill = treatment,
              group = interaction(treatment, event)),
          alpha = 0.30,
          inherit.aes = FALSE
        ) +
        theme_fig(legend.position = 'top') +
        labs(linetype = 'Event',
             color = 'Treatment',
             fill = 'Treatment') +
        ylab('Cumulative incidence') +
        xlab('Years since randomization') +
        scale_y_continuous(labels = scales::percent,
                           breaks = seq(0, ymax, by = 0.05),
                           limits = c(0, ymax)) +
        scale_x_continuous(breaks = c(0:13),
                           expand = c(0, 0, 0, 0)) +
        scale_color_manual(values = cols_tx) +
        scale_fill_manual(values = cols_tx)

      y_text <- data_cr |>
        group_by(treatment, event) |>
        filter(time == max(time)) |>
        group_by(event) |>
        summarize(est_max = max(est) + 0.02,
                  est_min = min(est) - 0.04)

      fig_cr <- add_annotations(
        fig_cr,
        cols = cols_bg,
        ymax = ymax,
        xmax = xmax,
        mid_label = '\nTrial\nAnd\nObservational\nPhase',
        x_cohort_phase = 7
      ) +
        annotate(geom = 'text',
                 x = 11.5,
                 y = y_text$est_max[y_text$event == 'Non-CVD mortality'],
                 size = 5,
                 label = 'Non-CVD\nMortality') +
        annotate(geom = 'text',
                 x = 11.5,
                 y = y_text$est_min[y_text$event == 'CVD mortality'],
                 size = 5,
                 label = 'CVD\nMortality')


      tibble(inc_acm = list(fig_kap),
             inc_cvd = list(fig_cr))

    }
  )

}
