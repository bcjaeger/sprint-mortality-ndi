#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @param trt_hetero
#' @param long_ds
#' @param recoders
#' @param long_inf
ndi_long_viz <- function(long_inf,
                         trt_hetero,
                         long_ds,
                         recoders) {


  rspec <- round_spec() |>
    round_using_magnitude(digits = c(2, 2, 1),
                          breaks = c(1, 10, Inf))

  recoders$levels$frail <- c("Fit", "Pre-frail", "Frail")
  recoders$group[c('Fit', "Pre_frail", "Frail")] <- c("Fit", "Pre-frail", "Frail")

  gg_spanners <- tibble(
    variable = recoders$variable,
    group = variable
  )

  gg_acm_incidence <- long_ds |>
    transmute(
      variable, group, trial_phase, treatment,
      mort_inc = table_glue(
        "{inc_mort_est} ({inc_mort_lwr}, {inc_mort_upr})",
        rspec = rspec
      )
    ) |>
    pivot_wider(values_from = mort_inc,
                names_from = treatment)

  gg_cvd_counts <- long_ds |>
    transmute(
      variable, group, trial_phase, treatment,
      cvd_label = table_glue(
        "{n_cvd}/{n_obs} ({100*n_cvd/n_obs})",
        rspec = rspec
      )
    ) |>
    pivot_wider(values_from = cvd_label,
                names_from = treatment)

  trt_hetero_merge_in <- trt_hetero |>
    pivot_wider(names_from = outcome,
                values_from = p.value,
                names_prefix = 'p_') |>
    rename(trial_phase = period)

  gg_acm <- long_inf |>
    left_join(trt_hetero_merge_in) |>
    left_join(gg_acm_incidence) |>
    transmute(
      variable = recode(variable, !!!recoders$variable),
      group = recode(group, !!!recoders$group),
      trial_phase,
      Standard,
      Intensive,
      est = mort_est,
      lwr = mort_lwr,
      upr = mort_upr,
      hr = table_glue("{mort_est} ({mort_lwr}, {mort_upr})",
                           rspec = rspec),
      p_cvd = table_value(p_cvd),
      p_acm = table_value(p_acm)
    ) |>
    mutate(across(starts_with("p_"), ~if_else(.x == '--', NA_character_, .x)))

  gg_cvd <- long_inf |>
    left_join(trt_hetero_merge_in) |>
    left_join(gg_cvd_counts) |>
    transmute(
      variable = recode(variable, !!!recoders$variable),
      group = recode(group, !!!recoders$group),
      trial_phase,
      Standard,
      Intensive,
      est = cvd_est,
      lwr = cvd_lwr,
      upr = cvd_upr,
      hr = table_glue("{cvd_est} ({cvd_lwr}, {cvd_upr})",
                           rspec = rspec),
      p_cvd = table_value(p_cvd),
      p_acm = table_value(p_acm)
    ) |>
    mutate(across(starts_with("p_"), ~if_else(.x == '--', NA_character_, .x)))

  gg <- list(acm = gg_acm,
             cvd = gg_cvd) |>
    map(
      .f = function(gg_data){

        split_1 <- split(gg_data, gg_data$variable) |>
          map_dfr(
            ~ {

              if(.x$variable[1] == 'Overall') return(.x)

              add_row(.x,
                      tibble(variable = .x$variable[1],
                             group = .x$variable[1],
                             trial_phase = c('trial', 'cohort'),
                             p_cvd = .x$p_cvd[1:2],
                             p_acm = .x$p_acm[1:2]),
                      .before = 1)

            }
          )

        split_2 <- split(split_1, split_1$trial_phase) |>
          map(select, -trial_phase) |>
          map(~mutate(.x, group = if_else(group == variable,
                                          true = group,
                                          false = paste0('   ', group)))) |>
          map(~mutate(.x, across(starts_with('p_'),
                                 ~ if_else(group == variable,
                                           true = .x,
                                           false = NA_character_)))) |>
          map(~mutate(.x, variable = factor(variable,
                                            levels = recoders$variable))) |>
          map(arrange, variable) |>
          map(~mutate(.x, x = rev(seq(n())))) |>
          map(
            ~ .x |>
              mutate(
                line_arrow_lwr_begin = if_else(lwr < 0.425,
                                               upr,
                                               NA_real_),
                line_arrow_lwr_end = if_else(lwr < 0.425,
                                             0.425,
                                             NA_real_),
                lwr = pmax(lwr, 0.425),
                est = if_else(est < 0.425, NA_real_, est)
              )
          )

        split_2

      }
    )


  y_col_0 <- exp(-5.25)
  y_col_1 <- exp(-3)
  y_col_2 <- exp(-1.5)
  y_col_3 <- exp(1.5)
  y_col_4 <- exp(2.5)
  size_text <- 3
  size_arrow <- 0.15

  ndi_forest_worker <- function(gg_data,
                                header_top,
                                header_ds,
                                y_col_0,
                                y_col_1,
                                y_col_2,
                                y_col_3,
                                y_col_4,
                                size_text,
                                size_arrow){

    xmax <- max(gg_data$x)

    y_trt <- exp((log(y_col_1) + log(y_col_2)) / 2)
    y_hr <- y_col_3 / 3
    y_mid <- exp((log(y_col_0) + log(y_col_4)) / 2)

    gg_header <- tribble(
      ~label                    , ~x    , ~y       , ~hjust, ~fontface,
      "Characteristic"          , xmax+2,  y_col_0 ,  0    , "bold",
      "Intensive"               , xmax+2,  y_col_1 ,  0.5  , "bold",
      "Standard"                , xmax+2,  y_col_2 ,  0.5  , "bold",
      "Hazard ratio (95% CI)"   , xmax+2,  y_hr    ,  0.5  , "bold",
      "P value"                 , xmax+2,  y_col_4 ,  1    , "bold",
      header_ds                 , xmax+1,  y_trt   ,  0.5  , "italic",
      header_top                , xmax+3.5,  y_mid   ,  0.5  , "bold"
    )


    gg_rect <- tibble(
      xmin = seq(xmax+1) - 1/2,
      xmax = seq(xmax+1) + 1/2,
      ymin = y_col_0,
      ymax = y_col_4
    ) |>
      filter(seq(n()) %% 2 == 0)

    gg_arrows_bottom <- tibble(
      x = c(0, 0),
      y = c(exp(-1/4), exp(1/4)),
      yend = c(exp(-1), exp(1)),
      label = c("Favors\nIntensive", "Favors\nStandard")
    )

    fig_bottom <- ggplot(gg_arrows_bottom) +
      aes(x=x, xend=x, y=y, yend=yend, label=label) +
      geom_segment(arrow=arrow(length = unit(size_arrow, 'cm'))) +
      geom_text(size = size_text,
                hjust = c(1,0),
                vjust = 1/2) +
      scale_y_log10(limits = c(y_col_0, y_col_4+0.1),
                    breaks = c(0.5, 1, 2),
                    expand = c(0,0)) +
      coord_flip() +
      theme_void()

    fig_main <- ggplot(gg_data) +
      aes(x = x) +
      geom_rect(data = gg_rect,
                inherit.aes = FALSE,
                aes(xmin = xmin,
                    xmax = xmax,
                    ymin = ymin,
                    ymax = ymax),
                fill = 'grey',
                alpha = 1/5) +
      geom_text(aes(label = group, y = y_col_0),
                hjust = 0, size = size_text) +
      geom_text(aes(label = Intensive, y = y_col_1),
                hjust = 0.5, size = size_text) +
      geom_text(aes(label = Standard, y = y_col_2),
                hjust = 0.5, size = size_text) +
      geom_text(aes(label = hr, y = y_col_3),
                hjust = 0.5, size = size_text) +
      geom_text(aes(label = p_acm, y = y_col_4),
                hjust = 1, size = size_text) +
      geom_text(data = gg_header,
                aes(x = x,
                    y = y,
                    label = label,
                    hjust = hjust,
                    fontface = fontface),
                size = size_text) +
      geom_vline(xintercept = max(gg_header$x)-3/4) +
      geom_vline(xintercept = xmax + 1/2) +
      geom_segment(y = 0, yend = 0,
                   x = 0, xend = xmax + 1/3,
                   color = 'grey',
                   alpha = 0.5,
                   linetype = 2) +
      geom_segment(y = log(0.5)/2, yend = log(2)/2,
                   x = 0, xend = 0) +
      geom_point(aes(y = est), shape = 15, size = 3) +
      geom_linerange(aes(ymin = lwr, ymax = upr)) +
      geom_segment(aes(xend = x,
                       y = line_arrow_lwr_begin,
                       yend = line_arrow_lwr_end),
                   arrow = arrow(length = unit(size_arrow, 'cm'))) +
      scale_y_log10(limits = c(y_col_0, y_col_4+0.1), breaks = c(0.5, 1, 2),
                    expand = c(0,0)) +
      scale_x_continuous(limits = c(1, max(gg_header$x))) +
      coord_flip() +
      theme_bw() +
      labs(y = 'Hazard ratio') +
      theme(panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.title = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.line = element_blank())

    cowplot::plot_grid(fig_main,
                       fig_bottom,
                       align = 'v',
                       nrow=2, rel_heights = c(14, 1))

  }

  figs_acm <- map2(
    .x = gg$acm,
    .y = c("Observational phase", "Trial phase"),
    .f = ndi_forest_worker,
    header_ds = "Incidence rate (95% CI) per 1000 person-years",
    y_col_0 = y_col_0,
    y_col_1 = y_col_1,
    y_col_2 = y_col_2,
    y_col_3 = y_col_3,
    y_col_4 = y_col_4,
    size_text = size_text,
    size_arrow = size_arrow
  )

  figs_cvd <- map2(
    .x = gg$cvd,
    .y = c("Observational phase", "Trial phase"),
    .f = ndi_forest_worker,
    header_ds = "no. of CVD deaths / no. at risk (%)",
    y_col_0 = y_col_0,
    y_col_1 = y_col_1,
    y_col_2 = y_col_2,
    y_col_3 = y_col_3,
    y_col_4 = y_col_4,
    size_text = size_text,
    size_arrow = size_arrow
  )

  list(acm = figs_acm,
       cvd = figs_cvd)

}

