#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param date
bp_long_viz <- function(date = "12-27-21") {

  cols_bg <- viridis(3)
  cols_tx <- pal_lancet()(2)

  .date <- str_remove_all(date, fixed('-'))

  fname <- paste0("SPRINT_EHR_SBP_GLIMMIX_LongTerm_", .date, ".csv")

  bp <- read_csv(file.path('ehr', fname)) |>
    select(-Statement, -Alpha, -tValue, -DF, -StdErr) |>
    separate(Label, into = c('group', 'time_months'), sep = ';') |>
    mutate(time_months = as.numeric(time_months),
           group = recode(group,
                          "STD" = "Standard",
                          "INT" = "Intensive",
                          "DIFF" = "Difference"))

  bp_levels <- filter(bp, group %in% c("Standard",
                                       "Intensive"))
  bp_diff <- filter(bp, group == 'Difference')

  bp_targets <- tibble(
    x = c(0, 0),
    xend = c(2.54, 2.54),
    y = c(140, 120),
    yend = c(140, 120),
    group = c("Standard", "Intensive")
  )

  bp_diff_targets <- tibble(
    x = 0,
    xend = 2.54,
    y = 20,
    yend = 20
  )

  fig_levels <- ggplot(bp_levels) +
    aes(x = time_months/12,
        y = Estimate,
        color = group) +
    geom_line(size = 1) +
    labs(y = 'Systolic blood pressure, mm Hg',
         x = 'Years since randomization',
         fill = 'Treatment',
         color = 'Treatment') +
    scale_x_continuous(limits = c(0,10),
                       breaks = seq(0,10)) +
    scale_y_continuous(limits = c(120, 142),
                       breaks = seq(120, 140, by=5)) +
    scale_color_manual(values = cols_tx) +
    scale_fill_manual(values = cols_tx) +
    theme_fig(legend.position = 'top') +
    geom_ribbon(alpha = 0.2,
                mapping = aes(x = time_months/12,
                              ymin = Lower,
                              ymax = Upper,
                              fill = group),
                inherit.aes = FALSE) +
    geom_segment(data=bp_targets,
                 size = 1.25,
                 inherit.aes = FALSE,
                 aes(x = x,
                     xend = xend,
                     y = y,
                     yend = yend,
                     color = group))

  fig_levels <-
    add_annotations(fig_levels,
                    cols = cols_bg,
                    ymax = 142,
                    xmax = 10,
                    ymin = 120,
                    ymult = 0.9,
                    x_cohort_phase = 6)

  fig_diff <- ggplot(bp_diff) +
    aes(x = time_months/12,
        y = Estimate) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper),
                alpha = 0.2) +
    geom_segment(inherit.aes = FALSE,
                 size = 1.25,
                 aes(x = 0,
                     xend = 2.54,
                     y = 20,
                     yend = 20)) +
    labs(y = 'Between-group difference in systolic blood pressure, mm Hg',
         x = 'Years since randomization',
         fill = 'Treatment',
         color = 'Treatment') +
    scale_x_continuous(limits = c(0,10),
                       breaks = seq(0,10)) +
    scale_y_continuous(limits = c(-5, 25),
                       breaks = seq(-5,25, by=5)) +
    theme_fig()

  fig_diff <-
    add_annotations(fig_diff,
                    cols = cols_bg,
                    ymax = 25,
                    xmax = 10,
                    ymin = -5,
                    ymult = 0.85,
                    x_cohort_phase = 6)

  list(levels = fig_levels,
       diff = fig_diff)

}
