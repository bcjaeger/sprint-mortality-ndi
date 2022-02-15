#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param date

bp_long_viz <- function(date_ehr = "02-08-22",
                        date_trial = "01-15-22") {

  cols_bg <- viridis(3)
  cols_tx <- pal_lancet()(2)
  cols_diff <- c("maroon4","black")

  .date_ehr <- str_remove_all(date_ehr, fixed('-'))
  .date_trial <- str_remove_all(date_trial, fixed('-'))


  fname_ehr <- paste0("SPRINT_EHR_SBP_LongTerm_EHRonly_", .date_ehr, ".csv")
  fname_trial <- paste0("SPRINT_EHR_SBP_GLIMMIX_JAMA_IM_REDO_", .date_trial, ".csv")

  # Blood pressure levels over time ----

  bp_ehr <- read_csv(file.path('ehr', fname_ehr)) |>
    select(-Statement, -Alpha, -tValue, -DF, -StdErr) |>
    separate(Label, into = c('group', 'time_months'), sep = ';') |>
    mutate(time_months = as.numeric(time_months),
           group = recode(group,
                          "STD" = "Standard (EHR)",
                          "INT" = "Intensive (EHR)",
                          "DIFF" = "Difference (EHR)"))

  bp_trial <- read_csv(file.path('ehr', fname_trial)) |>
    select(-Statement, -Alpha, -tValue, -DF, -StdErr) |>
    separate(Label, into = c('group', 'source', 'time_months'), sep = ';') |>
    mutate(time_months = as.numeric(time_months)) |>
    filter(source == 'CLINIC') |>
    select(-source) |>
    mutate(group = recode(group,
                          "STD" = "Standard (trial)",
                          "INT" = "Intensive (trial)",
                          "DIFF" = "Difference (trial)"))

  bp_levels <- bind_rows(bp_ehr, bp_trial) |>
    filter(!str_detect(group, '^Diff')) |>
    separate(group,
             into = c('treatment', 'source'),
             sep = ' ') |>
    mutate(source = str_remove_all(source, pattern = '\\(|\\)'),
           source = recode(source, 'trial' = 'Trial'),
           treatment = factor(treatment, levels = c("Standard", "Intensive")))

  # Blood pressure difference over time ----

  bp_diff <- bind_rows(bp_ehr, bp_trial) |>
    filter(str_detect(group, '^Diff')) |>
    separate(group,
             into = c('drop_me', 'source'),
             sep = ' ') |>
    mutate(source = str_remove_all(source, pattern = '\\(|\\)'),
           source = recode(source, 'trial' = 'Trial'),
           source = fct_reorder2(source, .x = time_months, .y = Estimate)) |>
    select(-drop_me)

  # Figure; BP levels ----

  fig_levels <- ggplot(bp_levels) +
    aes(x = time_months/12,
        y = Estimate,
        color = treatment,
        linetype = source) +
    geom_line(size = 0.85) +
    labs(y = 'Systolic blood pressure, mm Hg',
         x = 'Years since randomization',
         fill = 'Treatment',
         color = 'Treatment',
         linetype = 'Data Source') +
    scale_x_continuous(limits = c(0,10),
                       breaks = seq(0,10)) +
    scale_y_continuous(limits = c(110, 152),
                       breaks = seq(110, 150, by=5)) +
    scale_color_manual(values = cols_tx) +
    scale_fill_manual(values = cols_tx) +
    scale_linetype_manual(values = c(1,2)) +
    theme_fig(legend.position = 'right') +
    geom_ribbon(alpha = 0.2,
                mapping = aes(x = time_months/12,
                              ymin = Lower,
                              ymax = Upper,
                              fill = treatment,
                              group = interaction(treatment, source)),
                inherit.aes = FALSE)

  fig_levels <-
    add_annotations(fig_levels,
                    cols = cols_bg,
                    ymax = 152,
                    xmax = 10,
                    ymin = 110,
                    ymult = 0.965,
                    x_cohort_phase = 6)

  # Figure; BP Diff ----

  fig_diff <- ggplot(bp_diff) +
    aes(x = time_months/12,
        y = Estimate,
        linetype = source) +
    geom_line(color = cols_tx[2], size = 0.85) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper),
                fill = cols_tx[2],
                alpha = 0.2) +
    labs(y = 'Between-group difference in systolic blood pressure, mm Hg',
         x = 'Years since randomization',
         linetype = 'Data Source') +
    scale_linetype_manual(values = c(2,1)) +
    scale_x_continuous(limits = c(0,10),
                       breaks = seq(0,10)) +
    scale_y_continuous(limits = c(-5, 25),
                       breaks = seq(-5,25, by=5)) +
    theme_fig(legend.position = 'right')

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
