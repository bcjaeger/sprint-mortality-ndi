#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param long_inf
ndi_long_viz_forest <- function(long_inf, recoders) {

  rspec <- round_spec() |>
    round_using_decimal(digits = 2)

  data_init <- long_inf |>
    mutate(variable = str_remove(variable, 'long_inf_'),
           variable = recode(variable, !!!recoders$variable),
           group = recode(group, !!!recoders$group))

  data_bind <- tibble(variable = unique(data_init$variable),
                      group = recode(variable, !!!recoders$variable)) |>
    expand_grid(tibble(trial_phase = c("cohort", "trial")))

  data_gg <- bind_rows(data_bind, data_init) |>
    mutate(variable = fct_inorder(variable),
           group = fct_inorder(group)) |>
    arrange(variable, group, trial_phase) |>
    slice(-c(1,3)) |>
    group_by(trial_phase) |>
    mutate(
      x = seq(n(), 1),
      .before = 1,
      across(where(is.factor), as.character),
      group = if_else(
        condition = group == variable | group == 'Overall',
        true = group,
        false = paste(" ", group)
      ),
      trial_phase = factor(
        trial_phase,
        levels = c('trial', 'cohort'),
        labels = c("Trial Phase", "Cohort Phase")
      ),
      fill = 1 +
        as.numeric(variable == 'Overall') +
        as.numeric(variable == 'Overall' & trial_phase == 'Cohort Phase') +
        as.numeric(variable != group) +
        as.numeric(variable != group & trial_phase == 'Cohort Phase'),
      fill = factor(fill),
      label = table_value(mort_est, rspec = rspec),
      label = replace(label, label == 'NA', "")
    )

  range_y <- range(data_gg$mort_lwr, data_gg$mort_upr, na.rm = TRUE)

  data_gg <- data_gg |>
    mutate(
      text_y = if_else(
        condition = trial_phase == 'Trial Phase',
        true = range_y[2],
        false = range_y[1]
      ),
      hjust = if_else(
        condition = trial_phase == 'Trial Phase',
        true = 0,
        false = 1
      ),
      .before = fill
    )


  data_bind <- tibble(x = max(data_gg$x) + 1,
                      text_y = range_y[c(2,1)],
                      hjust = c(0,1),
                      variable = '',
                      group = '',
                      label = 'HR',
                      fill = '1',
                      trial_phase = c('Trial Phase', 'Cohort Phase'))


  data_gg <- bind_rows(data_bind, data_gg) |>
    mutate(
      trial_phase = factor(
        trial_phase,
        levels = c("Trial Phase", "Cohort Phase")
      )
    )

  fig_hrs <- ggplot(data_gg) +
    aes(x = x,
        y = mort_est,
        ymin = mort_lwr,
        ymax = mort_upr) +
    geom_rect(
      aes(
        xmin = x - 0.5,
        xmax = x + 0.5,
        ymin = range_y[1] * 0.85,
        ymax = range_y[2] * 1.15,
        fill = fill
      )
    ) +
    geom_text(aes(y = text_y, label = label)) +
    scale_fill_manual(
      values = c(
        "#FFFFFF33",
        "#44015433",
        "#FDE72533"
      ),
      guide = "none"
    ) +
    geom_point(pch = 15, size = 4) +
    geom_errorbar(width = 0.15) +
    geom_hline(yintercept = 1, linetype = 3) +
    coord_flip() +
    scale_x_continuous(breaks = seq(1, max(data_gg$x)),
                       labels = rev(unique(data_gg$group))) +
    theme_bw() +
    theme(text = element_text(family = 'serif', size = 12),
          axis.text = element_text(family = 'serif', color = 'black', size = 12),
          panel.grid = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank()) +
    facet_wrap(~trial_phase) +
    labs(y = 'Hazard ratio for all-cause mortality, intensive versus standard treatment') +
    scale_y_log10()

  fig_hrs

}
