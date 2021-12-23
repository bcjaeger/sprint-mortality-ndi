## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

subgroups <- tibble(group = c("overall",
                              "age_cat",
                              "sex",
                              "race",
                              "ckd",
                              "moca",
                              "frail"))

list(

  tar_target(recoders, recoders_make()),

  tar_target(
    ndi_baseline,
    ndi_load(sasinet_drive = 'Z',
             fname = 'longterm_death.csv',
             # participant identifier
             pid,
             # study site
             randSite,
             # all cause mortality time and statue
             acm_years, acm_event,
             # cvd mortality status
             cvd_event = as.numeric(cvd_event_cr == 1),
             cvd_event_cr)
  ),

  tar_target(
    ndi_longitudinal,
    ndi_load(sasinet_drive = 'Z',
             fname = 'longterm_death_td_subgroup.csv',
             pid,
             randSite,
             trial_phase = factor(trial_phase,
                                  levels = c(0,1),
                                  labels = c("trial", "cohort")),
             start_yrs, end_yrs,
             mortality, cvd_mortality)
  ),

  # arrow assignment is necessary to make 'long' an object that
  # will be recognized in downstream tar_ functions
  long <- tar_map(
    values = subgroups,
    # make the sub-groups
    tar_target(long_sub, ndi_split_group(ndi_longitudinal, group)),
    # ds = descriptive statistics
    tar_target(long_ds, ndi_long_describe(long_sub)),
    # inf = inference
    tar_target(long_inf, ndi_long_infer(long_sub))
  ),

  tar_combine(
    long_ds,
    long[[2]],
    command = bind_rows(!!!.x, .id = "variable") |>
      mutate(variable = str_remove(variable, '^long_ds_'))
  ),

  tar_combine(
    long_inf,
    long[[3]],
    command = bind_rows(!!!.x, .id = "variable") |>
      mutate(variable = str_remove(variable, '^long_inf_'))
  ),

  tar_target(
    long_viz_forest, ndi_long_viz_forest(long_inf, recoders)
  ),

  base <- tar_map(
    values = subgroups,
    # make the sub-groups
    tar_target(base_sub, ndi_split_group(ndi_baseline, group)),
    # ds = descriptive statistics
    tar_target(base_ds, ndi_base_describe(base_sub)),
    # inference
    tar_target(base_inf, ndi_base_infer(base_sub)),
    # cumulative incidence of acm and cvd
    tar_target(base_viz_inc, ndi_base_viz_incidence(base_sub)),
    # visualize HRs over time
    tar_target(base_viz_eff, ndi_base_viz_tv_effect(base_sub))
  ),

  # ndi_long_describe(ndi_data_list)

  tar_combine(
    base_ds,
    base[[2]],
    command = bind_rows(!!!.x, .id = "variable") |>
      mutate(variable = str_remove(variable, '^base_ds_'))
  ),

  tar_combine(
    base_inf,
    base[[3]],
    command = bind_rows(!!!.x, .id = "variable") |>
      mutate(variable = str_remove(variable, '^base_inf_'))
  ),

  tar_combine(
    base_viz_inc,
    base[[4]],
    command = bind_rows(!!!.x, .id = "variable") |>
      mutate(variable = str_remove(variable, '^base_viz_inc_'))
  ),

  tar_combine(
    base_viz_eff,
    base[[5]],
    command = bind_rows(!!!.x, .id = "variable") |>
      mutate(variable = str_remove(variable, '^base_viz_eff_'))
  ),

  tar_render(manuscript, "doc/manuscript.Rmd")


)








