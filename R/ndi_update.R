# Pull in the 2023 ndi data

ndi_baseline_update <- function(ndi_baseline) {

  ndi_2023 <-
    read_csv("T:/sprint/npajewski/NDI/Data/longterm_death_2023.csv") %>%
    select(pid, matches("^acm|^cvd")) %>%
    mutate(cvd_event = if_else(cvd_event_cr == 2, 0, cvd_event_cr))

  ndi_baseline %>%
    select(-matches("^acm|^cvd")) %>%
    left_join(ndi_2023)

}

ndi_longitudinal_update <- function(ndi_longitudinal){

  ndi_2023 <-
    read_csv("T:/sprint/npajewski/NDI/Data/longterm_death_2023.csv") %>%
    select(pid, intensive, years_clo_end, matches("^acm|^cvd")) %>%
    mutate(cvd_event = if_else(cvd_event_cr == 2, 0, cvd_event_cr)) %>%
    group_by(pid) %>%
    group_split() %>%
    map_dfr(expand_row) %>%
    mutate(trial_phase = factor(trial_phase,
                                levels = c(0, 1),
                                labels = c("trial", "cohort")))

  ndi_longitudinal %>%
    select(-start_yrs, -end_yrs, -mortality, -cvd_mortality) %>%
    left_join(ndi_2023)

}

ndi_longitudinal_assert_replication_nmp <- function(){

  attempt <- read_csv('T:/sprint/npajewski/NDI/Data/longterm_death.csv') %>%
    select(pid,
           years_clo_end,
           intensive,
           acm_years,
           acm_event,
           cvd_event_cr) %>%
    group_by(pid) %>%
    group_split() %>%
    map_dfr(expand_row)

  target <-
    read_csv('T:/sprint/npajewski/NDI/Data/longterm_death_td_subgroup.csv') %>%
    select(pid, intensive1:trial_phase)

  testthat::expect_equal(target, attempt)

}

expand_row <- function(x){

  out <- list()

  if (x$acm_years < x$years_clo_end){

    out[[1]] <- tibble(
      pid = x$pid,
      intensive1 = x$intensive,
      intensive2 = 0,
      start_yrs = 0,
      end_yrs = x$acm_years,
      mortality = x$acm_event,
      cvd_mortality = x$cvd_event_cr,
      trial_phase = 0
    )

  } else if (x$acm_years >= x$years_clo_end){

    out[[1]] <- tibble(
      pid = x$pid,
      intensive1 = x$intensive,
      intensive2 = 0,
      start_yrs = 0,
      end_yrs = x$years_clo_end,
      mortality = 0,
      cvd_mortality = 0,
      trial_phase = 0
    )

    out[[2]] <- tibble(
      pid = x$pid,
      intensive1 = x$intensive,
      intensive2 = x$intensive,
      start_yrs = x$years_clo_end,
      end_yrs = x$acm_years,
      mortality = x$acm_event,
      cvd_mortality = x$cvd_event_cr,
      trial_phase = 1
    )

  }

  bind_rows(out)

}
