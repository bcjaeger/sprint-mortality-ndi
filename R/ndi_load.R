#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param sasinet_drive
ndi_load <- function(sasinet_drive = "Z", fname, ...) {

  path <- file.path(
    paste0(sasinet_drive, ""),
    "npajewski",
    "NDI",
    "Data",
    fname
  )


  data_in <- read_csv(path) |>
    transmute(
      # ... makes it easy to modify the call to this function in targets
      # for including additional variables or modifying them.
      pid,
      ...,
      age_yrs = age,
      age_cat = factor(sub_senior,
                       levels = c(0, 1),
                       labels = c("lt75_years", "gteq75_years")),
      sex = factor(female,
                   levels = c(0,1),
                   labels = c("Male","Female")),
      race = factor(is.na(race_black),
                    levels = c(TRUE, FALSE),
                    labels = c("Non_Black", "Black")),
      ckd = factor(eGFR_CKDEPI < 60,
                   levels = c(FALSE, TRUE),
                   labels = c("No", "Yes")),
      # New creatinine based race-agnostic eGFR equation
      egfr_2021 = ckd_epi_2021_compute(age = age,
                                       sex = sex,
                                       screat = screat),
      ckd_2021 = factor(egfr_2021 < 60,
                        levels = c(FALSE, TRUE),
                        labels = c("No", "Yes")),
      moca = factor(moca_status,
                    levels = c(0, 1),
                    labels = c("gt10th_percentile",
                               "lteq10th_percentile")),
      frail = factor(frail_status,
                     levels = c(0,1,2),
                     labels = c("Fit",
                                "Pre_frail",
                                "Frail")),
      # treatment indicator with factor structure is useful for tables
      treatment = factor(intensive,
                         levels = c(0,1),
                         labels = c('Standard', 'Intensive'))
    )


  # indicate ancillary study membership for baseline data
  if(fname == 'longterm_death.csv'){

    ancillary <- read_csv('ehr/SPRINT_EHR_SBP_LongTerm_PID_011522.csv')

    names(ancillary) <- tolower(names(ancillary))

    data_in$ehr_ancillary <- if_else(
      condition = data_in$pid %in% ancillary$pid,
      true = 'Yes',
      false = 'No'
    )

  }

  data_in

}
