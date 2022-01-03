#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param sasinet_drive
ndi_load <- function(sasinet_drive = "Z", fname, ...) {

  path <- file.path(
    paste0(sasinet_drive, ":"),
    "npajewski",
    "NDI",
    "Data",
    fname
  )

  read_csv(path) |>
    transmute(
      # ... makes it easy to modify the call to this function in targets
      # for including additional variables or modifying them.
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
      egfr_2021 = ckd_epi_2021_compute(age, sex, screat),
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

}
