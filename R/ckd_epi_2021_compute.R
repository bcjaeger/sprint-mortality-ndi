#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param age
#' @param sex
#' @param screat
ckd_epi_2021_compute <- function(age, sex, screat) {

  min_term <- case_when(
    sex == 'Female' ~ pmin(screat / 0.7, 1),
    sex == 'Male' ~ pmin(screat / 0.9, 1)
  )

  max_term <- case_when(
    sex == 'Female' ~ pmax(screat / 0.7, 1),
    sex == 'Male' ~ pmax(screat / 0.9, 1)
  )

  case_when(
    sex == 'Female' ~
      142*(min_term^(-0.241))*(max_term^(-1.200))*(0.9938^(age))*1.012,
    sex == 'Male' ~
      142*(min_term^(-0.302))*(max_term^(-1.200))*(0.9938^(age))
  )

}
