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


# TESTS comparing my eqn to the online calculator available here:
# https://www.kidney.org/professionals/kdoqi/gfr_calculator

# FORMAT for results below
# test <number>
# my_r_function(characteristics of test patient)
#                my answer: <result using my function>
# online calculator answer: <result from url above>

# test 1
# ckd_epi_2021_compute(age = 60, sex = 'Female', screat = 1.2)
#                             MY EQN: 51.82133
# CKD-EPI creatinine equation (2021): 52

# test 2
# ckd_epi_2021_compute(age = 60, sex = 'Male', screat = 1.2)
#                             MY EQN: 69.23113
# CKD-EPI creatinine equation (2021): 69

# test 3
# ckd_epi_2021_compute(age = 60, sex = 'Male', screat = 0.6)
#                             MY EQN: 110.5115
# CKD-EPI creatinine equation (2021): 111

# test 4
# ckd_epi_2021_compute(age = 70, sex = 'Male', screat = 0.6)
#                             MY EQN: 103.8478
# CKD-EPI creatinine equation (2021): 104

# test 5
# ckd_epi_2021_compute(age = 70, sex = 'Female', screat = 1)
#                             MY EQN: 60.60605
# CKD-EPI creatinine equation (2021): 61

# test 6
# ckd_epi_2021_compute(age = 70, sex = 'Female', screat = 1.5)
#                             MY EQN: 37.25688
# CKD-EPI creatinine equation (2021): 37



