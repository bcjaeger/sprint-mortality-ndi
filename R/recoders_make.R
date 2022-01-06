#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

recoders_make <- function() {

  list(

    variable = c("overall" = "Overall",
                 "age_cat" = "Age group",
                 "age_yrs" = "Age, years",
                 "sex" = "Sex",
                 "race" = "Race",
                 "ckd_2021" = "Chronic Kidney Disease",
                 "moca" = "Cognitive Function",
                 "frail" = "Frailty Status"),

    group = c(
      "overall" = "Overall",
      "lt75_years" = "<75 years",
      "gteq75_years" = "\u226575 years",
      "Male" = "Male",
      "Female" = "Female",
      "Non_Black" = "Non-Black",
      "Black" = "Black",
      "No" = "No",
      "Yes" = "Yes",
      "gt10th_percentile" = ">10th percentile",
      "lteq10th_percentile" = "\u226410th percentile",
      "Fit" = "Fit (FI \u2264 0.10)",
      "Pre_frail" = "Pre-frail (0.10 < FI \u2264 0.21)",
      "Frail" = "Frail (FI > 0.21)"
    ),

    levels = list(
      age_cat = c("lt75_years" = "<75 years",
                  "gteq75_years" = "\u226575 years"),
      sex = c("Male" = "Male",
              "Female" = "Female"),
      race = c("Non_Black" = "Non-Black",
               "Black" = "Black"),
      ckd_2021 = c("No" = "No",
                   "Yes" = "Yes"),
      moca = c("gt10th_percentile" = ">10th percentile",
               "lteq10th_percentile" = "\u226410th percentile"),
      frail = c("Fit" = "Fit (FI\u22640.10)",
                "Pre_frail" = "Pre-frail (0.10<FI\u22640.21)",
                "Frail" = "Frail (FI>0.21)")
    )

  )



}
