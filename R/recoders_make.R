#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

recoders_make <- function() {

  list(

    variable = c('age_cat' = 'Age, years',
                 'sex' = 'Sex',
                 'race' = 'Race',
                 'ckd' = 'Chronic Kidney Disease',
                 'moca' = 'Cognitive Function',
                 'frail' = 'Frailty Status'),

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
      "Fit" = "Fit (FI\u22640.10)",
      "Pre_frail" = "Pre-frail (0.10<FI\u22640.21)",
      "Frail" = "Frail (FI>0.21)"
    )

  )



}
