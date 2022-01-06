#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ndi_baseline
ndi_tabulate_characteristics <- function(ndi_baseline, recoders) {


  ndi_baseline$ehr_ancillary <- sample(x = c('No','Yes'),
                                       size = nrow(ndi_baseline),
                                       replace = TRUE)

  ndi_tbl_data <- ndi_baseline |>
    select(ehr_ancillary,
           age_yrs,
           any_of(names(recoders$variable)))

  for(r in seq_along(recoders$variable)){

    variable <- names(recoders$variable)[r]

    # recode if the current variable is categorical
    if (!is.null(recoders$levels[[variable]])) {
      ndi_tbl_data[[variable]] <- recode(
        .x = ndi_tbl_data[[variable]],
        !!!recoders$levels[[variable]]
      )

    }

    if(variable != 'overall'){

      attr(ndi_tbl_data[[variable]], 'label') <- recoders$variable[r]

    }

  }

  tbl_summary(data = ndi_tbl_data,
              by = 'ehr_ancillary',
              statistic = list(
                age_yrs ~ c("{mean} ({sd})", "{median} ({p25}, {p75})"),
                all_categorical() ~ "{p}"
              ),
              label = list(age_cat ~ paste("Age", recoders$levels$age_cat[2]),
                           sex ~ recoders$levels$sex[2],
                           race ~ recoders$levels$race[2],
                           moca ~ paste("MoCA", recoders$levels$moca[2])),
              type = list(age_yrs ~ "continuous2",
                          age_cat ~ "dichotomous",
                          sex ~ "dichotomous",
                          race ~ "dichotomous",
                          ckd_2021 ~ "dichotomous",
                          moca ~ "dichotomous",
                          frail ~ "categorical"),
              value = list(age_cat = recoders$levels$age_cat[2],
                           sex = recoders$levels$sex[2],
                           race = recoders$levels$race[2],
                           ckd_2021 = recoders$levels$ckd_2021[2],
                           moca = recoders$levels$moca[2]),
              missing = 'no'
              ) |>
    add_overall() |>
    add_p()


}
