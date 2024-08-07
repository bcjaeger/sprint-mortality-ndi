#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ndi_baseline
ndi_tabulate_characteristics <- function(ndi_baseline, recoders) {


  ndi_tbl_data <- ndi_baseline |>
    select(ehr_ancillary,
           age_yrs,
           treatment,
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

  tbl_by_anci <- ndi_tbl_data |>
    relocate(sbp, treatment, n_agents, .after = race) |>
    tbl_summary(
      by = 'ehr_ancillary',
      statistic = list(
        all_continuous() ~ c("{mean} ({sd})"),
        all_categorical() ~ "{p}"
      ),
      digits = list(everything() ~ 1),
      label = list(age_cat ~ paste("Age", recoders$levels$age_cat[2]),
                   sex ~ recoders$levels$sex[2],
                   sbp ~ recoders$variable['sbp'],
                   treatment ~ "Intensive treatment",
                   race ~ recoders$levels$race[2],
                   moca ~ paste("MoCA", recoders$levels$moca[2])),
      type = list(age_yrs ~ "continuous",
                  age_cat ~ "dichotomous",
                  sbp ~ 'continuous',
                  n_agents ~ 'continuous',
                  treatment ~ "dichotomous",
                  sex ~ "dichotomous",
                  race ~ "dichotomous",
                  ckd_2021 ~ "dichotomous",
                  moca ~ "dichotomous",
                  frail ~ "categorical"),
      value = list(age_cat = recoders$levels$age_cat[2],
                   treatment = 'Intensive',
                   sex = recoders$levels$sex[2],
                   race = recoders$levels$race[2],
                   ckd_2021 = recoders$levels$ckd_2021[2],
                   moca = recoders$levels$moca[2]),
      missing = 'no'
    ) |>
    add_p()

  tbl_by_tx <- ndi_tbl_data |>
    relocate(sbp, treatment, n_agents, ehr_ancillary, .after = race) |>
    tbl_summary(
      by = 'treatment',
      statistic = list(
        all_continuous() ~ c("{mean} ({sd})"),
        all_categorical() ~ "{p}"
      ),
      digits = list(everything() ~ 1),
      label = list(age_cat ~ paste("Age", recoders$levels$age_cat[2]),
                   sex ~ recoders$levels$sex[2],
                   sbp ~ recoders$variable['sbp'],
                   ehr_ancillary ~ "Included in EHR ancillary study",
                   race ~ recoders$levels$race[2],
                   moca ~ paste("MoCA", recoders$levels$moca[2])),
      type = list(age_yrs ~ "continuous",
                  age_cat ~ "dichotomous",
                  ehr_ancillary ~ "dichotomous",
                  sbp ~ 'continuous',
                  n_agents ~ 'continuous',
                  sex ~ "dichotomous",
                  race ~ "dichotomous",
                  ckd_2021 ~ "dichotomous",
                  moca ~ "dichotomous",
                  frail ~ "categorical"),
      value = list(age_cat = recoders$levels$age_cat[2],
                   ehr_ancillary = 'Yes',
                   sex = recoders$levels$sex[2],
                   race = recoders$levels$race[2],
                   ckd_2021 = recoders$levels$ckd_2021[2],
                   moca = recoders$levels$moca[2]),
      missing = 'no'
    )

  tbl_overall <- ndi_tbl_data |>
    relocate(sbp, treatment, ehr_ancillary, .after = race) |>
    tbl_summary(
      by = NULL,
      statistic = list(
        all_continuous() ~ c("{mean} ({sd})"),
        all_categorical() ~ "{p}"
      ),
      digits = list(everything() ~ 1),
      label = list(age_cat ~ paste("Age", recoders$levels$age_cat[2]),
                   sex ~ recoders$levels$sex[2],
                   sbp ~ recoders$variable['sbp'],
                   treatment ~ "Intensive treatment",
                   ehr_ancillary ~ "Included in EHR ancillary study",
                   race ~ recoders$levels$race[2],
                   moca ~ paste("MoCA", recoders$levels$moca[2])),
      type = list(age_yrs ~ "continuous",
                  age_cat ~ "dichotomous",
                  sbp ~ 'continuous',
                  n_agents ~ 'continuous',
                  treatment ~ "dichotomous",
                  ehr_ancillary ~ "dichotomous",
                  sex ~ "dichotomous",
                  race ~ "dichotomous",
                  ckd_2021 ~ "dichotomous",
                  moca ~ "dichotomous",
                  frail ~ "categorical"),
      value = list(age_cat = recoders$levels$age_cat[2],
                   treatment = 'Intensive',
                   ehr_ancillary = 'Yes',
                   sex = recoders$levels$sex[2],
                   race = recoders$levels$race[2],
                   ckd_2021 = recoders$levels$ckd_2021[2],
                   moca = recoders$levels$moca[2]),
      missing = 'no'
    )

  tbl_merge(
    list(tbl_overall, tbl_by_tx, tbl_by_anci),
    tab_spanner = c('Overall',
                    'Treatment',
                    'Included in EHR ancillary study')
  )


}
