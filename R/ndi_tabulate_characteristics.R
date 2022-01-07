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
    select(-treatment) |>
    tbl_summary(
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
    add_p()

  tbls_by_tx <- split(ndi_tbl_data, ndi_tbl_data$ehr_ancillary) |>
    map(
      ~ .x |>
        select(-ehr_ancillary) |>
        tbl_summary(
          by = 'treatment',
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
        add_p()
    )

  list(
    main = tbl_by_anci,
    supp = tbl_merge(
      tbls_by_tx,
      tab_spanner = c('Not included in EHR ancillary study',
                      'Included in EHR ancillary study')
    )
  )


}
