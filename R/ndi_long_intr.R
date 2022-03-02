#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ndi_longitudinal
#' @param group
ndi_long_intr <- function(ndi_longitudinal, group) {

  # treatment heterogeneity is assessed in each period separately.

  if(group == 'overall'){
    return(tibble(outcome = c('cvd', 'acm'),
                  period = c('trial', 'cohort'),
                  p.value = NA_real_))
  }

  ndi_split <- ndi_longitudinal |>
    mutate(time = if_else(trial_phase == 'trial',
                          end_yrs,
                          end_yrs - start_yrs),
           .before = start_yrs) |>
    drop_na(all_of(group))

  ndi_split <- ndi_split |>
    split(ndi_split$trial_phase)

  frmla <-
    as.formula(
      glue(
        "Surv(time, mortality) ~
          treatment * {group} +
          strata(randSite)"
      )
    )

  acm <- ndi_split |>
    map(cph_fit, frmla) |>
    map(anova) |>
    map_dfr(tidy, .id = 'period') |>
    filter(term == paste("treatment", group, sep = ':')) |>
    transmute(period, p.value)

  frmla <- as.formula(
    glue(
      "Event(time, cvd_mortality) ~
          const(treatment) * const({group}) +
          cluster(randSite)"
    )
  )

  cvd <- ndi_split |>
    map_dfr(
      ~ {

        fit <- comp.risk(formula = frmla,
                         data = .x,
                         cause = 1,
                         model = 'prop',
                         n.sim = 1000)

        wald_index <- which(str_detect(rownames(coef(fit)), ':'))

        wald_pval <- fit |>
          wald.test(coef.null = wald_index) |>
          getElement('p.value')

        tibble(p.value = wald_pval)

      },
      .id = 'period'
    ) |>
    transmute(period, p.value)

  bind_rows(acm = acm, cvd = cvd, .id = 'outcome')

}
