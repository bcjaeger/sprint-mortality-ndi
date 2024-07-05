#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ndi_data_list
ndi_base_describe <- function(ndi_data_list) {

  map_dfr(
    .x = ndi_data_list,
    .id = 'group',
    .f = function(ndi_data) {

      time_by_tx <- ndi_data |>
        group_by(treatment) |>
        group_map(
          .f = ~ prodlim(Hist(acm_years, 1 - acm_event) ~ 1, data = .x) |>
            quantile() |>
            unclass() %>%
            as_tibble() %>%
            filter(q == 0.50) |>
            bind_cols(.y) # .y is the group info
        )

      time_ovrl <-
        prodlim(Hist(acm_years, 1 - acm_event) ~ 1,
                data = ndi_data) |>
        quantile() |>
        unclass() %>%
        as_tibble() %>%
        filter(q == 0.50) |>
        mutate(treatment = 'overall')

      time_by_tx_and_ovrl <-
        time_ovrl |>
        bind_rows(time_by_tx) |>
        select(
          treatment,
          fup_est = quantile,
          fup_lwr = lower,
          fup_upr = upper
        )


      # cvd_by_tx_and_ovrl <-
      #   ndi_data |>
      #   split(f = ndi_data$treatment) |>
      #   list_modify(overall = ndi_data) |>
      #   map_dfr(.f = cph_incidence, 'acm_years', 'cvd_event',
      #           .id = 'treatment') |>
      #   rename(inc_cvd_est = incidence_est,
      #          inc_cvd_lwr = incidence_lwr,
      #          inc_cvd_upr = incidence_upr)
      #
      # acm_by_tx_and_ovrl <-
      #   ndi_data |>
      #   split(f = ndi_data$treatment) |>
      #   list_modify(overall = ndi_data) |>
      #   map_dfr(.f = cph_incidence, 'acm_years', 'acm_event',
      #           .id = 'treatment') |>
      #   rename(inc_acm_est = incidence_est,
      #          inc_acm_lwr = incidence_lwr,
      #          inc_acm_upr = incidence_upr)

      events_by_tx_and_ovrl <-
        ndi_data |>
        split(f = ndi_data$treatment) |>
        list_modify(overall = ndi_data) |>
        map_dfr(.id = 'treatment',
                .f = ~ summarize(.x,
                                 n_obs = n(),
                                 n_acm = sum(acm_event),
                                 n_cvd = sum(cvd_event)))

      reduce(.x = list(time_by_tx_and_ovrl,
                       # cvd_by_tx_and_ovrl,
                       # acm_by_tx_and_ovrl,
                       events_by_tx_and_ovrl),
             .f = left_join,
             by = 'treatment')


    }

  )

}
