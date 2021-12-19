#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param model
#' @param k_mat
cph_estimate <- function(model, k_mat) {

  model |>
    glht(linfct = k_mat) |>
    confint() |>
    getElement('confint') |>
    as_tibble() |>
    mutate(across(everything(), exp)) |>
    rename(est = Estimate)

}
