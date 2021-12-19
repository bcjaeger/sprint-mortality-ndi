#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ndi_data
#' @param formula
cph_fit <- function(ndi_data, formula) {

  fit <- do.call(what = survival::coxph,
                 args = list(data = ndi_data, formula = formula))

  pv <- as_tibble(
    getElement(cox.zph(fit), 'table'),
    rownames = 'variable'
  ) |>
    filter(variable == 'GLOBAL') |>
    pull(p)

  message("Non-proportional hazards global p-value = ", table_pvalue(pv))


  return(fit)

}
