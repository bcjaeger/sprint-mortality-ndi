##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##'
##' @param time
##' @param data 
##' @param status
cph_incidence <- function(data, time, status, div_time_by = 1000) {
  
  total_events <- sum(getElement(data, status))
  total_time <- sum(getElement(data, time))
  
  total_time <- total_time / div_time_by
  
  estimate = total_events / total_time
  
  fit <- glm(total_events ~ offset(log(total_time)), family="poisson")
  
  fit_ci <- exp(confint(fit))
  
  list(incidence_est = estimate,
       incidence_lwr = fit_ci['2.5 %'],
       incidence_upr = fit_ci['97.5 %'])
  
}
