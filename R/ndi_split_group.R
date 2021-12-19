#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ndi_data
#' @param group
ndi_split_group <- function(ndi_data, group) {

  if(group == 'overall') return(list(overall = ndi_data))

  split(ndi_data, f = ndi_data[[group]])

  # long_dat <- 'trial_phase' %in% names(ndi_data)
  #
  # if(group == 'overall' && !long_dat) return(list(overall = ndi_data))
  #
  # fctr <- vector(mode = 'list', length = 2)
  # fctr_index <- 1
  #
  # if(long_dat){
  #   fctr[[fctr_index]] <- ndi_data$trial_phase_fctr
  #   fctr_index <- fctr_index + 1
  # }
  #
  # if(group != 'overall'){
  #   fctr[[fctr_index]] <- ndi_data[[group]]
  #   fctr_index <- fctr_index + 1
  # }
  #
  # split(ndi_data, f = fctr[seq(fctr_index-1, 1)], sep = '')

}
