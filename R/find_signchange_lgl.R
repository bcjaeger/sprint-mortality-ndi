#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
find_signchange_lgl <- function(x){

  c(0,diff(sign(x))) != 0

}



