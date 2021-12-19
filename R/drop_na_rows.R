#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

drop_na_rows <- function(x){
  keep_rows <- apply(x, 1, function(.x) !all(is.na(.x)))
  return(x[keep_rows, ])
}
