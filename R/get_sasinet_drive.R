#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

get_sasinet_drive <- function() {

  username <- getElement(gitcreds::gitcreds_get(), 'username')

  switch(
    username,
    'bcjaeger' = 'Z',
    'nmpieyeskey' = 'O'
  )

}
