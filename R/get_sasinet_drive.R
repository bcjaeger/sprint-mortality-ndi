#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

get_sasinet_drive <- function(username = NULL) {

  if(is.null(username))
    username <- getElement(gitcreds::gitcreds_get(), 'username')

  switch(
    username,
    'bcjaeger' = 'T:/sprint',
    'nmpieyeskey' = 'O:/sprint'
  )

}
