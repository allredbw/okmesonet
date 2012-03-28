#' Update list of Oklahoma Mesonet stations
#'
#' The list of Oklahoma Mesonet stations is updated each time \code{okmesonet}
#' is loaded. If this fails (e.g., internet connectivty is temporarily down),
#' use \code{updatestn} to manually update the list.
#'
#' @export
#' @name updatestn
#' @seealso \code{\link{okstations}}

updatestn <- function() {
  ## Update okstations by calling downloadstn()
  ##
  ## Arguments: none
  ## Returns: updated okstations objects
  unlockBinding("okstations",env=as.environment("package:okmesonet"))
  assign("okstations", downloadstn(), inherits=T)
  lockBinding("okstations",env=as.environment("package:okmesonet"))
}