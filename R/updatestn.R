#' Update list of Oklahoma Mesonet stations
#'
#' The list of Oklahoma Mesonet stations is updated each time \code{okmesonet}
#' is loaded. If this fails (e.g., internet connectivity is temporarily down),
#' use \code{updatestn} to manually update the list.
#'
#' @export
#' @name updatestn
#' @seealso \code{\link{okstations}}
#' 
#' @examples
#' \dontrun{
#' ## Update Oklahoma Mesonet station list
#' okstations <- updatestn()
#' }

updatestn <- function() {
  ## Update okstations by calling downloadstn()
  ##
  ## Arguments: none
  ## Returns: updated okstations objects
  downloadstn()
}