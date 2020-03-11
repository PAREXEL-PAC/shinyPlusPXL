#' Extends \code{shiny::validate} to produce an informative pop-up message
#' @export
#'
#' @param session The shiny client session.
#' @param ... function calls to \code{\link[shiny]{validate}}.
#' @param errorTitle Optional error title string to be passed to \code{link{errorMessage}}.
#' @param raiseModal Logical, raise a modal \code{sweetAlert} if the input is invalid.
#' @return Invisible TRUE if executes correctly, can be used for further validation or resets.
#' @examples
#' \dontrun{validateThen(session,
#' need(!is.na(x), "x is NA"),
#' errorTitle = "Invalid input", raiseModal = TRUE)}
#'
#' @seealso \code{\link{validateThenCSS}}, \code{\link{errorMessage}},
#' \code{\link[shiny]{validate}}
#'
#' @author Andrea Berardi \email{Andrea.Berardi@@PAREXEL.com}
validateThen = function(session, ..., errorTitle = NULL, raiseModal = TRUE) {
  results = sapply(list(...), function(x) {
    if (is.null(x))
      return(NA_character_)
    else if (identical(x, FALSE))
      return("")
    else if (is.character(x))
      return(paste(as.character(x), collapse = "\\n"))
    else stop("Unexpected validation result: ", as.character(x))
  })
  results = stats::na.omit(results)
  if (length(results) == 0)
    return(invisible(TRUE))
  results = results[nzchar(results)]
  if (raiseModal) errorMessage(session, text = paste(results, collapse = ", "), title = errorTitle)
  shiny:::reactiveStop(paste(results, collapse = "\\n"), "Validation")
}
