#' Extends \code{shiny::validate} to produce an informative pop-up message and applies a CSS class if invalid
#' @export
#'
#' @param session The shiny client session.
#' @param inputId The input id to be validated.
#' @param ... function calls to \code{\link[shiny]{need}}.
#' @param errorTitle Optional error title string to be passed to \code{link{errorMessage}}.
#' @param invalidCSS CSS class to be applied if invalid, or removed if valid.
#' @param raiseModal Logical, raise a modal \code{sweetAlert} if the input is invalid.
#' @return Invisible TRUE if executes correctly, can be used for further validation or resets.
#' @examples
#' \dontrun{validateThenCSS(session,
#' "inputId", need(!is.na(x), "x is NA"))}
#'
#' @seealso \code{\link{validateThen}}, \code{\link{errorMessage}},
#' \code{\link[shiny]{validate}}, \code{\link[shiny]{need}}
#'
#' @author Andrea Berardi \email{Andrea.Berardi@PAREXEL.com}
validateThenCSS = function(session, inputId, ..., errorTitle = NULL, invalidCSS = "invalidInput", raiseModal = TRUE) {
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
  if (length(results) == 0) {
    shinyjs::removeCssClass(id = inputId, class = invalidCSS)
    return(invisible(TRUE))
  }
  results = results[nzchar(results)]
  shinyjs::addCssClass(id = inputId, class = invalidCSS)
  if (raiseModal) errorMessage(session, text = paste(results, collapse = ", "), title = errorTitle)
  shiny:::reactiveStop(paste(results, collapse = "\\n"), "Validation")
}
