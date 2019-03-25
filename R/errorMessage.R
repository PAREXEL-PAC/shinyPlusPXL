#' Produces a sweetAlert-powered customized error message
#' @export
#'
#' @param session The current shiny session object
#' @param text Body text for the pop-up message
#' @param title Title text for the pop-up message
#' @return None
#' @examples
#' \dontrun{errorMessage(session)}
#'
#' @author Andrea Berardi \email{Andrea.Berardi@@PAREXEL.com}
errorMessage = function(session, text = "Incorrect input entered",
                        title = "Input error") {
  shinyWidgets::sendSweetAlert(
    session = session,
    title = title,
    text = text,
    type = "error"
  )
}
