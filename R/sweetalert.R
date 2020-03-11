#
# #' @title Load sweetAlert libs in a shiny apps
# #'
# #' @description
# #' Load sweetAlert libs in a shiny apps.
# #'
# #' @importFrom htmltools htmlDependency attachDependencies
# #'
# #' @export
#
# useSweetAlert <- function() {
#   tagSweet <- tags$div()
#   dep <- htmltools::htmlDependency(
#     "sweetAlert", "0.1.0", c(href="shinyWidgets"),
#     script = "sweetAlert/js/sweetalert.min.js",
#     stylesheet = "sweetAlert/css/sweetalert.css"
#   )
#   htmltools::attachDependencies(tagSweet, dep)
# }

#' @title Load Sweet Alert dependencies
#'
#' @description
#' This function isn't necessary for \code{sendSweetAlert}, \code{confirmSweetAlert},
#'  \code{inputSweetAlert} (except if you want to use a theme other than the default one),
#'  but is still needed for \code{progressSweetAlert}.
#'
#' @param theme Theme to modify alerts appearance.
#'
#' @seealso \code{\link{sendSweetAlert}}, \code{\link{confirmSweetAlert}}, \code{\link{inputSweetAlert}}.
#'
#' @importFrom htmltools attachDependencies htmlDependency singleton tagList tags
#'
#' @export
#'
#' @example examples/useSweetAlert.R
useSweetAlert <- function(theme = c("sweetalert2", "minimal",
                                    "dark", "bootstrap-4",
                                    "borderless")) {
  theme <- match.arg(theme)
  tag_sa <- singleton(
    tagList(
      tags$span(id = "sw-sa-deps")
    )
  )
  attachDependencies(tag_sa, htmlDependency(
    name = "sweetalert2",
    version = "8.17.6",
    src = c(href="shinyWidgets/sweetalert2"),
    script = c("js/sweetalert2.min.js", "sweetalert-bindings.js"),
    stylesheet = sprintf("css/%s.min.css", theme)
  ))
}

