#' Extends \code{shiny::navbarPage} to include logos easily
#' @export
#'
#' @param title The title to display in the navbar
#' @param ... \code{\link{tabPanel}} elements to include in the page. The
#'   \code{navbarMenu} function also accepts strings, which will be used as menu
#'   section headers. If the string is a set of dashes like \code{"----"} a
#'   horizontal separator will be displayed in the menu.
#' @param id If provided, you can use \code{input$}\emph{\code{id}} in your
#'   server logic to determine which of the current tabs is active. The value
#'   will correspond to the \code{value} argument that is passed to
#'   \code{\link{tabPanel}}.
#' @param selected The \code{value} (or, if none was supplied, the \code{title})
#'   of the tab that should be selected by default. If \code{NULL}, the first
#'   tab will be selected.
#' @param position Determines whether the navbar should be displayed at the top
#'   of the page with normal scrolling behavior (\code{"static-top"}), pinned at
#'   the top (\code{"fixed-top"}), or pinned at the bottom
#'   (\code{"fixed-bottom"}). Note that using \code{"fixed-top"} or
#'   \code{"fixed-bottom"} will cause the navbar to overlay your body content,
#'   unless you add padding, e.g.: \code{tags$style(type="text/css", "body
#'   {padding-top: 70px;}")}
#' @param header Tag or list of tags to display as a common header above all
#'   tabPanels.
#' @param footer Tag or list of tags to display as a common footer below all
#'   tabPanels
#' @param inverse \code{TRUE} to use a dark background and light text for the
#'   navigation bar
#' @param collapsible \code{TRUE} to automatically collapse the navigation
#'   elements into a menu when the width of the browser is less than 940 pixels
#'   (useful for viewing on smaller touchscreen device)
#' @param collapsable Deprecated; use \code{collapsible} instead.
#' @param fluid \code{TRUE} to use a fluid layout. \code{FALSE} to use a fixed
#'   layout.
#' @param responsive This option is deprecated; it is no longer optional with
#'   Bootstrap 3.
#' @param theme Alternative Bootstrap stylesheet (normally a css file within the
#'   www directory). For example, to use the theme located at
#'   \code{www/bootstrap.css} you would use \code{theme = "bootstrap.css"}.
#' @param windowTitle The title that should be displayed by the browser window.
#'   Useful if \code{title} is not a string.
#' @param logosHTML A div including spans containing the logos to be displayed.
#'   The div should be styled to float right and have the navbar-header and navbar-brand classes.
#'   A valid logosHTML object is shown in the examples.
#'
#' @return Invisible TRUE if executes correctly, can be used for further validation or resets.
#' @examples
#' \dontrun{
#' logosHTML = div(
#'   class = "navbar-header navbar-brand",
#'   style = "float:right",
#'   span("", style = "display: block; overflow: auto; position: relative;"),
#'   span(img(
#'     src = "pathtofile.png",
#'     style = "height: 1em; vertical-align:center; margin-left: 10px;"
#'   ))
#' )
#' }
#' @seealso \code{\link[shiny]{navbarPage}}
#'
#' @author Andrea Berardi \email{Andrea.Berardi@@PAREXEL.com}
navbarPageLogo = function(title = NULL,
                          ...,
                          id = NULL,
                          selected = NULL,
                          position = c("static-top", "fixed-top", "fixed-bottom"),
                          header = NULL,
                          footer = NULL,
                          inverse = FALSE,
                          collapsible = FALSE,
                          collapsable,
                          fluid = TRUE,
                          responsive = NULL,
                          theme = NULL,
                          windowTitle = title,
                          logosHTML = NULL) {
  if (!missing(collapsable)) {
    shiny:::shinyDeprecated("`collapsable` is deprecated; use `collapsible` instead.")
    collapsible <- collapsable
  }
  pageTitle <- title
  navbarClass <- "navbar navbar-default"
  position <- match.arg(position)
  if (!is.null(position))
    navbarClass <- paste(navbarClass, " navbar-", position,
                         sep = "")
  if (inverse)
    navbarClass <- paste(navbarClass, "navbar-inverse")
  if (!is.null(id))
    selected <- shiny::restoreInput(id = id, default = selected)
  tabs <- list(...)
  tabset <- shiny:::buildTabset(tabs, "nav navbar-nav", NULL, id, selected)
  className <- function(name) {
    if (fluid)
      paste(name, "-fluid", sep = "")
    else
      name
  }
  if (collapsible) {
    navId <-
      paste("navbar-collapse-", shiny:::p_randomInt(1000, 10000), sep = "")
    pageTitleSpan = NULL
    if (!is.null(pageTitle))
      pageTitleSpan = shiny::span(class = "navbar-brand", pageTitle)
    containerDiv <-
      shiny::div(
        class = className("container"),
        shiny::div(
          class = "navbar-header",
          shiny::tags$button(
            type = "button",
            class = "navbar-toggle collapsed",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", navId),
            shiny::span(class = "sr-only", "Toggle navigation"),
            shiny::span(class = "icon-bar"),
            shiny::span(class = "icon-bar"),
            shiny::span(class = "icon-bar")
          ),
          pageTitleSpan
        ),
        shiny::div(
          class = "navbar-collapse collapse",
          id = navId,
          tabset$navList,
          logosHTML
        )
      )
  }
  else {
    pageTitleDiv = NULL
    if (!is.null(pageTitle))
      pageTitleDiv = shiny::div(
        class = "navbar-header", shiny::span(class = "navbar-brand", pageTitle))
    containerDiv <-
      shiny::div(class = className("container"),
          pageTitleDiv,
          tabset$navList,
          logosHTML)
  }
  contentDiv <- shiny::div(class = className("container"))
  if (!is.null(header))
    contentDiv <- shiny::tagAppendChild(contentDiv, shiny::div(class = "row", header))
  contentDiv <- shiny::tagAppendChild(contentDiv, tabset$content)
  if (!is.null(footer))
    contentDiv <- shiny::tagAppendChild(contentDiv, shiny::div(class = "row", footer))
  shiny::bootstrapPage(
    title = windowTitle,
    responsive = responsive,
    theme = theme,
    shiny::tags$nav(class = navbarClass, role = "navigation", containerDiv),
    contentDiv
  )
}
