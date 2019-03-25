#' Extend \code{shinyWidgets::actionBttn}
#'
#' @description Like \code{actionBttn} and even awesomer
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label The contents of the button, usually a text label.
#' @param icon An optional icon to appear on the button.
#' @param style Style of the button, to choose between \code{simple}, \code{bordered},
#' \code{minimal}, \code{stretch}, \code{jelly}, \code{gradient}, \code{fill},
#' \code{material-circle}, \code{material-flat}, \code{pill}, \code{float}, \code{unite}.
#' @param color Color of the button : \code{default}, \code{primary}, \code{warning},
#'  \code{danger}, \code{success}, \code{royal}.
#' @param size Size of the button : \code{xs},\code{sm}, \code{md}, \code{lg}.
#' @param block Logical, full width button.
#' @param no_outline Logical, don't show outline when navigating with
#'  keyboard/interact using mouse or touch.
#' @param classPlus String; pass additional classes to the button tag
#' @param stylePlus String; pass additional styles to the button tag
#' @param ... Pass additional arguments to the button tag
#'
#' @export
#'
#' @seealso \link[shinyWidgets]{downloadBttn}
#'
#' @importFrom shiny restoreInput
#' @importFrom shiny tags
#'
#' @author Andrea Berardi \email{Andrea.Berardi@@PAREXEL.com}

actionBttnPlus = function(inputId,
                          label = NULL,
                          icon = NULL,
                          style = "unite",
                          color = "default",
                          size = "md",
                          block = FALSE,
                          no_outline = TRUE,
                          classPlus = "",
                          stylePlus = "",
                          ...)
{
  value <- shiny::restoreInput(id = inputId, default = NULL)
  style <- match.arg(
    arg = style,
    choices = c(
      "simple",
      "bordered",
      "minimal",
      "stretch",
      "jelly",
      "gradient",
      "fill",
      "material-circle",
      "material-flat",
      "pill",
      "float",
      "unite"
    )
  )
  color <- match.arg(
    arg = color,
    choices = c("default", "primary",
                "warning", "danger", "success", "royal")
  )
  size <- match.arg(arg = size, choices = c("xs", "sm", "md", "lg"))
  tagBttn <- shiny::tags$button(
    id = inputId,
    type = "button",
    class = classPlus,
    style = stylePlus,
    class = "action-button bttn",
    `data-val` = value,
    class = paste0("bttn-", style),
    class = paste0("bttn-", size),
    class = paste0("bttn-", color),
    list(icon, label),
    class = if (block)
      "bttn-block",
    class = if (no_outline)
      "bttn-no-outline",
    ...
  )
  shinyWidgets:::attachShinyWidgetsDep(tagBttn, "bttn")
}
