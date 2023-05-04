#' @importFrom ggplot2 element_blank element_line element_rect element_text
#'   margin unit aes as_label enquo geom_boxplot ggplot guide_axis guide_legend
#'   guides labs scale_x_continuous scale_y_continuous quo_name geom_jitter
#'   position_jitter position_jitterdodge theme expansion waiver
#' @importFrom ggsignif geom_signif
#' @importFrom dplyr group_by summarise arrange desc pull case_when mutate
NULL

#' @importFrom magrittr %>%
##' @export
magrittr::`%>%`

#' Set a default value if an object is null
#'
#' @param lhs An object to set if it's null
#' @param rhs The value to provide if x is null
#'
#' @return rhs if lhs is null, else lhs
#'
#' @name set-if-null
#'
#' @author Hadley Wickham
#' @references \url{https://adv-r.hadley.nz/functions.html#missing-arguments}
#'
#' @examples
#' \dontrun{
#' 4 %||% 5
#' NULL %||% 5
#' }
#'
#' @keywords internal
#'
`%||%` <- function(lhs, rhs) {
  if (!is.null(x = lhs)) {
    return(lhs)
  } else {
    return(rhs)
  }
}


#' Check if a variable is a character or factor
#'
#' This function checks if a variable is a character or factor.
#'
#' @param x A variable to be checked.
#'
#' @return A logical value indicating whether the variable is a character or factor.
#'
#' @examples
#' library(catplot)
#' is_char_or_factor("hello")
#' is_char_or_factor(factor("world"))
#'
#' @export
is_char_or_factor <- function(x) {
  is.character(x) || is.factor(x)
}

#' Check if a string is a valid color
#'
#' This function attempts to convert a string to an RGB value using the
#' \code{col2rgb()} function from the \code{grDevices} package. If the RGB value
#' is not empty, the string is considered a valid color.
#'
#' @param str A character string representing a color
#' @return A logical value indicating whether the input is a valid color
#' @examples
#' library(catplot)
#' is_color("red") # TRUE
#' is_color("#00FF00") # TRUE
#' is_color("not a color") # FALSE
#' @export
is_color <- function(str) {
  # Attempt to convert the string to an RGB value
  rgb <- tryCatch(grDevices::col2rgb(str), error = function(e) NULL)

  # If the RGB value is not empty, the string is a valid color
  !is.null(rgb)
}
