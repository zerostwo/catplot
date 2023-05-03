#' @importFrom ggplot2 element_blank element_line element_rect element_text
#'   margin unit aes as_label enquo geom_boxplot ggplot guide_axis guide_legend
#'   guides labs scale_x_continuous scale_y_continuous
#' @importFrom ggsignif geom_signif
#' @importFrom dplyr mutate
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
