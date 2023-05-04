#' Modify factor levels of data
#'
#' This function is used to adjust the factor levels of data based on the
#' x_order and y_order inputs.
#'
#' @param data A data frame to be modified.
#' @param x A column name of data frame to be adjusted factor levels.
#' @param y A column name of data frame to be adjusted factor levels.
#' @param x_order Optional. A character vector specifying how to adjust the
#'   factor levels of the x column. It can be one of the following: "desc"
#'   (arrange in descending order according to the mean value), "asc" (arrange
#'   in ascending order according to the mean value), a vector of factor levels
#'   in the desired order, or NULL (do not change).
#' @param y_order Optional. A character vector specifying how to adjust the
#'   factor levels of the y column. It can be one of the following: "desc"
#'   (arrange in descending order according to the mean value), "asc" (arrange
#'   in ascending order according to the mean value), a vector of factor levels
#'   in the desired order, or NULL (do not change).
#'
#' @return A modified data frame with adjusted factor levels.
#'
#' @examples
#' library(catplot)
#' data(iris)
#' head(cat_modify_data(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   x_order =
#'     c("virginica", "versicolor", "setosa")
#' ))
#' head(cat_modify_data(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   x_order = "desc"
#' ))
#'
#' @export
cat_modify_data <- function(data, x, y, x_order = NULL, y_order = NULL) {
  if (!is.null(x_order)) {
    if (length(x_order) == 1) {
      x_order <- data %>%
        group_by({{ x }}) %>%
        summarise(mean = mean({{ y }})) %>%
        arrange(case_when(
          x_order == "desc" ~ desc(mean),
          x_order == "asc" ~ mean
        )) %>%
        pull({{ x }})
    }
    x <- enquo(x)
    data[[quo_name(x)]] <- factor(data[[quo_name(x)]], levels = x_order)
  }

  if (!is.null(y_order)) {
    if (length(y_order) == 1) {
      y_order <- data %>%
        group_by({{ y }}) %>%
        summarise(mean = mean({{ x }})) %>%
        arrange(case_when(
          y_order == "desc" ~ desc(mean),
          y_order == "asc" ~ mean
        )) %>%
        pull({{ y }})
    }
    y <- enquo(y)
    data[[quo_name(y)]] <- factor(data[[quo_name(y)]], levels = y_order)
  }

  return(data)
}
