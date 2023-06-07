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
  # check if x_order is not NULL
  if (!is.null(x_order)) {
    # check if length of x_order is one
    if (length(x_order) == 1) {
      # group the data by x and calculate the mean of y for each group
      # sort the groups in descending or ascending order based on x_order value
      # extract the levels of each group into x_order
      x_order <- data %>%
        group_by({{ x }}) %>%
        summarise(mean = mean({{ y }})) %>%
        arrange(case_when(
          x_order == "desc" ~ desc(mean),
          x_order == "asc" ~ mean
        )) %>%
        pull({{ x }})
    }

    # get the variable name of x and convert it into a quosure
    x <- enquo(x)

    # convert the x variable of data into a factor with levels defined by
    # x_order
    data[[quo_name(x)]] <- factor(data[[quo_name(x)]], levels = x_order)
  }

  # check if y_order is not NULL
  if (!is.null(y_order)) {
    # check if length of y_order is one
    if (length(y_order) == 1) {
      # group the data by y and calculate the mean of x for each group
      # sort the groups in descending or ascending order based on y_order value
      # extract the levels of each group into y_order
      y_order <- data %>%
        group_by({{ y }}) %>%
        summarise(mean = mean({{ x }})) %>%
        arrange(case_when(
          y_order == "desc" ~ desc(mean),
          y_order == "asc" ~ mean
        )) %>%
        pull({{ y }})
    }

    # get the variable name of y and convert it into a quosure
    y <- enquo(y)

    # convert the y variable of data into a factor with levels defined by
    # y_order
    data[[quo_name(y)]] <- factor(data[[quo_name(y)]], levels = y_order)
  }

  # return the modified data with factor variables
  return(data)
}
