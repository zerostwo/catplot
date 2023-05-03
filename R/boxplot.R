#' Create a boxplot with customized options using ggplot2
#'
#' This function creates a boxplot using ggplot2 with options for modifying the appearance of the plot.
#'
#' @inheritParams cat_modify_plot
#' @param data A data frame containing the data to be plotted.
#' @param x The variable to be plotted on the x-axis.
#' @param y The variable to be plotted on the y-axis.
#' @param fill The variable to be used for filling the boxes. Defaults to NULL.
#' @param width The width of the boxes. Defaults to 0.6.
#' @param fatten The amount by which to fatten the boxes. Defaults to 1.
#' @param flip Logical indicating whether to flip the axes of the plot. Defaults to FALSE.
#' @param show_legend Logical indicating whether to show the legend. Defaults to FALSE.
#'
#' @return A ggplot2 object representing the boxplot with customized options.
#' @export
#'
#' @examples
#' library(catplot)
#' library(ggplot2)
#' library(dplyr)
#'
#' data("iris")
#' iris %>%
#'   mutate(group = if_else(log2(Petal.Length + 1) > 1.5,
#'     "high", "low"
#'   )) %>%
#'   cat_boxplot(
#'     x = Species,
#'     y = log2(Petal.Width + 1),
#'     aspect_ratio = 1,
#'     show_panel_grid = "both",
#'     fill = group,
#'     legend_title = "ee",
#'     legend_position = "top",
#'     x_text_angle = 45
#'   )

#' p <- iris |>
#'   cat_boxplot(
#'     x = Species, y = Sepal.Length,
#'     aspect_ratio = 2, frame = "open",
#'     x_text_angle = 45,
#'     comparisons = list(c("versicolor", "virginica"))
#'   ) + scale_fill_brewer(palette = "Paired")
#' p
cat_boxplot <- function(data,
                        x,
                        y,
                        fill = NULL,
                        width = 0.6,
                        fatten = 1,
                        show_legend = FALSE,
                        aspect_ratio = NULL,
                        flip = FALSE,
                        frame = "closed",
                        font_size = 8,
                        show_panel_grid = NULL,
                        plot_title = NULL,
                        x_axis_title = NULL,
                        y_axis_title = NULL,
                        show_title = "y",
                        show_ticks = "both",
                        show_text = "both",
                        text_italic = NULL,
                        x_text_angle = NULL,
                        y_text_angle = NULL,
                        legend_title = NULL,
                        legend_position = "right",
                        legend_direction = NULL,
                        comparisons = NULL,
                        test = "wilcox.test",
                        step_increase = 0.1,
                        map_signif_level = TRUE) {
  # Data transform
  fill <- enquo(fill)
  if (as_label(fill) == "NULL") {
    fill <- enquo(x)
  } else {
    fill <- enquo(fill)
  }
  if (flip) {
    input_data <- data %>%
      mutate(
        y = {{ x }},
        x = {{ y }},
        fill = {{ fill }}
      )
  } else {
    input_data <- data %>%
      mutate(
        x = {{ x }},
        y = {{ y }},
        fill = {{ fill }}
      )
  }
  # Plot core
  p <- ggplot(data = input_data, mapping = aes(x = x, y = y))
  if (as_label(fill) %in% colnames(input_data)) {
    p <- p + geom_boxplot(
      mapping = aes(fill = fill),
      fatten = fatten,
      width = width,
      color = "black",
      linewidth = 0.5 * 0.5 * 0.93,
      outlier.shape = NA, show.legend = show_legend
    )
  } else {
    fill <- gsub("\"", "", as_label(fill))
    p <- p + geom_boxplot(
      fill = fill,
      fatten = fatten,
      width = width,
      outlier.shape = NA,
      color = "black",
      linewidth = 0.5 * 0.5 * 0.93,
      show.legend = show_legend
    )
  }
  if (flip) {
    p <- p + scale_x_continuous(expand = c(0, 0.3))
    show_title <- "x"
    x_axis_title <- x_axis_title %||% enquo(y)
    y_axis_title <- y_axis_title %||% enquo(x)
  } else {
    p <- p + scale_y_continuous(expand = c(0, 0.3))
    show_title <- "y"
    x_axis_title <- x_axis_title %||% enquo(x)
    y_axis_title <- y_axis_title %||% enquo(y)
  }
  legend_title <- legend_title %||% enquo(fill)
  p <- p + cat_modify_plot(aspect_ratio = aspect_ratio,
    frame = frame,
    font_size = font_size,
    show_panel_grid = show_panel_grid,
    plot_title = plot_title,
    x_axis_title = x_axis_title,
    y_axis_title = y_axis_title,
    show_title = show_title,
    show_ticks = show_ticks,
    show_text = show_text,
    text_italic = text_italic,
    x_text_angle = x_text_angle,
    y_text_angle = y_text_angle,
    legend_title = legend_title,
    legend_position = legend_position,
    legend_direction = legend_direction,
    comparisons = comparisons,
    test = test,
    step_increase = step_increase,
    map_signif_level = map_signif_level
  )
  return(p)
}
