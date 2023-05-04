#' Create a box plot with customizable color or fill
#'
#' This function creates a box plot using the \code{geom_boxplot()} function
#' from the \code{ggplot2} package. The color or fill aesthetic can be
#' customized using the \code{color} or \code{fill} parameter, respectively.
#'
#' @param color A character string representing the color to use for the box
#'   plot outline
#' @param fill A character string representing the color to use for the box plot
#'   fill
#' @param ... Additional arguments to be passed to
#'   \code{\link[ggplot2]{geom_boxplot}}
#' @return A ggplot object with a box plot
#' @examples
#' library(catplot)
#' library(ggplot2)
#' # Create a box plot with red outline
#' geom_boxplot_internal(color = "red")
#'
#' # Create a box plot with green fill
#' geom_boxplot_internal(fill = "green")
#'
#' # Create a box plot with custom data
#' data <- data.frame(x = rep(c("A", "B"), each = 10), y = rnorm(20))
#' ggplot(data, aes(x, y)) +
#'   geom_boxplot_internal(color = "blue", fill = "yellow")
#' @export
geom_boxplot_internal <- function(color = NULL, fill = NULL, ...) {
  color <- enquo(color) # Capture the 'color' argument
  fill <- enquo(fill) # Capture the 'fill' argument
  if (quo_name(color) != "NULL") {
    # Check if 'color' is provided
    if (is_color(quo_name(color))) {
      # Check if 'color' is a valid color
      geom_boxplot(color = quo_name(color), ...) # Use 'color' directly
    } else {
      geom_boxplot(
        mapping = aes(
          color = !!color
        ), ...
      ) # Map 'color' using 'aes()'
    }
  } else if (quo_name(fill) != "NULL") {
    # Check if 'fill' is provided
    if (is_color(quo_name(fill))) {
      # Check if 'fill' is a valid color
      geom_boxplot(
        fill = quo_name(fill),
        color = "black", ...
      ) # Use 'fill' directly
    } else {
      geom_boxplot(
        mapping = aes(fill = !!fill),
        color = "black",
        ...
      ) # Map 'fill' using 'aes()'
    }
  } else {
    # Throw an error if neither color nor fill is provided
    stop("Please provide either 'color' or 'fill' parameter.")
  }
}

#' Create a boxplot with customized options using ggplot2
#'
#' This function creates a grouped boxplot with optional jitter points. It takes
#' in a data frame, x variable, y variable, fill variable, color variable, and
#' various other arguments to customize the plot.
#'
#' @inheritParams geom_boxplot_internal
#' @inheritParams cat_modify_data
#' @inheritParams cat_modify_plot
#' @param width A numeric value indicating the width of each boxplot. Defaults
#'   to 0.4.
#' @param fatten A numeric value indicating the amount to increase the width of
#'   each boxplot relative to the default. Defaults to 1.
#' @param outlier_colour Deprecated. Use outlier_color instead.
#' @param outlier_color A character string or symbol indicating the color of the
#'   outlier points. Defaults to NULL.
#' @param outlier_fill A character string or symbol indicating the fill color of
#'   the outlier points. Defaults to NULL.
#' @param outlier_shape An integer indicating the shape of the outlier points.
#'   Defaults to 19.
#' @param outlier_size A numeric value indicating the size of the outlier
#'   points. Defaults to 0.5.
#' @param outlier_stroke A numeric value indicating the stroke width of the
#'   outlier points. Defaults to 0.5.
#' @param outlier_alpha A numeric value indicating the alpha transparency of the
#'   outlier points. Defaults to NULL.
#' @param notch A logical value indicating whether to display notches around the
#'   median. Defaults to FALSE.
#' @param notchwidth A numeric value indicating the width of the notches.
#'   Defaults to 0.5.
#' @param varwidth A logical value indicating whether to adjust the width of
#'   each boxplot based on the number of observations. Defaults to FALSE.
#' @param na_rm A logical value indicating whether to remove missing values.
#'   Defaults to FALSE.
#' @param orientation A character string indicating whether the plot should be
#'   horizontal ("horizontal") or vertical ("vertical"). Defaults to NA.
#' @param show_legend A logical value indicating whether to show the legend.
#'   Defaults to FALSE.
#' @param add_jitter A logical value indicating whether to add jitter points to
#'   the plot. Defaults to FALSE.
#' @param jitter_width A numeric value indicating the width of the jitter
#'   points. Defaults to 0.6.
#' @param jitter_size A numeric value indicating the size of the jitter points.
#'   Defaults to 0.5.
#' @param jitter_color A character string or symbol indicating the variable to
#'   use for color of the jitter points. Defaults to NULL.
#'
#' @return A ggplot object representing the grouped boxplot with optional jitter
#'   points.
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
#'     fill = group,
#'     aspect_ratio = 1,
#'     show_panel_grid = "both",
#'     legend_title = "ee",
#'     legend_position = "top",
#'     x_text_angle = 45
#'   )
#'
#' p <- iris %>%
#'   cat_boxplot(
#'     x = Species,
#'     y = Sepal.Length,
#'     aspect_ratio = 2,
#'     frame = "open",
#'     x_text_angle = 45,
#'     comparisons = list(c("versicolor", "virginica"))
#'   ) + scale_fill_brewer(palette = "Paired")
#' p
#' @export
cat_boxplot <- function(data,
                        x,
                        y,
                        fill = NULL,
                        color = NULL,
                        x_order = NULL,
                        y_order = NULL,
                        width = 0.4,
                        fatten = 1,
                        outlier_colour = NULL,
                        outlier_color = NULL,
                        outlier_fill = NULL,
                        outlier_shape = 19,
                        outlier_size = 0.5,
                        outlier_stroke = 0.5,
                        outlier_alpha = NULL,
                        notch = FALSE,
                        notchwidth = 0.5,
                        varwidth = FALSE,
                        na_rm = FALSE,
                        orientation = NA,
                        show_legend = FALSE,
                        add_jitter = FALSE,
                        jitter_width = 0.6,
                        jitter_size = 0.5,
                        jitter_color = NULL,
                        plot_title = waiver(),
                        x_axis_title = waiver(),
                        y_axis_title = waiver(),
                        x_text_angle = NULL,
                        y_text_angle = NULL,
                        legend_title = waiver(),
                        font_size = 8,
                        linewidth = 0.5,
                        aspect_ratio = NULL,
                        frame = "closed",
                        show_panel_grid = "both_not",
                        show_title = "both",
                        show_text = "both",
                        text_italic = "both_not",
                        show_ticks = "both",
                        ticks_length = 4,
                        legend_position = "right",
                        legend_direction = NULL,
                        comparisons = NULL,
                        test = "wilcox.test",
                        step_increase = 0.1,
                        map_signif_level = TRUE) {
  # Check if x variable is character or factor
  x_is_char_or_factor <- is_char_or_factor(data[[quo_name(enquo(x))]])
  # Set color variable if provided
  if (quo_name(enquo(color)) != "NULL") {
    color <- enquo(color)
  }
  if (quo_name(enquo(fill)) != "NULL") {
    fill <- enquo(fill)
  }
  # Determine fill variable based on x and color variables
  if (quo_name(enquo(fill)) == "NULL" && quo_name(enquo(color)) == "NULL" &&
    x_is_char_or_factor) {
    fill <- enquo(x)
  } else if (!x_is_char_or_factor) {
    fill <- enquo(y)
  }
  # Set default jitter_color if not provided
  jitter_color <- enquo(jitter_color)
  if (quo_name(jitter_color) == "NULL") {
    jitter_color <- "black"
  }
  # Show legend if necessary
  if (all(
    quo_name(enquo(x)) != quo_name(fill),
    quo_name(enquo(x)) != quo_name(color)
  )) {
    show_legend <- TRUE
  }
  # Modify data based on input arguments
  data <- cat_modify_data(data = data, x = {{ x }}, y = {{ y }}, x_order = x_order, y_order = y_order)
  # Create the boxplot
  p <- ggplot(data = data, mapping = aes(x = {{ x }}, y = {{ y }})) +
    geom_boxplot_internal(
      fill = {{ fill }},
      color = {{ color }},
      width = width,
      linewidth = linewidth * 0.5 * 0.93,
      fatten = fatten,
      outlier.colour = outlier_colour,
      outlier.color = outlier_color,
      outlier.fill = outlier_fill,
      outlier.shape = outlier_shape,
      outlier.size = outlier_size,
      outlier.stroke = outlier_stroke,
      outlier.alpha = outlier_alpha,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      na.rm = na_rm,
      orientation = orientation,
      show.legend = show_legend
    )
  # Modify the plot based on input arguments
  p <- p + cat_modify_plot(
    plot_title = plot_title,
    x_axis_title = x_axis_title,
    y_axis_title = y_axis_title,
    x_text_angle = x_text_angle,
    y_text_angle = y_text_angle,
    legend_title = legend_title,
    font_size = font_size,
    linewidth = linewidth,
    aspect_ratio = aspect_ratio,
    frame = frame,
    show_panel_grid = show_panel_grid,
    show_title = show_title,
    show_text = show_text,
    text_italic = text_italic,
    show_ticks = show_ticks,
    ticks_length = ticks_length,
    legend_position = legend_position,
    legend_direction = legend_direction,
    comparisons = comparisons,
    test = test,
    step_increase = step_increase,
    map_signif_level = map_signif_level
  )
  # Adjust y-axis range if comparisons are provided
  if (!is.null(comparisons)) {
    scale_fn <- if (x_is_char_or_factor) scale_y_continuous else scale_x_continuous
    p <- p + scale_fn(expand = expansion(mult = c(0.05, 0.1)))
  }
  # Add jitter points if required
  if (add_jitter && quo_name(jitter_color) %in% colnames(data)) {
    p <- p + geom_jitter(
      mapping = aes(color = {{ jitter_color }}),
      size = jitter_size,
      position = position_jitterdodge(
        jitter.width = jitter_width / 2,
        jitter.height = 0.1,
        seed = 717
      ),
      show.legend = FALSE
    )
  } else if (add_jitter && !(quo_name(jitter_color) %in% colnames(data))) {
    color <- quo_name(jitter_color)
    p <- p + geom_jitter(
      color = color,
      size = jitter_size,
      position = position_jitter(
        width = jitter_width / 2,
        height = NULL, seed = 717
      ),
      show.legend = FALSE
    )
  }
  # Return the plot
  return(p)
}
