#' Custom theme for catplot
#'
#' @param font_size Numeric, font size (default: 8)
#' @param linewidth Numeric, line width (default: 0.5)
#' @param aspect_ratio Numeric, aspect ratio of the plot (default: NULL)
#' @param frame Character, frame type, "closed" or "open" (default: "closed")
#' @param show_panel_grid Character, panel grid visibility (default: "both_not")
#' @param show_title Character, axis title visibility (default: "both")
#' @param show_text Character, axis text visibility (default: "both")
#' @param text_italic Character, axis text italicization (default: "both_not")
#' @param show_ticks Character, axis tick visibility (default: "both")
#' @param ticks_length Numeric, tick length (default: 4)
#' @param legend_position Character, position of legend (default: "right")
#' @param legend_direction Character, direction of legend (default: NULL)
#' @return A ggplot2 theme
#' @export
#' @examples
#' library(catplot)
#' library(ggplot2)
#'
#' p <- ggplot(mpg, aes(x = manufacturer, y = hwy)) +
#'   geom_point()
#' p + theme_cat()
theme_cat <- function(font_size = 8,
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
                      legend_direction = NULL) {
  linewidth <- linewidth * 0.5 / 1.07

  arg_list <- list(
    frame = c("closed", "open"),
    show_panel_grid = c("x", "y", "both", "both_not"),
    show_title = c("x", "y", "both", "both_not"),
    show_ticks = c("x", "y", "both", "both_not"),
    show_text = c("x", "y", "both", "both_not"),
    text_italic = c("x", "y", "both", "both_not"),
    legend_position = c("right", "left", "bottom", "top")
  )

  frame <- match.arg(frame, arg_list$frame)
  show_panel_grid <-
    match.arg(show_panel_grid, arg_list$show_panel_grid)
  show_title <- match.arg(show_title, arg_list$show_title)
  show_ticks <- match.arg(show_ticks, arg_list$show_ticks)
  show_text <- match.arg(show_text, arg_list$show_text)
  text_italic <- match.arg(text_italic, arg_list$text_italic)
  legend_position <-
    match.arg(legend_position, arg_list$legend_position)

  legend_margin <- switch(legend_position,
    "right" = margin(l = -8),
    "top" = margin(b = -8),
    "bottom" = margin(t = -8),
    "left" = margin(r = -8)
  )
  legend_direction <- legend_direction %||% "default"
  legend_direction <- switch(legend_position,
    "h" = "horizontal",
    "horizontal" = "horizontal",
    "v" = "vertical",
    "vertical" = "vertical",
    "default" = "NULL"
  )

  tick_element <-
    element_line(
      linewidth = linewidth,
      lineend = "square",
      colour = "black"
    )
  title_element <-
    element_text(
      size = font_size,
      face = "plain",
      colour = "black"
    )
  grid_element <- element_line(
    colour = "lightgrey",
    linewidth = linewidth,
    lineend = "square"
  )
  theme_base <- theme(
    aspect.ratio = aspect_ratio,
    axis.ticks.length = unit(ticks_length, "pt"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.background = element_blank(),
    legend.title = title_element,
    legend.text = title_element,
    legend.key = element_blank(),
    legend.key.height = unit(font_size, "pt"),
    legend.key.width = unit(font_size, "pt"),
    legend.position = legend_position,
    legend.margin = legend_margin,
    legend.direction = legend_direction,
    plot.background = element_blank(),
    plot.title = element_text(
      size = font_size,
      hjust = 0.5,
      vjust = -1,
      face = "plain",
      colour = "black"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = font_size,
      face = "plain",
      colour = "black"
    ),
    strip.background = element_blank(),
    strip.text = title_element
  )

  # Frame
  frame_theme <- switch(frame,
    "open" = theme(
      axis.line = tick_element,
      panel.border = element_blank()
    ),
    "closed" = theme(
      axis.line = element_blank(),
      panel.border = element_rect(
        linewidth = linewidth,
        fill = NA,
        colour = "black"
      )
    )
  )
  # Title
  title_theme <- switch(show_title,
    "x" = theme(
      axis.title.x = title_element,
      axis.title.y = element_blank()
    ),
    "y" = theme(
      axis.title.y = title_element,
      axis.title.x = element_blank()
    ),
    "both" = theme(axis.title = title_element),
    "both_not" = theme(axis.title = element_blank())
  )
  # Ticks
  ticks_theme <- switch(show_ticks,
    "x" = theme(
      axis.ticks.x = tick_element,
      axis.ticks.y = element_blank()
    ),
    "y" = theme(
      axis.ticks.y = tick_element,
      axis.ticks.x = element_blank()
    ),
    "both" = theme(axis.ticks = tick_element),
    "both_not" = theme(axis.ticks = element_blank())
  )
  # Fontface
  x_fontface <- ifelse(text_italic == "x" |
    text_italic == "both", "italic", "plain")
  y_fontface <- ifelse(text_italic == "y" |
    text_italic == "both", "italic", "plain")
  text_theme <- switch(show_text,
    "x" = theme(
      axis.text.x = element_text(
        size = font_size,
        face = x_fontface,
        colour = "black"
      ),
      axis.text.y = element_blank()
    ),
    "y" = theme(
      axis.text.y = element_text(
        size = font_size,
        face = y_fontface,
        colour = "black"
      ),
      axis.text.x = element_blank()
    ),
    "both" = theme(
      axis.text.x = element_text(
        size = font_size,
        face = x_fontface,
        colour = "black"
      ),
      axis.text.y = element_text(
        size = font_size,
        face = x_fontface,
        colour = "black"
      )
    ),
    "both_not" = theme(axis.text = element_blank())
  )

  # Panel grid
  grid_theme <- switch(show_panel_grid,
    "x" = theme(
      panel.grid.major.x = grid_element,
      panel.grid.major.y = element_blank()
    ),
    "y" = theme(
      panel.grid.major.y = grid_element,
      panel.grid.major.x = element_blank()
    ),
    "both" = theme(panel.grid = grid_element),
    "both_not" = theme(panel.grid = element_blank())
  )
  theme_modifications <- list(
    theme_base,
    frame_theme,
    title_theme,
    ticks_theme,
    text_theme,
    grid_theme
  )
  theme_modifications <- Reduce("+", theme_modifications)
  return(theme_modifications)
}

#' Modify a ggplot2 plot with custom theme, title, axis labels, and ggsignif
#' comparisons
#'
#' This function modifies a ggplot2 plot by adding a custom theme, plot title,
#' axis labels, and ggsignif comparisons. The function returns a list of ggplot2
#' objects that can be used to modify the plot.
#'
#' @inheritParams theme_cat
#' @param plot_title A character string representing the title of the plot
#' @param x_axis_title A character string representing the title of the x-axis
#' @param y_axis_title A character string representing the title of the y-axis
#' @param x_text_angle An integer representing the angle of the x-axis text
#'   labels
#' @param y_text_angle An integer representing the angle of the y-axis text
#'   labels
#' @param legend_title A character string representing the title of the plot
#'   legend
#' @param comparisons A list of comparison groups for ggsignif
#' @param test A character string representing the statistical test to use for
#'   ggsignif
#' @param step_increase A numeric value representing the step increase for
#'   ggsignif
#' @param map_signif_level A logical value representing whether to map the
#'   significance level for ggsignif
#' @return A list of ggplot2 objects that can be used to modify the plot
#' @export
#' @examples
#' library(ggplot2)
#' library(catplot)
#'
#' # Create a plot
#' data(mtcars)
#' p <-
#'   ggplot(mtcars, aes(
#'     x = factor(cyl),
#'     y = mpg,
#'     fill = factor(gear)
#'   )) +
#'   geom_boxplot() +
#'   labs(
#'     title = "Miles per Gallon by Cylinder and Gear",
#'     x = "Cylinder",
#'     y = "Miles per Gallon",
#'     fill = "Gear"
#'   )
#'
#' # Modify the plot with custom theme, title, and axis labels
#' p + cat_modify_plot(
#'   plot_title = "Miles per Gallon by Cylinder and Gear",
#'   x_axis_title = "Cylinder",
#'   y_axis_title = "Miles per Gallon"
#' )
#'
#' # Modify the plot with custom ggsignif comparisons
#' p + cat_modify_plot(
#'   comparisons = list(c("4", "6"), c("4", "8"), c("6", "8")),
#'   test = "t.test"
#' )
#'
#' # Modify the plot with custom theme and ggsignif comparisons
#' p + cat_modify_plot(
#'   font_size = 12,
#'   linewidth = 1,
#'   comparisons = list(c("4", "6"), c("4", "8"), c("6", "8")),
#'   test = "t.test"
#' )
cat_modify_plot <- function(plot_title = waiver(),
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
  list(
    # Plot title and axis title
    labs(title = plot_title, x = x_axis_title, y = y_axis_title),
    # Plot legend
    guides(
      x = guide_axis(angle = x_text_angle),
      y = guide_axis(angle = y_text_angle),
      fill = guide_legend(title = legend_title)
    ),
    # Plot ggsignif
    if (!is.null(comparisons)) {
      geom_signif(
        comparisons = comparisons,
        size = 0.5 * 0.5 * 0.93,
        textsize = 0.36 * 6,
        test = test,
        step_increase = step_increase,
        tip_length = 0.02,
        map_signif_level = map_signif_level
      )
    },
    theme_cat(
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
      legend_direction = legend_direction
    )
  )
}
