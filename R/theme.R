#' Custom theme for catplot function
#'
#' This is a custom theme for the catplot function in the catplot package. It
#' sets the visual appearance of the plot elements, such as axis, panel, legend,
#' and title, to match the default style of catplot plots.
#'
#' @param aspect_ratio numeric; ratio of x to y axis length
#' @param frame character; indicates whether the plot frame should be "open" or
#'   "closed"
#' @param show_panel_grid character; indicates which panel grid to show, "x",
#'   "y", "both", or "both_not"
#' @param show_title character; indicates which axis title to show, "x", "y",
#'   "both", or "both_not"
#' @param show_ticks character; indicates which axis ticks to show, "x", "y",
#'   "both", or "both_not"
#' @param show_text character; indicates which axis text to show, "x", "y",
#'   "both", or "both_not"
#' @param text_italic character; indicates which axis text to italicize, "x",
#'   "y", "both", or "both_not"
#' @param legend_position character; indicates the position of the legend,
#'   "right", "left", "bottom", or "top"
#' @param legend_direction character; indicates the direction of the legend, "h"
#'   or "horizontal", "v" or "vertical", or "default"
#' @param font_size numeric; base size of text elements in the theme
#' @param base_family character; base font family of text elements in the theme
#' @param base_line_size numeric; base line size of elements in the theme
#' @param base_rect_size numeric; base line size of rect in the theme
#' @param ... additional arguments passed to \code{\link[ggplot2:theme]{theme}}
#'
#' @return a ggplot2 theme object
#' @export
#'
#' @examples
#' library(catplot)
#' library(ggplot2)
#'
#' data("iris")
#' ggplot(data = iris, aes(x = Species, y = Sepal.Length)) +
#'   geom_boxplot() +
#'   theme_cat(
#'     aspect_ratio = 1,
#'     show_panel_grid = "both",
#'     show_title = "y"
#'   )
theme_cat <- function(aspect_ratio = NULL,
                      frame = "closed",
                      show_panel_grid = NULL,
                      show_title = "both",
                      show_ticks = "both",
                      show_text = "both",
                      text_italic = NULL,
                      legend_position = "right",
                      legend_direction = NULL,
                      font_size = 8,
                      base_family = "",
                      base_line_size = font_size / 16,
                      base_rect_size = font_size / 16,
                      ...) {
  match.arg(arg = frame, choices = c("closed", "open"))
  show_panel_grid <- show_panel_grid %||% "both_not"
  match.arg(arg = show_panel_grid,
            choices = c("x", "y", "both", "both_not"))
  show_title <- show_title %||% "both_not"
  match.arg(arg = show_title,
            choices = c("x", "y", "both", "both_not"))
  show_ticks <- show_ticks %||% "both_not"
  match.arg(arg = show_ticks,
            choices = c("x", "y", "both", "both_not"))
  show_text <- show_text %||% "both_not"
  match.arg(arg = show_text,
            choices = c("x", "y", "both", "both_not"))
  text_italic <- text_italic %||% "both_not"
  match.arg(arg = text_italic,
            choices = c("x", "y", "both", "both_not"))
  match.arg(arg = legend_position,
            choices = c("right", "left", "bottom", "top"))

  legend_margin <- switch(
    legend_position,
    "right" = margin(l = -8),
    "top" = margin(b = -8),
    "bottom" = margin(t = -8),
    "left" = margin(r = -8)
  )
  legend_direction <- legend_direction %||% "default"
  legend_direction <-
    if (legend_direction %in% c("h", "horizontal")) {
      "horizontal"
    } else if (legend_direction %in% c("v", "vertical")) {
      "vertical"
    } else if (legend_direction == "default") {
      legend_direction <- NULL
    }
  theme <- theme(
    aspect.ratio = aspect_ratio,
    # Axis
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks.length = unit(4, "pt"),
    # Panel
    panel.background = element_blank(),
    panel.grid = element_blank(),
    # Legend
    legend.background = element_blank(),
    legend.title = element_text(
      size = font_size,
      face = "plain",
      colour = "black"
    ),
    legend.text = element_text(
      size = font_size,
      face = "plain",
      colour = "black"
    ),
    legend.key = element_blank(),
    legend.key.height = unit(font_size, "pt"),
    legend.key.width = unit(font_size, "pt"),
    legend.position = legend_position,
    legend.margin = legend_margin,
    legend.direction = legend_direction,
    # Plot
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
    # Facetting
    strip.background = element_blank(),
    strip.text = element_text(
      size = font_size,
      face = "plain",
      colour = "black"
    ),
    ...
  )
  # Frame
  if (frame == "open") {
    theme <- theme + theme(
      axis.line = element_line(
        linewidth = base_line_size * 0.5 / 1.07,
        lineend = "square",
        colour = "black"
      ),
      panel.border = element_blank()
    )
  } else if (frame == "closed") {
    theme <- theme + theme(
      axis.line = element_blank(),
      panel.border = element_rect(
        linewidth = base_line_size * 0.5 / 1.07,
        fill = NA,
        colour = "black"
      )
    )
  }
  # Title
  if (show_title == "x") {
    theme <- theme + theme(
      axis.title.x = element_text(
        size = font_size,
        face = "plain",
        colour = "black"
      ),
      axis.title.y = element_blank()
    )
  } else if (show_title == "y") {
    theme <- theme + theme(
      axis.title.y = element_text(
        size = font_size,
        face = "plain",
        colour = "black"
      ),
      axis.title.x = element_blank()
    )
  } else if (show_title == "both") {
    theme <- theme + theme(axis.title = element_text(
      size = font_size,
      face = "plain",
      colour = "black"
    ))
  }
  # Ticks
  if (show_ticks == "x") {
    theme <- theme +
      theme(
        axis.ticks.x = element_line(
          linewidth = base_line_size * 0.5 / 1.07,
          lineend = "square",
          colour = "black"
        ),
        axis.ticks.y = element_blank()
      )
  } else if (show_ticks == "y") {
    theme <- theme +
      theme(
        axis.ticks.y = element_line(
          linewidth = base_line_size * 0.5 / 1.07,
          lineend = "square",
          colour = "black"
        ),
        axis.ticks.x = element_blank()
      )
  } else if (show_ticks == "both") {
    theme <- theme +
      theme(
        axis.ticks = element_line(
          linewidth = base_line_size * 0.5 / 1.07,
          lineend = "square",
          colour = "black"
        )
      )
  }
  # Fontface
  x_fontface <- ifelse(text_italic == "x" |
                         text_italic == "both", "italic", "plain")
  y_fontface <- ifelse(text_italic == "y" |
                         text_italic == "both", "italic", "plain")
  # Text
  if (show_text == "x") {
    theme <- theme +
      theme(
        axis.text.x = element_text(
          size = font_size,
          face = x_fontface,
          colour = "black"
        ),
        axis.text.y = element_blank()
      )
  } else if (show_text == "y") {
    theme <- theme +
      theme(
        axis.text.y = element_text(
          size = font_size,
          face = y_fontface,
          colour = "black"
        ),
        axis.text.x = element_blank()
      )
  } else if (show_text == "both") {
    theme <- theme +
      theme(
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
      )
  }
  # Panel grid
  if (show_panel_grid == "x") {
    theme <- theme +
      theme(
        panel.grid.major.x = element_line(
          colour = "lightgrey",
          linewidth = base_line_size * 0.5 / 1.07,
          lineend = "square"
        ),
        panel.grid.major.y = element_blank()
      )
  } else if (show_panel_grid == "y") {
    theme <- theme +
      theme(
        panel.grid.major.y = element_line(
          colour = "lightgrey",
          linewidth = base_line_size * 0.5 / 1.07,
          lineend = "square"
        ),
        panel.grid.major.x = element_blank()
      )
  } else if (show_panel_grid == "both") {
    theme <- theme +
      theme(
        panel.grid = element_line(
          colour = "lightgrey",
          linewidth = base_line_size * 0.5 / 1.07,
          lineend = "square"
        )
      )
  }
  theme <- theme + theme(...)
  return(theme)
}


#' Modify a ggplot2 plot with customized options and add significance labels
#'
#' @inheritParams theme_cat
#' @param plot_title Title of the plot.
#' @param x_axis_title Title of the x-axis.
#' @param y_axis_title Title of the y-axis.
#' @param x_text_angle Angle of the x-axis labels.
#' @param y_text_angle Angle of the y-axis labels.
#' @param legend_title Title of the legend.
#' @param legend_position Position of the legend, can be "left", "right", "top", "bottom", or NULL.
#' @param legend_direction Direction of the legend, can be "horizontal" or "vertical".
#' @param comparisons List of comparison groups for adding significance labels.
#' @param test Type of statistical test to use for calculating significance. Defaults to "wilcox.test".
#' @param step_increase Step increase for adjusting the significance label position.
#' @param map_signif_level Logical indicating whether to map the significance level to asterisks.
#'
#' @return A list of ggplot2 options for modifying a plot and adding significance labels.
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
#'   geom_boxplot() +
#'   cat_modify_plot(plot_title = "My Plot Title",
#'                 x_axis_title = "X Axis Title",
#'                 y_axis_title = "Y Axis Title",
#'                 test = "t.test",
#'                 comparisons = list(c("4", "6"), c("4", "8")),
#'                 aspect_ratio = 1)
#' p
#'
#' @export
cat_modify_plot <- function(plot_title = NULL,
                            x_axis_title = NULL,
                            y_axis_title = NULL,
                            x_text_angle = NULL,
                            y_text_angle = NULL,
                            legend_title = NULL,
                            comparisons = NULL,
                            test = "wilcox.test",
                            step_increase = 0.1,
                            map_signif_level = TRUE,
                            aspect_ratio = NULL,
                            frame = "closed",
                            font_size = 8,
                            show_panel_grid = NULL,
                            show_title = "both",
                            show_ticks = "both",
                            show_text = "both",
                            text_italic = NULL,
                            legend_position = "right",
                            legend_direction = NULL) {
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
    # Plot theme
    theme_cat(
      aspect_ratio = aspect_ratio,
      frame = frame,
      show_panel_grid = show_panel_grid,
      show_title = show_title,
      show_ticks = show_ticks,
      show_text = show_text,
      text_italic = text_italic,
      legend_position = legend_position,
      legend_direction = legend_direction,
      font_size = font_size
    )
  )
}

# cat_modify_plot <- function(p,
#                             plot_title = NULL,
#                             x_axis_title = NULL,
#                             y_axis_title = NULL,
#                             comparisons = NULL,
#                             test = "wilcox.test",
#                             step_increase = 0.1,
#                             map_signif_level = TRUE,
#                             flip = FALSE,
#                             ) {
#   # Plot title and axis title
#   p <-
#     p + labs(title = plot_title, x = x_axis_title, y = y_axis_title)
#   # Plot theme
#   p <- p +
#     theme_cat(
#       aspect_ratio = aspect_ratio,
#       frame = frame,
#       show_panel_grid = show_panel_grid,
#       show_title = show_title,
#       show_ticks = show_ticks,
#       show_text = show_text,
#       text_italic = text_italic,
#       legend_position = legend_position,
#       legend_direction = legend_direction,
#       font_size = font_size
#     )
#   # Plot legend
#   p <- p + guides(
#     x = guide_axis(angle = x_text_angle),
#     y = guide_axis(angle = y_text_angle),
#     fill = guide_legend(title = legend_title)
#   )
#
#   # Plot scale
#   if (flip) {
#     p <- p +
#       scale_x_continuous(expand = c(0, 0.3))
#   } else {
#     p <- p +
#       scale_y_continuous(expand = c(0, 0.3))
#   }
#
#   # Plot ggsignif
#   if (!is.null(comparisons)) {
#     p <- p + geom_signif(
#       comparisons = comparisons,
#       size = 0.5 * 0.5 * 0.93,
#       textsize = 0.36 * 6,
#       test = test,
#       step_increase = step_increase,
#       tip_length = 0.02,
#       map_signif_level = map_signif_level
#     )
#   }
#   return(p)
# }
