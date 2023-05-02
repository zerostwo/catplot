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
#' @param base_size numeric; base size of text elements in the theme
#' @param base_family character; base font family of text elements in the theme
#' @param base_line_size numeric; base line size of elements in the theme
#' @param base_rect_size numeric; base line size of rect in the theme
#' @param ... additional arguments passed to \code{\link[ggplot2:theme]{theme}}
#'
#' @return a ggplot2 theme object
#'
#' @importFrom ggplot2 margin
#'
#' @export
#'
#' @examples
#' library(catplot)
#' library(ggplot2)
#'
#' data("iris")
#' iris |>
#'   ggplot(aes(x = Species, y = Sepal.Length)) +
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
                      base_size = 8,
                      base_family = "",
                      base_line_size = base_size / 16,
                      base_rect_size = base_size / 16,
                      ...) {
  match.arg(arg = frame, choices = c("closed", "open"))
  show_panel_grid <- show_panel_grid %||% "both_not"
  match.arg(
    arg = show_panel_grid,
    choices = c("x", "y", "both", "both_not")
  )
  show_title <- show_title %||% "both_not"
  match.arg(
    arg = show_title,
    choices = c("x", "y", "both", "both_not")
  )
  show_ticks <- show_ticks %||% "both_not"
  match.arg(
    arg = show_ticks,
    choices = c("x", "y", "both", "both_not")
  )
  show_text <- show_text %||% "both_not"
  match.arg(
    arg = show_text,
    choices = c("x", "y", "both", "both_not")
  )
  text_italic <- text_italic %||% "both_not"
  match.arg(
    arg = text_italic,
    choices = c("x", "y", "both", "both_not")
  )
  match.arg(
    arg = legend_position,
    choices = c("right", "left", "bottom", "top")
  )

  legend_margin <- switch(legend_position,
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
      size = base_size,
      face = "plain",
      colour = "black"
    ),
    legend.text = element_text(
      size = base_size,
      face = "plain",
      colour = "black"
    ),
    legend.key = element_blank(),
    legend.key.height = unit(base_size, "pt"),
    legend.key.width = unit(base_size, "pt"),
    legend.position = legend_position,
    legend.margin = legend_margin,
    legend.direction = legend_direction,
    # Plot
    plot.background = element_blank(),
    plot.title = element_text(
      size = base_size,
      hjust = 0.5,
      vjust = -1,
      face = "plain",
      colour = "black"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = base_size,
      face = "plain",
      colour = "black"
    ),
    # Facetting
    strip.background = element_blank(),
    strip.text = element_text(
      size = base_size,
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
        size = base_size,
        face = "plain",
        colour = "black"
      ),
      axis.title.y = element_blank()
    )
  } else if (show_title == "y") {
    theme <- theme + theme(
      axis.title.y = element_text(
        size = base_size,
        face = "plain",
        colour = "black"
      ),
      axis.title.x = element_blank()
    )
  } else if (show_title == "both") {
    theme <- theme + theme(axis.title = element_text(
      size = base_size,
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
          size = base_size,
          face = x_fontface,
          colour = "black"
        ),
        axis.text.y = element_blank()
      )
  } else if (show_text == "y") {
    theme <- theme +
      theme(
        axis.text.y = element_text(
          size = base_size,
          face = y_fontface,
          colour = "black"
        ),
        axis.text.x = element_blank()
      )
  } else if (show_text == "both") {
    theme <- theme +
      theme(
        axis.text.x = element_text(
          size = base_size,
          face = x_fontface,
          colour = "black"
        ),
        axis.text.y = element_text(
          size = base_size,
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
        panel.grid.major = element_line(
          colour = "lightgrey",
          linewidth = base_line_size * 0.5 / 1.07,
          lineend = "square"
        )
      )
  }
  theme <- theme + theme(...)
  return(theme)
}
