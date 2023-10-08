paired <- c(
  "PowderBlue" = "#A6CEE3",
  "NavyBlue" = "#1F78B4",
  "SpringGreen" = "#B2DF8A",
  "ForestGreen" = "#33A02C",
  "SalmonPink" = "#FB9A99",
  "Red" = "#E31A1C",
  "Apricot" = "#FDBF6F",
  "Orange" = "#FF7F00",
  "Lavender" = "#CAB2D6",
  "DeepPurple" = "#6A3D9A",
  "LemonYellow" = "#FFFF99"
)
set3 <- c(
  "Teal" = "#8DD3C7",
  "Pale Yellow" = "#FFFFB3",
  "Lavender Gray" = "#BEBADA",
  "Salmon" = "#FB8072",
  "Sky Blue" = "#80B1D3",
  "Apricot" = "#FDB462",
  "Pistachio" = "#B3DE69",
  "Pale Pink" = "#FCCDE5",
  "Light Gray" = "#D9D9D9",
  "Purple" = "#BC80BD",
  "Pale Green" = "#CCEBC5"
)
okabeito <- c(
  "Orange" = "#E69F00",
  "SkyBlue" = "#56B4E9",
  "BluishGreen" = "#009E73",
  "Yellow" = "#F0E442",
  "Blue" = "#0072B2",
  "Vermillion" = "#D55E00",
  "RediishPurple" = "#CC79A7",
  "Gray" = "#999999",
  "Black" = "#000000"
)

.brewer_palette <- function() {
  c(
    # sequential
    "Blues",
    "BuGn",
    "BuPu",
    "GnBu",
    "Greens",
    "Greys",
    "Oranges",
    "OrRd",
    "PuBu",
    "PuBuGn",
    "PuRd",
    "Purples",
    "RdPu",
    "Reds",
    "YlGn",
    "YlGnBu YlOrBr",
    "YlOrRd",
    # Divergent
    "BrBG",
    "PiYG",
    "PRGn",
    "PuOr",
    "RdBu",
    "RdGy",
    "RdYlBu",
    "RdYlGn",
    "Spectral",
    # Qualitative
    "Accent",
    "Dark2",
    "Paired",
    "Pastel1",
    "Pastel2",
    "Set1",
    "Set2",
    "Set3"
  )
}
.ggsci_palette <- function() {
  # Scientific Journal and Sci-Fi Themed Color Palettes for ggplot2
  # ggsci package:
  # https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html
  c(
    "npg",
    "aaas",
    "nejm",
    "lancet",
    "jama",
    "jco",
    "ucscgb",
    "d3",
    "locuszoom",
    "igv",
    "uchicago",
    "startrek",
    "tron",
    "futurama",
    "rickandmorty",
    "simpsons"
  )
}

#' Check if a palette name is valid
#'
#' Check if a provided name or vector of names corresponds to a valid color
#' palette. Valid palettes include those from 'ggplot2' and 'RColorBrewer', as
#' well as some additional default palettes.
#'
#' @param palette A character vector containing the names of the color palettes
#'   to check
#' @return A logical value indicating whether the palette is valid
#' @examples
#' catplot:::.is_color_palette("Paired") # TRUE
#' catplot:::.is_color_palette(c("foo")) # FALSE
#' @export
.is_color_palette <- function(palette) {
  if (is.null(palette)) {
    return(FALSE)
  } else {
    return(
      length(palette) == 1 & palette[1] %in% c(
        .brewer_palette(),
        .ggsci_palette(),
        "default",
        "hue",
        "grey_pal",
        "gray_pal"
      )
    )
  }
}

#' Get a color palette from the ggsci package
#'
#' This function returns a color palette from the ggsci package by name, with
#' the option to specify the number of colors in the palette.
#'
#' @param palette A character value indicating the name of the ggsci palette to
#'   use
#' @param n An integer value specifying the number of colors to use from the
#'   palette. Defaults to the maximum number supported by the palette if not
#'   specified.
#' @return A vector of colors from the selected ggsci palette
#' @examples
#' \dontrun{
#' palette <- catplot:::.get_ggsci_palette("npg", 6)
#'
#' barplot(1:6, col = palette)
#' }
.get_ggsci_palette <- function(
    palette = c(
      "npg",
      "aaas",
      "nejm",
      "lancet",
      "jama",
      "jco",
      "ucscgb",
      "d3",
      "locuszoom",
      "igv",
      "uchicago",
      "startrek",
      "tron",
      "futurama",
      "rickandmorty",
      "simpsons"
    ),
    n) {
  pal <- match.arg(palette)

  if (pal %in% c("npg", "aaas", "jco", "d3")) {
    max_n <- 10
  } else if (pal %in% c("nejm")) {
    max_n <- 8
  } else if (pal %in% c("jama", "locuszoom", "startrek", "tron")) {
    max_n <- 7
  } else if (pal %in% c("igv")) {
    max_n <- 51
  } else if (pal %in% c("lancet", "uchicago")) {
    max_n <- 9
  } else if (pal %in% c("ucscgb")) {
    max_n <- 26
  } else if (pal %in% c("futurama", "rickandmorty")) {
    max_n <- 12
  } else if (pal %in% c("simpsons")) {
    max_n <- 16
  } else {
    stop("Don't support palette name: ", pal)
  }

  functs <- list(
    npg = ggsci::pal_npg(),
    aaas = ggsci::pal_aaas(),
    nejm = ggsci::pal_nejm(),
    lancet = ggsci::pal_lancet(),
    jama = ggsci::pal_jama(),
    jco = ggsci::pal_jco(),
    ucscgb = ggsci::pal_ucscgb(),
    d3 = ggsci::pal_d3(),
    locuszoom = ggsci::pal_locuszoom(),
    igv = ggsci::pal_igv(),
    uchicago = ggsci::pal_uchicago(),
    startrek = ggsci::pal_startrek(),
    tron = ggsci::pal_tron(),
    futurama = ggsci::pal_futurama(),
    rickandmorty = ggsci::pal_rickandmorty(),
    simpsons = ggsci::pal_simpsons()
  )

  if (n <= max_n) {
    functs[[pal]](n)
  } else {
    grDevices::colorRampPalette(functs[[pal]](max_n))(n)
  }
}

#' Get a color palette from the RColorBrewer package
#'
#' This function returns a color palette from the RColorBrewer package by name,
#' with the option to specify the number of colors in the palette.
#'
#' @param palette A character value indicating the name of the RColorBrewer
#'   palette to use
#' @param n An integer value specifying the number of colors to use from the
#'   palette.
#' @return A vector of colors from the selected RColorBrewer palette
#' @examples
#' palette <- catplot:::.get_brewer_palette("Blues", 6)
#'
#' barplot(1:6, col = palette)
.get_brewer_palette <- function(palette, n) {
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop(
      "RColorBrewer package needed.
      Please install it using install.packages('RColorBrewer')."
    )
  }
  initial_n <- n
  n <- max(c(n, 3))
  pal <- palette[1]
  sequential <- c(
    "Blues",
    "BuGn",
    "BuPu",
    "GnBu",
    "Greens",
    "Greys",
    "Oranges",
    "OrRd",
    "PuBu",
    "PuBuGn",
    "PuRd",
    "Purples",
    "RdPu",
    "Reds",
    "YlGn",
    "YlGnBu YlOrBr",
    "YlOrRd"
  )
  divergent <-
    c(
      "BrBG",
      "PiYG",
      "PRGn",
      "PuOr",
      "RdBu",
      "RdGy",
      "RdYlBu",
      "RdYlGn",
      "Spectral"
    )

  if (pal %in% sequential) {
    max_n <- 9
  } else if (pal %in% divergent) {
    max_n <- 11
  } else if (pal %in% c("Accent", "Dark2", "Pastel2", "Set2")) {
    max_n <- 8
  } else if (pal %in% c("Pastel1", "Set1")) {
    max_n <- 9
  } else if (pal %in% c("Paired", "Set3")) {
    max_n <- 12
  } else {
    stop("Don't support palette name: ", pal)
  }

  if (n <= max_n) {
    cols <- RColorBrewer::brewer.pal(n, palette)
    if (initial_n == 2) {
      cols <- cols[c(1, 3)]
    } else if (initial_n == 1) {
      cols <- cols[1]
    }
    cols
  } else {
    grDevices::colorRampPalette(RColorBrewer::brewer.pal(max_n, palette))(n)
  }
}

#' Generate a color palette
#'
#' This function generates a color palette based on the input parameters.
#'
#' @param palette A character string specifying the name of the color palette to
#'   use. The default value is "default". Valid options include "default",
#'   "hue", "grey", "gray", "brewer", and "ggsci".
#' @param n An integer specifying the number of colors to generate. The default
#'   value is NULL, which generates a default number of colors based on the
#'   selected palette.
#' @param color_names A character vector specifying names for the individual
#'   colors in the palette. The length of this vector should be the same as the
#'   number of colors in the palette. The default value is NULL, which does not
#'   assign names to the colors.
#' @param modification A character string specifying the modification to apply
#'   to the colors. The default value is "go_lighter". Valid options include
#'   "go_darker", "go_lighter", and "go_both_ways".
#' @param view_palette A logical value indicating whether to display the
#'   generated color palette. The default value is TRUE.
#' @param view_labels A logical value indicating whether to display labels for
#'   the colors in the generated palette. The default value is TRUE.
#'
#' @return A vector of colors, with each element representing a color in the
#'   generated palette.
#' @export
#' @examples
#' generate_colors(palette = "default", n = 5)
#' generate_colors(palette = "Paired", n = 8, view_palette = FALSE)
#' generate_colors(
#'   palette = "npg",
#'   n = 3,
#'   color_names = c("red", "blue", "green"),
#'   view_labels = FALSE
#' )
generate_colors <-
  function(palette = NULL,
           n = NULL,
           color_names = NULL,
           modification = NULL,
           view_palette = TRUE,
           view_labels = TRUE) {
    if (is.null(palette)) {
      palette <- "default"
    }
    if (is.null(modification)) {
      modification <- "go_lighter"
    }
    if (length(palette) == 1) {
      switch(palette,
        "default" = {
          hues <- seq(15, 375, length = n + 1)
          colors <-
            grDevices::hcl(
              h = hues,
              l = 65,
              c = 100,
              alpha = 1
            )[1:n]
        },
        "hue" = {
          hues <- seq(15, 375, length = n + 1)
          colors <-
            grDevices::hcl(
              h = hues,
              l = 65,
              c = 100,
              alpha = 1
            )[1:n]
        },
        "grey" = {
          colors <- grDevices::grey.colors(n, start = 0.2, end = 0.8)
        },
        "gray" = {
          colors <- grDevices::grey.colors(n, start = 0.2, end = 0.8)
        },
        {
          if (.is_color_palette(palette)) {
            if (palette %in% .brewer_palette()) {
              colors <- .get_brewer_palette(palette, n)
            } else if (palette %in% .ggsci_palette()) {
              colors <- .get_ggsci_palette(palette, n)
            }
          } else if (length(palette) == 1 && .is_color(palette)) {
            colors <-
              monochromeR::generate_palette(
                colour = palette,
                modification = modification,
                n_colours = n
              )
          }
        }
      )
    } else {
      colors <- grDevices::colorRampPalette(palette)(n)
    }
    if (view_palette) {
      print(monochromeR::view_palette(colors, view_labels = view_labels))
    }

    if (!is.null(color_names)) {
      if (length(color_names) != length(colors)) {
        stop("The name of the color and the length
             of the color need to be consistent!")
      }
      names(colors) <- color_names
    }
    colors
  }

#' Scale fill for categorical plots
#'
#' This function generates colors using the \code{\link{generate_colors}}
#' function and applies them to a manual scale for fill aesthetics.
#'
#' @param palette A character string specifying the name of the color palette to
#'   use. The default value is "Paired". Valid options include any palette name
#'   recognized by the \code{\link{generate_colors}} function.
#' @param n An integer specifying the number of colors to generate. The default
#'   value is NULL, which generates a default number of colors based on the
#'   selected palette.
#' @param color_names A character vector specifying names for the individual
#'   colors in the palette. The length of this vector should be the same as the
#'   number of colors in the palette. The default value is NULL, which does not
#'   assign names to the colors.
#' @param modification A character string specifying the modification to apply
#'   to the colors. The default value is "go_lighter". Valid options include
#'   "go_darker", "go_lighter", and "go_both_ways".
#' @param direction An integer specifying the direction in which to order the
#'   colors. The default value is 1, which orders the colors from light to dark.
#'   A value of -1 orders the colors from dark to light.
#'
#' @return A manual scale for fill aesthetics using the generated colors.
#' @export
#' @examples
#' scale_fill_catplot(palette = "Set1", n = 3)
#' scale_fill_catplot(
#'   palette = "Dark2",
#'   n = 3,
#'   color_names = c("A", "B", "C"),
#'   direction = -1
#' )
scale_fill_catplot <- function(palette = "Paired",
                               n = NULL,
                               color_names = NULL,
                               modification = "go_lighter",
                               direction = 1) {
  if (!direction %in% c(1, -1)) {
    stop("The value of 'direction' must be either 1 or -1.")
  }
  colors <- generate_colors(
    palette = palette,
    n = n,
    color_names = color_names,
    modification = modification,
    view_palette = FALSE,
    view_labels = FALSE
  )
  if (direction == -1) {
    colors <- rev(colors)
  }
  scale_fill_manual(values = colors)
}

#' Scale color for categorical plots
#'
#' This function generates colors using the \code{\link{generate_colors}}
#' function and applies them to a manual scale for color aesthetics.
#'
#' @param palette A character string specifying the name of the color palette to
#'   use. The default value is "Paired". Valid options include any palette name
#'   recognized by the \code{\link{generate_colors}} function.
#' @param n An integer specifying the number of colors to generate. The default
#'   value is NULL, which generates a default number of colors based on the
#'   selected palette.
#' @param color_names A character vector specifying names for the individual
#'   colors in the palette. The length of this vector should be the same as the
#'   number of colors in the palette. The default value is NULL, which does not
#'   assign names to the colors.
#' @param modification A character string specifying the modification to apply
#'   to the colors. The default value is "go_lighter". Valid options include
#'   "go_darker", "go_lighter", and "go_both_ways".
#' @param direction An integer specifying the direction in which to order the
#'   colors. The default value is 1, which orders the colors from light to dark.
#'   A value of -1 orders the colors from dark to light.
#'
#' @return A manual scale for color aesthetics using the generated colors.
#' @export
#' @examples
#' scale_color_catplot(palette = "Set1", n = 3)
#' scale_color_catplot(
#'   palette = "Dark2",
#'   n = 3,
#'   color_names = c("A", "B", "C"),
#'   direction = -1
#' )
scale_color_catplot <- function(palette = "Paired",
                                n = NULL,
                                color_names = NULL,
                                modification = "go_lighter",
                                direction = 1) {
  if (!direction %in% c(1, -1)) {
    stop("The value of 'direction' must be either 1 or -1.")
  }
  colors <- generate_colors(
    palette = palette,
    n = n,
    color_names = color_names,
    modification = modification,
    view_palette = FALSE,
    view_labels = FALSE
  )
  if (direction == -1) {
    colors <- rev(colors)
  }
  scale_color_manual(values = colors)
}

#' Modify color aesthetics for categorical plots
#'
#' This function generates colors using the \code{\link{generate_colors}}
#' function and applies them to manual scales for color and/or fill aesthetics.
#'
#' @param palette A character string specifying the name of the color palette to
#'   use. The default value is "Paired". Valid options include any palette name
#'   recognized by the \code{\link{generate_colors}} function.
#' @param n An integer specifying the number of colors to generate. The default
#'   value is NULL, which generates a default number of colors based on the
#'   selected palette.
#' @param color_names A character vector specifying names for the individual
#'   colors in the palette. The length of this vector should be the same as the
#'   number of colors in the palette. The default value is NULL, which does not
#'   assign names to the colors.
#' @param modification A character string specifying the modification to apply
#'   to the colors. The default value is "go_lighter". Valid options include
#'   "go_darker", "go_lighter", and "go_both_ways".
#' @param direction An integer specifying the direction in which to order the
#'   colors. The default value is 1, which orders the colors from light to dark.
#'   A value of -1 orders the colors from dark to light.
#' @param aesthetics A character string specifying which aesthetics to modify.
#'   The default value is "both". Valid options include "fill", "color", and
#'   "both".
#'
#' @return A manual scale or list of manual scales for color and/or fill
#'   aesthetics using the generated colors.
#' @export
#' @examples
#' library(catplot)
#'
#' cat_modify_color(palette = "Set1", n = 3, aesthetics = "fill")
#' cat_modify_color(
#'   palette = "Dark2",
#'   n = 3,
#'   color_names = c("A", "B", "C"),
#'   direction = -1,
#'   aesthetics = "color"
#' )
cat_modify_color <- function(palette = "Paired",
                             n = NULL,
                             color_names = NULL,
                             modification = "go_lighter",
                             direction = 1,
                             aesthetics = "both") {
  fill <- scale_fill_catplot(
    palette = palette,
    n = n,
    color_names = color_names,
    modification = modification,
    direction = direction
  )
  color <- scale_color_catplot(
    palette = palette,
    n = n,
    color_names = color_names,
    modification = modification,
    direction = direction
  )
  return(switch(aesthetics,
    "fill" = fill,
    "color" = color,
    "both" = list(fill, color)
  ))
}
