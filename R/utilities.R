#' @importFrom ggplot2 element_blank element_line element_rect element_text
#'   margin unit aes as_label enquo geom_boxplot ggplot guide_axis guide_legend
#'   guides labs scale_x_continuous scale_y_continuous quo_name geom_jitter
#'   position_jitter scale_fill_manual scale_color_manual position_jitterdodge
#'   theme expansion waiver layer GeomPoint
#'   GeomBoxplot GeomDensity GeomRug GeomViolin StatSummary ggsave last_plot geom_hline mean_se position_dodge2
#' @importFrom ggsignif geom_signif
#' @importFrom rlang %||% list2 arg_match0
#' @importFrom dplyr group_by summarise arrange desc pull case_when mutate
#' @importFrom grDevices col2rgb
NULL

#' @importFrom magrittr %>%
##' @export
magrittr::`%>%`

#' Check if an object is a character vector or a factor
#'
#' @description Check if a given object is a character vector or a factor.
#' This function is used internally by other functions.
#'
#' @param x An object to check
#'
#' @return A logical value indicating whether the object is a character vector
#'   or a factor
#'
#' @examples
#' x <- c("a", "b", "c")
#' y <- factor(x)
#' catplot:::.is_char_or_factor(x)
#' catplot:::.is_char_or_factor(y)
.is_char_or_factor <- function(x) {
  class(x) %in% c("character", "factor")
}

#' Check if a string is a valid color value
#'
#' This function takes a string and attempts to parse it as a color value using
#' the \code{col2rgb()} function. If the color value is valid, \code{TRUE} is
#' returned. Otherwise, \code{FALSE} is returned.
#'
#' @param str A character string specifying the color value to be checked.
#' @return A logical value indicating whether the input string is a valid color
#'   value.
#' @examples
#' catplot:::.is_color("red") # returns TRUE
#' catplot:::.is_color("#FFA500") # returns TRUE
#' catplot:::.is_color("fdafd") # returns FALSE
.is_color <- function(str) {
  rgb_val <- tryCatch(
    expr = col2rgb(str),
    error = function(e) {
      NULL
    }
  )
  is_valid_color <- !is.null(rgb_val)
  return(is_valid_color)
}

#' Construct aesthetic mappings or parameters for a plot
#'
#' Create a mapping and parameters for a plot with a given \code{color} and
#' \code{fill}. The function checks if the color and fill arguments are valid
#' colors and, if so, adds them to the list of \code{params}. Otherwise, they
#' are added to the \code{mapping_props} list. Additional parameter can be
#' provided to customize the mapping and parameters.
#'
#' @param color A character string specifying the color to be used in the plot
#' @param fill A character string specifying the fill color to be used in the plot
#' @param shape A character string specifying the shape to be used in the plot
#' @param alpha A character string specifying the alpha value to be used in the plot
#' @param size A character string specifying the size to be used in the plot
#' @return A list containing the mapping and parameters for the plot
#'
#' @export
create_ap <- function(color = NULL,
                      fill = NULL,
                      shape = NULL,
                      alpha = NULL,
                      size = NULL) {
  color <- enquo(color)
  color_quo_name <- quo_name(color)
  fill <- enquo(fill)
  fill_quo_name <- quo_name(fill)
  shape <- enquo(shape)
  shape_quo_name <- quo_name(shape)
  alpha <- enquo(alpha)
  alpha_quo_name <- quo_name(alpha)
  size <- enquo(size)
  size_quo_name <- quo_name(size)

  mapping_props <- list()
  params <- list2()

  if (color_quo_name != "NULL") {
    if (!is.na(color_quo_name) &&
      .is_color(color_quo_name)) {
      params$color <- color_quo_name
    } else if (is.na(color_quo_name)) {
      params$color <- NA
    } else if (!.is_color(color_quo_name)) {
      mapping_props$color <- color
    }
  }
  if (fill_quo_name != "NULL") {
    if (!is.na(fill_quo_name) &&
      .is_color(fill_quo_name)) {
      params$fill <- fill_quo_name
    } else if (is.na(fill_quo_name)) {
      params$fill <- NA
    } else {
      mapping_props$fill <- fill
    }
  }
  if (shape_quo_name != "NULL") {
    print(shape_quo_name)
    is_numeric <- grepl("^[0-9]+([.][0-9]+)?$", shape_quo_name)
    print(is_numeric)
    if (is_numeric) {
      params$shape <- as.numeric(shape_quo_name)
    } else {
      mapping_props$shape <- shape
    }
  }
  if (alpha_quo_name != "NULL") {
    is_numeric <- grepl("^[0-9]+([.][0-9]+)?$", alpha_quo_name)
    if (is_numeric) {
      params$alpha <- as.numeric(alpha_quo_name)
    } else {
      mapping_props$alpha <- alpha
    }
  }
  if (size_quo_name != "NULL") {
    is_numeric <- grepl("^[0-9]+([.][0-9]+)?$", size_quo_name)
    if (is_numeric) {
      params$size <- as.numeric(size_quo_name)
    } else {
      mapping_props$size <- size
    }
  }

  mapping <- aes(!!!mapping_props)
  return(list(mapping = mapping, params = params))
}


#' Save a ggplot object to a file
#'
#' This function saves a ggplot object to a file. It provides an option to save
#' the raw data used to create the plot as a separate CSV file.
#'
#' @param filename A character string specifying the name of the file to save
#'   the plot to
#' @param plot A ggplot object to be saved
#' @param device A character string specifying the graphics device to use
#' @param path A character string specifying the directory to save the file to
#' @param scale A numeric value specifying the scaling factor for the plot
#' @param width A numeric value specifying the width of the plot
#' @param height A numeric value specifying the height of the plot
#' @param units A character string specifying the units of the width and height
#'   arguments
#' @param dpi A numeric value specifying the resolution of the plot
#' @param limitsize A logical value specifying whether to limit the size of the
#'   plot to the device size
#' @param bg A character string specifying the background color of the plot
#' @param source_data A logical value specifying whether to save the raw data used
#'   to create the plot as a separate CSV file
#' @param ... Additional arguments to be passed to ggsave
#' @importFrom utils write.csv
#' @export
cat_save <- function(filename,
                     plot = last_plot(),
                     device = NULL,
                     path = NULL,
                     scale = 1,
                     width = NA,
                     height = NA,
                     units = c("in", "cm", "mm", "px"),
                     dpi = 300,
                     limitsize = TRUE,
                     bg = NULL,
                     source_data = FALSE,
                     ...) {
  if (is.null(path)) {
    # cat_save(filename="~/figure1a.pdf", path = NULL)
    path <- cat_set_path(dirname(filename))
    filename <- basename(filename)
  } else {
    # cat_save(filename="figure1a.pdf", path = "~/")
    path <- cat_set_path(path)
  }

  if (source_data) {
    prefix <- tools::file_path_sans_ext(filename)
    data <- plot$data
    data_file <- file.path(path, paste0(prefix, ".data.csv"))
    write.csv(
      x = data,
      file = data_file
    )
    plot_file <- file.path(path, paste0(prefix, ".plot.rds"))
    saveRDS(
      object = plot,
      file = plot_file
    )
  }
  ggsave(
    filename = filename,
    plot = plot,
    device = device,
    path = path,
    scale = scale,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    limitsize = limitsize,
    bg = bg,
    ...
  )
}

#' Create a directory if it does not exist and return the path
#'
#' This function takes a path as input and creates a directory if it does not
#' exist. It then returns the path.
#'
#' @param path A character string specifying the path to be created.
#'
#' @return A character string specifying the path.
#'
#' @examples
#' cat_set_path("~/my_folder")
#'
#' @export
cat_set_path <- function(path) {
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  return(path)
}


#' Create a new project directory structure
#'
#' @param path A character string specifying the path to the project directory
#'
#' @return A character string specifying the path to the project directory
#' @export
#'
#' @examples
#' create_new_project("~/my_project")
create_new_project <- function(path) {
  project_name <- basename(path)
  # code: scripts, functions, figures
  cat_set_path(file.path(path, "code", "scripts"))
  cat_set_path(file.path(path, "code", "functions"))
  cat_set_path(file.path(path, "code", "figures"))
  cat_set_path(file.path(path, "code", "workflows"))
  # data: raw, intermediate, processed
  cat_set_path(file.path(path, "data", "raw"))
  cat_set_path(file.path(path, "data", "intermediate"))
  cat_set_path(file.path(path, "data", "processed"))
  # results: figures, tables, reports
  cat_set_path(file.path(path, "results", "figures"))
  cat_set_path(file.path(path, "results", "tables"))
  cat_set_path(file.path(path, "results", "reports"))
  # logs
  cat_set_path(file.path(path, "logs"))
  # Rproj
  writeLines(
    sprintf(
      "Version: 1.0\nRestoreWorkspace: Default\nSaveWorkspace: Default\nAlwaysSaveHistory: Default\n\nRCodeVersion: %s\nShowHiddenFiles: Default\nLibs: %s\n",
      R.version$version.string,
      .libPaths()
    ),
    file.path(path, sprintf("%s.Rproj", project_name))
  )
  # .gitignore
  writeLines(
    c(
      "data/intermediate/",
      "data/processed/",
      "results/figures/",
      "results/tables/",
      "results/reports/",
      "logs/",
      ".Rproj.user/",
      ".Rhistory"
    ),
    file.path(path, ".gitignore")
  )
  # README
  writeLines(
    sprintf(
      "# %s\n\n## Description\n\n## Usage\n\n## Author\n\n## License\n\n## References",
      project_name
    ),
    file.path(path, "README.md")
  )

  system(paste0("git init ", path))
  # Congradulations
  message(
    sprintf(
      "Congradulations! You have created a new project at %s",
      path
    )
  )
}
