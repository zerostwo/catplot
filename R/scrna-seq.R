#' Create a dimensionality reduction plot for categorical data
#'
#' This function creates a dimensionality reduction plot for categorical data using Seurat and ggplot2. It allows for the selection of the reduction method, grouping, and splitting variables, as well as the visualization of labels, rasterization, and color palette. The cat_dimplot() function is intended to be used as a wrapper around Seurat's DimPlot() function.
#'
#' @param object A Seurat object containing categorical data.
#' @param reduction The dimensionality reduction method. Default is NULL.
#' @param group_by The variable to group data by. Default is NULL.
#' @param split_by The variable to split data by. Default is NULL.
#' @param label Logical value indicating whether to show labels on the plot. Default is FALSE.
#' @param raster Logical value indicating whether to use rasterization for improved performance. Default is TRUE.
#' @param show_legend Logical value indicating whether to show the legend on the plot. Default is TRUE.
#' @param show_axis Logical value indicating whether to show the axis on the plot. Default is TRUE.
#' @param show_border Logical value indicating whether to show the panel and axis borders on the plot. Default is TRUE.
#' @param title The title for the plot. Default is NULL.
#' @param pt_size The size of the points on the plot. Default is 1.
#' @param repel Logical value indicating whether to use point repulsion to avoid overlapping labels. Default is TRUE.
#' @param palette The color palette to use for the plot. Default is "Paired".
#' @param ... Additional parameters to be passed to the DimPlot() function in Seurat.
#' @return A ggplot2 object containing the dimensionality reduction plot.
#'
#' @examples
#' library(Seurat)
#' library(ggplot2)
#' data("pbmc_small")
#' pbmc_small <- CreateSeuratObject(counts = pbmc_small)
#' Idents(pbmc_small) <- pbmc_small$percent.mt > 10
#' cat_dimplot(object = pbmc_small, reduction = "tsne", group_by = "ident", palette = "Set1")
#'
#' @importFrom ggplot2 theme element_blank element_text margin scale_color_manual labs
#' @importFrom grDevices colorRampPalette
#'
#' @export
cat_dimplot <- function(object,
                        reduction = NULL,
                        group_by = NULL,
                        split_by = NULL,
                        label = FALSE,
                        raster = TRUE,
                        show_legend = TRUE,
                        show_axis = TRUE,
                        show_border = TRUE,
                        title = NULL,
                        pt_size = 1,
                        repel = TRUE,
                        palette = "Paired",
                        ...) {
  p <- Seurat::DimPlot(
    object = object,
    reduction = reduction,
    label = label,
    repel = repel,
    group.by = group_by,
    split.by = split_by,
    label.size = 8 * 0.36,
    pt.size = pt_size,
    raster = raster,
    seed = 717,
    ...
  )
  if (!show_legend) {
    p <- p + Seurat::NoLegend()
  }
  reduction <- reduction %||% DefaultDimReduc(object = object)
  object[["ident"]] <- Seurat::Idents(object = object)
  group_by <- group_by %||% "ident"
  n <- length(table(object[[group_by]]))
  p <- p +
    theme_cat(aspect_ratio = 1) +
    theme(legend.margin = margin(l = -8)) +
    labs(
      x = paste0(stringr::str_to_upper(reduction), " 1"),
      y = paste0(stringr::str_to_upper(reduction), " 2"),
      title = title
    )

  if (!is.null(palette)) {
    if (n > 12) {
      values <- colorRampPalette(RColorBrewer::brewer.pal(12, palette))(n)
    } else {
      values <- RColorBrewer::brewer.pal(12, palette)
    }
    p <- p + scale_color_manual(values = values)
  }
  if (!show_axis) {
    p <- p + theme(
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  }
  if (!show_border) {
    p <- p + theme(
      panel.border = element_blank(),
      axis.line = element_blank()
    )
  }
  return(p)
}

#' Plot a violin plot with categorical groups
#'
#' This function plots a violin plot with categorical groups using the VlnPlot function from the Seurat package.
#'
#' @param object A Seurat object containing the data to plot.
#' @param features A character vector of feature names to plot.
#' @param pt_size The size of the points to plot.
#' @param sort Whether to sort the features by their mean expression or not.
#' @param group_by A character vector specifying the grouping variable. Defaults to "ident".
#' @param split_by A character vector specifying the splitting variable.
#' @param show_legend Whether to show the legend or not. Defaults to FALSE.
#' @param angle_x The angle of the x-axis labels. Defaults to 0.
#' @param aspect_ratio The aspect ratio of the plot. Defaults to 0.5.
#'
#' @return A ggplot2 object.
#'
#' @importFrom ggplot2 expansion guide_axis scale_fill_manual
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' cat_vlnplot(object = mySeuratObject, features = c("CD3D", "CD8A", "CD4"))
#'
#' @export
cat_vlnplot <- function(object,
                        features,
                        pt_size = 0,
                        sort = FALSE,
                        group_by = NULL,
                        split_by = NULL,
                        show_legend = FALSE,
                        angle_x = 0,
                        aspect_ratio = 0.5) {
  p <- Seurat::VlnPlot(
    object = object,
    features = features,
    pt.size = pt_size,
    sort = sort,
    group.by = group_by,
    split.by = split_by
  )
  if (!show_legend) {
    p <- p + Seurat::NoLegend()
  }
  object[["ident"]] <- Seurat::Idents(object = object)
  group_by <- group_by %||% "ident"
  n <- length(table(object[[group_by]]))
  if (n > 12) {
    values <- colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(n)
  } else {
    values <- RColorBrewer::brewer.pal(12, "Paired")
  }
  p <- p + theme_cat(aspect_ratio = aspect_ratio) +
    theme(
      axis.title.x = element_blank(),
      legend.margin = margin(l = -8)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    guides(x = guide_axis(angle = angle_x)) +
    scale_fill_manual(values = values)
  return(p)
}

#' Plot a dot plot with categorical groups
#'
#' This function plots a dot plot with categorical groups using the DotPlot function from the Seurat package.
#'
#' @param x A Seurat object containing the data to plot.
#' @param features A character vector of feature names to plot.
#' @param dot_scale The size of the dots to plot.
#' @param col_min The minimum value for the color scale. Defaults to -2.5.
#' @param col_max The maximum value for the color scale. Defaults to 2.5.
#'
#' @return A ggplot2 object.
#'
#' @importFrom ggplot2 coord_fixed guides theme element_text margin scale_color_distiller scale_y_discrete guide_axis guide_legend guide_colorbar
#'
#' @examples
#' cat_dotplot(x = mySeuratObject, features = c("CD3D", "CD8A", "CD4"))
#'
#' @export
cat_dotplot <- function(x,
                        features,
                        dot_scale = 4,
                        col_min = -2.5,
                        col_max = 2.5,
                        assay = NULL) {
  p <-
    Seurat::DotPlot(
      x,
      dot.scale = dot_scale,
      features = features,
      col.min = col_min,
      col.max = col_max,
      assay = assay,
      scale = TRUE
    ) + coord_fixed() +
    guides(
      x = guide_axis(angle = 90),
      size = guide_legend(title = "Percent (%)"),
      color = guide_colorbar(
        title = "Z score",
        frame.colour = "black",
        frame.linewidth = 0.2,
        ticks.colour = "black",
        ticks.linewidth = 0.2
      )
    ) +
    theme_cat() +
    theme(
      axis.text.x = element_text(face = "italic"),
      axis.title = element_blank(),
      legend.margin = margin(l = -8),
      panel.grid = element_line(
        colour = "lightgrey",
        linewidth = 0.2 / 1.07
      )
    ) +
    scale_color_distiller(palette = "RdBu") +
    scale_y_discrete(limits = rev)
  return(p)
}

#' Plot feature expression in reduced dimensions
#'
#' This function plots feature expression in reduced dimensions using the FeaturePlot function from the Seurat package.
#'
#' @param object A Seurat object containing the data to plot.
#' @param features A character vector of feature names to plot.
#' @param reduction A character string specifying the dimensionality reduction to use (e.g., "PCA", "UMAP", "tSNE"). Defaults to NULL.
#' @param label A character vector specifying the labels to use for each cell group. Defaults to label.
#' @param split_by A character vector specifying the cell groups to split the plot by. Defaults to NULL.
#' @param label_size A numeric value specifying the size of the labels. Defaults to 8 * 0.36.
#' @param pt_size A numeric value specifying the size of the points. Defaults to 1.
#' @param slot A character string specifying which slot in the Seurat object to use (e.g., "data", "scale.data", "integrated"). Defaults to "data".
#' @param max_cutoff A numeric value specifying the maximum expression cutoff. Defaults to NA.
#' @param raster A logical value specifying whether to use raster graphics. Defaults to TRUE.
#' @param seed An integer value specifying the random seed. Defaults to 717.
#' @param title A character string specifying the plot title. Defaults to NULL.
#' @param legend_title A character string specifying the legend title. Defaults to NULL.
#' @param show_legend A logical value specifying whether to show the legend. Defaults to TRUE.
#' @param show_axis A logical value specifying whether to show the plot axis. Defaults to TRUE.
#' @param show_border A logical value specifying whether to show the plot border. Defaults to TRUE.
#' @param palette A character string specifying the color palette to use. Defaults to "YlOrRd".
#' @param direction A numeric value specifying the direction of the color palette. Defaults to 1.
#' @param ... Additional parameters to pass to FeaturePlot.
#'
#' @return A ggplot2 object.
#'
#' @importFrom ggplot2 labs scale_color_distiller guides
#' @importFrom ggplot2 theme element_blank element_text margin
#'
#' @examples
#' cat_featureplot(x = mySeuratObject, features = c("CD3D", "CD8A", "CD4"), reduction = "UMAP")
#'
#' @export
cat_featureplot <-
  function(object,
           features,
           reduction = NULL,
           label = label,
           split_by = NULL,
           label_size = 8 * 0.36,
           pt_size = 1,
           slot = "data",
           max_cutoff = NA,
           raster = TRUE,
           seed = 717,
           title = NULL,
           legend_title = NULL,
           show_legend = TRUE,
           show_axis = TRUE,
           show_border = TRUE,
           palette = "YlOrRd",
           direction = 1,
           ...) {
    p <- Seurat::FeaturePlot(
      object = object,
      features = features,
      reduction = reduction,
      split.by = split_by,
      label.size = 8 * 0.36,
      pt.size = pt_size,
      slot = slot,
      raster = raster,
      order = TRUE,
      max.cutoff = max_cutoff,
      ...
    )
    if (!show_legend) {
      p <- p + Seurat::NoLegend()
    }
    reduction <- reduction %||% DefaultDimReduc(object = object)
    object[["ident"]] <- Seurat::Idents(object = object)
    title <- title %||% features
    p <- p +
      theme_cat(aspect_ratio = 1) +
      theme(legend.margin = margin(l = -8)) +
      labs(
        x = paste0(stringr::str_to_upper(reduction), " 1"),
        y = paste0(stringr::str_to_upper(reduction), " 2"),
        title = title
      )

    if (!is.null(palette)) {
      p <-
        p + scale_color_distiller(palette = palette, direction = direction)
    }
    if (!is.null(legend_title)) {
      p <- p + guides(color = guide_colorbar(title = legend_title))
    }
    if (!show_axis) {
      p <- p + theme(
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
    }
    if (!show_border) {
      p <- p + theme(
        panel.border = element_blank(),
        axis.line = element_blank()
      )
    }
    return(p)
  }
