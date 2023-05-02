
<!-- README.md is generated from README.Rmd. Please edit that file -->

# catplot: Capable And Tity Plot <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->
<!-- badges: end -->

## 🤪 Overview

`catplot` is a capable and tity data visualization tool and maintained
by [Songqi Duan](https://songqi.online).

## 📦 Installation

Install the latest version from
[GitHub](https://github.com/zerostwo/catplot) as follow:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("zerostwo/catplot")
```

## 🕹️ Usage

``` r
library(catplot)
library(ggplot2)

data("iris")
p <- iris |>
  ggplot(aes(x = Species, y = Sepal.Length)) +
  geom_boxplot() +
  theme_cat(
    aspect_ratio = 1,
    show_panel_grid = "both",
    show_title = "y"
  )
p
```

<img src="man/figures/README-iris-1.png" width="100%" />
