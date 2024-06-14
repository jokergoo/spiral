# Visualize Data on Spirals <img width="150" src="https://user-images.githubusercontent.com/449218/121876090-723e0900-cd09-11eb-8d0d-82fbeeb83997.png" align="right">


[![R-CMD-check](https://github.com/jokergoo/spiral/workflows/R-CMD-check/badge.svg)](https://github.com/jokergoo/spiral/actions)
[![CRAN](https://www.r-pkg.org/badges/version/spiralize)](https://cran.r-project.org/web/packages/spiralize/index.html)
[![CRAN](https://cranlogs.r-pkg.org/badges/grand-total/spiralize)](https://cran.r-project.org/web/packages/spiralize/index.html)


## Features

The package **spiralize** visualizes data along an [Archimedean spiral](https://en.wikipedia.org/wiki/Archimedean_spiral).
It has two major advantages for visualization:

1. It is able to visualize data with very long axis with high resolution.
2. It is efficient for time series data to reveal periodic patterns.

## Documentation

https://jokergoo.github.io/spiralize/

## Citation

Zuguang Gu, et al., spiralize: an R package for Visualizing Data on Spirals, Bioinformatics, 2021. https://doi.org/10.1093/bioinformatics/btab778

## Install

The package is available on CRAN and can be installed by:

```r
install.packages("spiralize")
```

If you want the latest version, install it directly from GitHub:

```r
library(devtools)
install_github("jokergoo/spiralize")
```

## Usage

It includes three steps:

1. initialize the spiral,
2. add a track,
3. add graphics to the track.

Step 2 and 3 can be applied multiple times to allow multiple-track visualization along the spiral.

The code for making spiral plot looks likes follows:

```r
library(spiralize)
spiral_initialize(...)
spiral_track(...)
spiral_points(...)
...
```

## Graphics

Complex plots are baiscally constructed from simple graphics. Here there are following low-level graphics functions:

- `spiral_points()`
- `spiral_lines()`
- `spiral_rect()`
- `spiral_segments()`
- `spiral_polygon()`
- `spiral_bars()`
- `spiral_text()`
- `spiral_axis()`
- `spiral_yaxis()`
- `spiral_raster()`

Particularlly, horizon chart is very suitable to put on the spiral, thus there is one function for this:

- `spiral_horizon()`

Spiral plot can also visualize dendrograms with large number of leaves, thus there are following two functions:

- `spiral_dendrogram()`
- `spiral_phylo()` 


## Examples

1. Difference of **ggplot2** daily downloads to the mean of the current year (2015-2021). Each loop contains 52 weeks so that same weeks in different years locate at the same angle in the polar coordinates.

![](https://user-images.githubusercontent.com/449218/122206221-671de100-cea1-11eb-823e-6c48de851667.png)


2. A phylogenetic life tree with 50645 species. 

![](https://user-images.githubusercontent.com/449218/123804978-fbe6fc80-d8ed-11eb-93d8-d3f83d552dde.png)

3. The spiral COVID-19 Shiny app

![](https://user-images.githubusercontent.com/449218/154753102-d66b3588-eca1-471b-bdfe-2c147ed257f5.gif)


## License

MIT @ Zuguang Gu
