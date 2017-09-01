#' Identify and plot spatial clusters from point data
#'
#' This function identifies spatial clusters from spatial coordinates (longitude
#' / latitude) using Kmeans clustering. The resulting object is a
#' \code{spatial_clusters} object which can be plotted with several options.
#'
#' @export
#'
#' @rdname spatial_clusters
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @return
#'
#' The function returns a \code{spatial_cluster} object, which is a list with
#' the following items:
#'
#' \itemize{
#'  \item \code{lonlat}: a matrix of lon/lat for individual points
#'  \item \code{clusters_coords}: a matrix of lon/lat/size for clusters
#'  \item \code{clusters}: cluster membership for the point locations
#'  \item \code{voronoi}: voronoi tesselation for the clusters
#' }
#'
#'
#' @param x A \code{matrix} oar \code{data.frame} with two columns indicating
#'   longitude and latitude.
#'
#' @param k An \code{integer} indicating the number of clusters to be found.q
#'
#' @examples
#'
#' \dontrun{
#' if (require("outbreaks")) {
#'
#' lonlat <- ebola_sim$linelist[, c(10, 11)]
#' head(lonlat)
#' nrow(lonlat)
#'
#'
#' res <- spatial_clusters(lonlat, 30)
#' res
#' plot(res)
#'
#' plot(res, show_density = TRUE, show_tiles = TRUE,
#'      cl_alpha = .2, cl_col = "blue")
#'
#' }
#' }

spatial_clusters <- function(x, k){
  x <- check_clean_lonlat(x)

  ## find clusters and corresponding voronoi tesselation

  clusters <- stats::kmeans(x, k)
  clusters_coords <- data.frame(clusters$centers,
                                number = clusters$size)

  voronoi <- deldir::deldir(clusters$centers[, 1],
                            clusters$centers[, 2])

  out <- list(lonlat = x,
              clusters_coords = clusters_coords,
              clusters = factor(clusters$cluster),
              voronoi = voronoi)

  class(out) <- c("spatial_clusters", "list")
  return(out)
}





#' @export
#'
#' @rdname spatial_clusters

print.spatial_clusters <- function(x, ...) {
  n <- nrow(x$lonlat)
  k <- nrow(x$clusters)
  cat("\n  /// spatial_clusters object")
  cat("\n  // $lonlat: [matrix]", "lon/lat for",
      n, "point locations")
  cat("\n  // $clusters_coords: [matrix]", "lon/lat/size for",
      k, "clusters")
  cat("\n  // $clusters: [factor]", "cluster membership for",
      n, "point locations")
  cat("\n  // $voronoi: [deldir]", "voronoi tesselation for",
      k, "clusters")
  cat("\n")
}





#' @export
#'
#' @rdname spatial_clusters
#'
#' @param x A \code{spatial_clusters} object.
#'
#' @param show_clusters A logical indicating if clusters should be plotted.
#'
#' @param show_points A logical indicating if individual points should be
#'   plotted.
#'
#' @param show_tiles A logical indicating if Voronoi tesselation should be
#'   plotted.
#'
#' @param show_density A logical indicating if case density should be plotted.
#'
#' @param pt_col The color to be used for individual points.
#'
#' @param pt_alpha The transparency level to be used for individual points.
#'
#' @param cl_col The color to be used for clusters.
#'
#' @param cl_alpha The transparency level to be used for clusters.
#'
#' @param dens_alpha The transparency level to be used for case density.
#'
#' @param dens_bw The bandwidth to be used for case density estimation; larger
#'   values correspond to a wider kernel.
#'
#' @param leg_posi The (x,y) position of the legend, on a relative scale from 0
#'   to 1.
#'
#' @param zoom The zoom level to be used for \code{ggmap}.

plot.spatial_clusters <- function(x,
                                  show_clusters = TRUE,
                                  show_points = FALSE,
                                  show_tiles = FALSE,
                                  show_density = FALSE,
                                  pt_col = "red", pt_alpha = .3,
                                  cl_col = "red", cl_alpha = .7,
                                  dens_alpha = 0.3, dens_bw = 0.005,
                                  leg_posi = c(.1, .8),
                                  zoom = 13, ...) {

  map <- ggmap::get_map(as.data.frame(x$lonlat), zoom = zoom)
  base <- ggmap::ggmap(map) + ggplot2::labs(x = "", y = "")
  out <- base

  if (show_density) {
    heat_colors <- ggplot2::scale_fill_gradientn(
      "Case density", colors = c("white", "gold", "red", "darkred"),
      guide = FALSE)

    out <- out + ggplot2::stat_density_2d(
      data = as.data.frame(x$lonlat),
      ggplot2::aes(fill = ..level..),
      geom = "polygon",
      alpha = dens_alpha,
      h = dens_bw) +
       heat_colors
  }

  if (show_clusters) {
    out <- out + ggplot2::geom_point(
      data = as.data.frame(x$clusters_coords),
      ggplot2::aes(x = lon, y = lat, size = number),
      alpha = cl_alpha, color = cl_col) +
      ggplot2::scale_size("Number of cases", range = c(1,15)) +
      ggplot2::theme(legend.position = leg_posi)
  }

  if (show_points) {
     out <- out + ggplot2::geom_point(
      data = as.data.frame(x$lonlat),
      ggplot2::aes(x = lon, y = lat),
      alpha = pt_alpha, color = pt_col)
  }

  if (show_tiles) {
    out <- out + ggplot2::geom_segment(
      data = x$voronoi$dirsgs,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      linetype = 1,
      color = "black")
  }


  return(out)
}
