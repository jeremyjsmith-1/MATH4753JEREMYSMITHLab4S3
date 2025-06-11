#' Analyse attribute for outliers with respect to k
#'
#' @param x A numeric vector of continuous values.
#' @param k A numeric constant indicating the number of standard deviations to define outliers.
#'
#' @return A list containing:
#' \describe{
#'   \item{result_table}{A data frame with original values, z-scores, and in/out classification.}
#'   \item{plot}{A histogram ggplot object.}
#'   \item{summary}{A summary table with statistics grouped by in/out classification.}
#' }
#'
#' @importFrom stats sd median quantile
#' @importFrom dplyr group_by summarise n .data
#' @export
#'
#' @examples
#' mybiplot(rnorm(100), 2)
print.math4753biplot <- function(x, ...){

  stopifnot(is.double(x))
  stopifnot(is.double(k))

  #Provide Histogram of data with inout shading
  plot <- ggplot2::ggplot(result_table, ggplot2::aes(x = x, fill = inout)) +
    ggplot2::geom_histogram(bins = 30, color = "black") +
    ggplot2::labs(
      title = paste0("Histogram of biplot with k = ", k),
      subtitle = "Jeremy Smith")
    )

  list(result_table = result_table, plot = plot, summary = sumry)
}
