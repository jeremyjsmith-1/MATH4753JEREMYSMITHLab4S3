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
mybiplot <- function(x = double(), k = double()){

  stopifnot(is.double(x))
  stopifnot(is.double(k))

  # Calculate metrics:
  x_bar <- mean(x, na.rm = TRUE)
  stdev <- sd(x, na.rm = TRUE)
  z_scr <- (x - x_bar)/stdev

  # Analyse data for outliers with respect to k
  y <- ifelse(abs(z_scr) < k, "in",
                  ifelse(z_scr > k, "outu", "outl"))

  list(x = x, y = y, k = k)
  structure(.Data = l, class = "math4753biplot")
}
