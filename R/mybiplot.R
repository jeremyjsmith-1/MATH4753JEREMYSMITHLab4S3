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
  inout <- ifelse(abs(z_scr) < k, "in",
                  ifelse(z_scr > k, "outu", "outl"))

  # Display the table for inspection
  result_table <- data.frame(
    x = x,
    z_score = round(z_scr,1),
    inout = inout,
    k = k)

  #Provide Histogram of data with inout shading
  plot <- ggplot2::ggplot(result_table, ggplot2::aes(x = x, fill = inout)) +
    ggplot2::geom_histogram(bins = 30, color = "black") +
    ggplot2::labs(
      title = paste0("Histogram of biplot with k = ", k),
      subtitle = "Jeremy Smith")

  #Calculate descriptive statistics for inout sets
  sumry <- result_table |>
    dplyr::group_by(inout) |>
    dplyr::summarise(
      mu = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      IQR = quantile(x, 0.75, na.rm = TRUE) - quantile(x, 0.25, na.rm = TRUE),
      n = dplyr::n()
    )

  list(result_table = result_table, plot = plot, summary = sumry)
}
