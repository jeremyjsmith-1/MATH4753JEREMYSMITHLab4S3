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
