#' Plot Weight of evidence (WOE)
#' 
#' This function plots Weight of evidence.
#' @param data           input data frame
#' @param var            variable name in quotes
#' @param target         target variable name in quotes
#' @param n_bins         Number of bins
#' @param output         location to save the plot
#' @param marker         short name for the data in quotes
#' @return The function returns WOE plot for the given variable. If there are missing records, they will be classified
#' in one bin and that bin is not displayed in the plot. The plot gets saved in output.
#' @author Hanish M Kusumanchi
#' @export 
#' @examples
#' location <- here::here()
#' plot_WOE(titanic_train, "Age", "Survived", 4, location, "Titanic")

plot_WOE <- function(data, var, target, n_bins, output, marker){
  
  require(ggplot2)
  
  WOE_table <- calc_WOE(data, var, target, n_bins, output, marker, print_result = 1)
  
  WOE_plot <- 
    WOE_table %>% 
    ggplot(aes(x = bin, y = WOE)) +
    geom_point(size = 2, color = "red") +
    geom_line(size = 1, color = "red") +
    theme_bw() +
    labs(x = "Bin number", y = "WOE",
         title = paste("Variation of WOE with bin", "( Data:", marker, "& Variable:", var, "& Bins:", n_bins, ")")) +
    theme(panel.border = element_rect(color = "grey"),
          plot.title = element_text(size = 10))
  
  ggsave(filename = paste(output, marker, "_", var, "_", n_bins, "bins_WOE_plot.jpg", sep = ""), plot = WOE_plot)
  return(WOE_plot)
}
