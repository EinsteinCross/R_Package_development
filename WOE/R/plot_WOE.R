#' Plot Weight of evidence (WOE)
#' 
#' This function plots Weight of evidence.
#' @param data           input data frame
#' @param var            variable name in quotes
#' @param target         target variable name in quotes
#' @param n_bins         Number of bins
#' @param output         location to save the plot
#' @param marker         short name for the data in quotes
#' @param plot_y_limits  Limits of y-axis on plot. Example: c(0,6) means displayed y-axis lies between 0 and 6.
#' @return The function returns WOE plot for the given variable. If there are missing records, they will be classified
#' in one bin and that bin is not displayed in the plot. The plot gets saved in output.
#' @author Hanish M Kusumanchi
#' @export 
#' @examples
#' location <- here::here()
#' plot_WOE(titanic_train, "Age", "Survived", 4, location, "Titanic", c(0,0.3))

plot_WOE <- function(data, var, target, n_bins, output, marker, plot_y_limits){
  
  require(dplyr)
  require(rlang)
  require(ggplot2)
  
  varQ <- quo(!! sym(var))
  targetQ <- quo(!! sym(target))
  
  WOE_plot <- 
    
    data %>% 
    
    select(
      !!varQ, 
      !!targetQ
    ) %>%
    
    mutate(
      bin = 
        ntile(data %>% dplyr::select(!!varQ), n_bins)
    ) %>%
    
    group_by(
      bin
    ) %>% 
    
    summarise(
      lbound = min(!!varQ),
      ubound = max(!!varQ),
      bin_n_events = sum(!!targetQ),
      bin_n_non_events = n() - bin_n_events
    ) %>% 
    
    mutate(
      n_tot_events = sum(data %>% select(!!targetQ)),
      n_tot_non_events = nrow(data) - n_tot_events,
      bin_prop_tot_events = bin_n_events/n_tot_events,
      bin_prop_tot_non_events = bin_n_non_events/n_tot_non_events,
      WOE = log(bin_prop_tot_events/bin_prop_tot_non_events)
    ) %>% 
    
    ggplot(aes(x = bin, y = WOE)) +
    geom_point(size = 2, color = "red") +
    geom_line(size = 1, color = "red") +
    scale_y_continuous(limits = plot_y_limits) +
    theme_bw() +
    labs(x = "Bin number", y = "WOE",
         title = paste("Variation of WOE with bin", "( Data:", marker, "& Variable:", var, "& Bins:", n_bins, ")")) +
    theme(panel.border = element_rect(color = "grey"),
          plot.title = element_text(size = 10))
  
  save(WOE_plot, file = paste(output, marker, "_", var, "_", n_bins, "bins_WOE_plot.jpg", sep = ""))
  return(WOE_plot)
}
