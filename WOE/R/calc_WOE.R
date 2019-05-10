#' Weight of evidence (WOE) calculation
#' 
#' This function calculates Weight of evidence. Mathematically, it is equal to the natural logarithm of proportion of
#' events to the proportion of non-events in a particular bin/category. 
#' @param data         input data frame
#' @param var          variable name in quotes
#' @param target       target variable name in quotes
#' @param n_bins       Number of bins
#' @param output       location to save final result
#' @param marker       short name for the data in quotes
#' @param print_result 0/1. Default value is set to 0, which means the final result will not be displayed.
#' @return The function returns a table that contains the WOE value for each bin only when print_result is 1.
#' Otherwise, the function saves the table in output. The table contains the following columns.
#' \itemize{
#' \item Bin                     : The bin number;
#' \item lbound                  : The minimum value of var in the bin;
#' \item ubound                  : The maximum value of var in the bin;
#' \item bin_n_events            : Number of events in the bin;
#' \item bin_n_non_events        : Number of non-events in the bin;
#' \item n_tot_events            : Number of total events in the data;
#' \item n_tot_non_events        : Number of total non-events in the data;
#' \item bin_prop_tot_events     : Proportion of events in the bin to the total events;
#' \item bin_prop_tot_non_events : Proportion of non-events in the bin to the total non-events;
#' \item WOE                     : Weight of evidence value.
#' }
#' @author Hanish M Kusumanchi
#' @seealso \url{https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html} used as reference for coding
#' @seealso \url{https://github.com/r-lib/rlang/issues/116} used as reference for coding
#' @export 
#' @examples
#' location <- here::here()
#' calc_WOE(titanic_train, "Age", "Survived", 4, location, "Titanic", print_result = 1)

calc_WOE <- function(data, var, target, n_bins, output, marker, print_result = 0){
  
  require(dplyr)
  require(rlang)
  
  varQ <- quo(!! sym(var))
  targetQ <- quo(!! sym(target))
  
  WOE_table <- 
    
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
    )
  
  if(print_result == 1){return(WOE_table)} 
  else {save(WOE_table, file = paste(output, marker, "_", var, "_WOE_table.rda", sep = ""))}
}