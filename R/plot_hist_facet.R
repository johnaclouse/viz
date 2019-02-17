#' Faceted ggplot histograms
#'
#' @name plot_hist_facet
#' @import tidyverse
#' @import ggplot2
#' @importFrom magrittr "%>%"
#' @param data data frame containing features
#' @param bins number of histogram bins
#' @param ncol number of histogram columns
#' @param fct_reorder reorder factors
#' @param fct_rev reverse factor order
#' @param fill color value for geom_hist fill argument
#' @param color color value for geom_hist color argument
#' @param scale value for geom_hist scale argument
#' 
#' Original code by Matt Dancho DS4B 201
#' @return ggplot object containing histograms
#' @export
#'
#' @examples
#' plot_hist_facet(iris)

plot_hist_facet <- function(data, bins = 10, ncol = 5,
                            fct_reorder = FALSE,
                            fct_rev = FALSE, 
                            fill = "#18BC9C", 
                            color = "white",
                            scale = "free") {
  
  data_factored <- data %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    gather(key = key, value = value, factor_key = TRUE) 
  
  if (fct_reorder) {
    data_factored <- data_factored %>%
      mutate(key = as.character(key) %>% as.factor())
  }
  
  if (fct_rev) {
    data_factored <- data_factored %>%
      mutate(key = fct_rev(key))
  }
  
  g <- data_factored %>%
    ggplot(aes(x = value, group = key)) +
    geom_histogram(bins = bins, fill = fill, color = color) +
    facet_wrap(~ key, ncol = ncol, scales = scale) 
    # tidyquant::theme_tq()
  
  return(g)
  
}

