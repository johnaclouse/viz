#' Helper function for GGally::ggpairs
#'
#' @name plot_ggpairs
#' @import tidyverse
#' @import ggplot2
#' @import GGally
#' @import rlang
#' @importFrom magrittr "%>%"
#' @param data data frame containing features and target
#' @param color column name to use for color aesthetic in pair plots
#' @param density_alpha alpha setting for frequency density plots
#' 
#' @return ggplot object containing pairwise comparisons
#' @export
#'
#' @examples
#' plot_ggpairs(iris)

plot_ggpairs <- function(data,
                         color = NULL,
                         density_alpha = 0.5) {
  color_expr <- rlang::enquo(color)
  if (rlang::quo_is_null(color_expr)) {
    g <- data %>%
      GGally::ggpairs(lower = "blank")
  } else {
    color_name <- quo_name(color_expr)
    g <- data %>%
      GGally:ggpairs(
        mapping = aes_string(color = color_name),
        lower = "blank",
        legend = 1,
        diag = list(continuous = wrap("densityDiag",
                                      alpha = density_alpha))
      ) +
      theme(legend.position = "bottom")
  }
  return(g)
}
