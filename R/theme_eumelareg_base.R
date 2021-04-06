#' An emR function
#'
#' This function defines the base theme for eumelareg functions with longer y-axis ticks and corbel as the default font.
#' @export

theme_eumelareg_base <- function(){
  theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.ticks.length = unit(.20, "cm")
          )
}



