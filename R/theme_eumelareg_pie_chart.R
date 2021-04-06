#' Theme for pie charts in EuMelaReg style
#'
#' This function defines the basic graphical representation of barplots for EuMelaReg
#' @export

theme_eumelareg_pie_chart <- function(){
  theme_eumelareg_base() +
     theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          plot.title = element_text(hjust = 0.5, color = "#666666"))
}

