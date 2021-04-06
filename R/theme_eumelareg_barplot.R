#' Theme for barplots in EuMelaReg style
#'
#' This function defines the basic graphical representation of barplots for EuMelaReg
#' @param textsize The default textsize used for Barplots
#' @export

theme_eumelareg_barplot <- function(textsize=14){
  theme_eumelareg_base() +
    theme(legend.position = "none",
          axis.line.y.left = element_line(),
          axis.line.x.bottom = element_line(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(face = "bold", size = textsize),
          axis.title.y = element_text(face = "bold", size = textsize),
          axis.text.y = element_text(size = textsize))
}



