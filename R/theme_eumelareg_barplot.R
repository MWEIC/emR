#' An emR function
#'
#' This function defines the basic graphical representation of barplots for EuMelaReg
#' @param textsize The default textsize used for Barplots
#' @examples ggplot(mpg, aes(class, hwy)) +
#'             geom_bar(stat = "summary", fun = "mean", color = "black") +
#'             theme_eumelareg_barplot()
#' @export

theme_eumelareg_barplot <- function(textsize=14){
  theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.position = "none",
          axis.line.y.left = element_line(),
          axis.line.x.bottom = element_line(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(face = "bold", size = textsize),
          axis.ticks.length.y = unit(.25, "cm"),
          axis.title.y = element_text(face = "bold", size = textsize),
          axis.text.y = element_text(size = textsize))
}



