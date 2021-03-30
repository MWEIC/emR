#' An emR function
#'
#' This function defines the basic graphical representation of survival curves (Kaplan-Meier Plots) for EuMelaReg
#' @param textsize The default textsize used for survival curves.
#' @export

theme_eumelareg_surv_plot <- function(textsize =12){
  theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line.y.left = element_line(),
          axis.line.x.bottom = element_line(),
          axis.text = element_text(size = textsize),
          axis.title = element_text(face = "bold", size = textsize))
}
