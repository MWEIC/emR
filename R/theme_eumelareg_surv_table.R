#' An emR function
#'
#' This function defines the basic graphical representation of the table below survival curves for EuMelaReg, showing
#' the number of patients at risk at a given time.
#' @export

theme_eumelareg_surv_table <- function(){
  theme_eumelareg_base() +
    theme(legend.position = "none",
          title = element_text(face ="bold"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())
}
