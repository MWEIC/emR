#' Save a list of plots generated with \code{save_surv_plot_eumelareg}
#'
#' This function saves a list of default EuMelaReg survival plots (Kaplan-Meier plot) produced with \code{survminer}.
#' The default are 4 plots (1 for each Line of treatment and survival type pair).
#' @param data dataframe containing survival data.
#' @param var1 Grouping variable at first line of treatment.
#' @param var2 If different value for second line of treatment (e.g. ECOG or LDH) please specify here.
#' @param ... Additional arguments passed on to \code{\link[survminer]{ggsurvplot}}.
#' If NULL var1 is recycled (e.g. for GENDER).
#' @export

save_surv_plot_list_eumelareg <- function(data,var1,var2=NULL,...){
  if (is.null(var2)) {
    var2 <- var1
  }
  input <- data.frame(LOT= rep(c(1,2),2),
                      var = rep(c(var1,var2),2),
                      surv = c("PFS", "PFS","OS", "OS"))
  mapply(save_surv_plot_eumelareg, LOT=input[,1], var= input[,2], survival = input[,3],
         MoreArgs = list(data=data,  palette = "jco", ...))
}
