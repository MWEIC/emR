#' Save survival curve generated with \code{survplot_eumelareg}
#'
#' This function saves a default EuMelaReg survival plot (Kaplan-Meier plot) produced with \code{survminer}.
#' @param data dataframe containing survival data.
#' @param LOT Line of treatment. Numerical value, either 1 or 2.
#' @param survival Specify which survival shall be plotted. Allowed values include "PFS" for progression free survival
#' and "OS" for overall survival. Default is "PFS".
#' @param var Grouping variable to compare survival.
#' @param regID Name of column containing registry information. Default is "REGISTR" as defined by SAS output.
#' @param registry Name of EuMelaReg registry. If \code{registry = "EUMELAREG"} data from all registries is plotted.
#' @param path File path where the plot shall be stored. By default a folder "Results/survival_curve" is generated
#' within the working directory and objects are stored in this subfolder.
#' @param ... Additional arguments passed to \code{\link[survminer]{ggsurvplot}}.
#' @export


save_surv_plot_eumelareg <- function(data,LOT,survival = "PFS", var,regID = "REGISTR", registry = "EUMELAREG",path = NULL,...){

  if (is.null(path)) {
    if (!dir.exists(paste("Results/survival_curve/",registry, sep = ""))) {
      dir.create(paste("Results/survival_curve/",registry, sep = ""), recursive = TRUE)
    }
    path <- paste("Results/survival_curve/",registry, sep = "")
  }

  if (ifelse(is.na(stringr::str_extract(var,"\\d$") != LOT),TRUE, stringr::str_extract(var,"\\d$") != LOT)){
    warning("Please check if grouping variable corresponds to the correct line of treatment")
  }

  if (registry != "EUMELAREG"){
    data <-  data[eval(parse(text=regID)) == registry]
    data[[var]] <- droplevels(data[[var]])
  }

  filename <- paste("/survival_curve_",var,"_",registry,"_",survival,LOT,".png", sep = "")
  png(paste(path, filename, sep = ""), units="in", width=12, height=6.2, res=600)
  if(survival == "PFS"){
    p <- survplot_eumelareg(data = data, time = paste("time",LOT,sep=""), status = paste("PFS",LOT,"ZENS",sep=""),
                            var = var,ylab = "Probability of Progression Free Survival",...)
  } else if(survival == "OS"){
    p <- survplot_eumelareg(data = data, time = paste("OS",LOT,sep=""), status = "OSCENS",var = var,
                            ylab = "Probability of Overall Survival",...)
  } else {
    stop("Please specify type of survival")
  }
  print(p)
  dev.off()
}

