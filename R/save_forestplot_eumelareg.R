save_foresplot_eumelareg <- function(data, LOT, survival,regID,main = NULL, registry = "EUMELAREG",path = NULL,...){
  
  if (is.null(path)) {
    if (!dir.exists(paste("Results/cox_proportional_hazard/",registry, sep = ""))) {
      dir.create(paste("Results/cox_proportional_hazard/",registry, sep = ""), recursive = TRUE)
    }
    path <- paste("Results/cox_proportional_hazard/",registry, sep = "")
  }
  
  if (registry != "EUMELAREG"){
    data <- data[eval(parse(text=regID)) == registry]
  }
  
  varnames <- c(rep("Gender",2), "Age", rep("ECOG", 5), rep("LDH", 3), rep("AJCC Stage", 6), rep("Melanoma subtype",4))
  
  filename <- paste("/coxph_forestplot_",registry,"_",survival,LOT,".png", sep = "")
  png(paste(path, filename, sep = ""), units="in", width=10, height=6, res=600)
  if (survival == "PFS") {
    formula <- paste("Surv(time",LOT,",", "PFS",LOT,"ZENS", ") ~ ", "GENDER + AGE",LOT," + ECOG",LOT, " + LDH",LOT," + Stage",LOT," + MELTYPE", sep = "")
    res.cox <- coxph(as.formula(formula), data = data)
    if (is.null(main)) {
      main <- "Hazard ratio (multivariate) for disease progression (95% CI)"
    }
    p <- forestplot_eumelareg(res.cox, data = data, main = main, varnames = varnames,...)
  } else if (survival == "OS") {
    formula <- paste("Surv(OS",LOT,",", "OSCENS", ") ~ ", "GENDER + AGE",LOT," + ECOG",LOT, " + LDH",LOT," + Stage",LOT," + MELTYPE", sep = "")
    res.cox <- coxph(as.formula(formula), data = data)
    if (is.null(main)) {
      main <- "Hazard ratio (multivariate) for death (95% CI)"
    }
    p <- forestplot_eumelareg(res.cox, data = data,main = main, varnames = varnames,...)
  } else {
    stop("Please specify type of survival")
  }
  print(p)
  dev.off()
}