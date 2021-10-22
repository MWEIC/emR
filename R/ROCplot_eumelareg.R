#' Plot ROC curve with youden index and auc.
#'
#' This function plots a default EuMelaReg ROC curve using the package [plotROC()]. By default Youden-Index and AUC are printed and displayed in the graph.
#' Additional ggplot layers can be added to the graph.
#' @param predictions vector containing predicted probabilities obtained from a prediction model.
#' @param labels observed labels of two outcome possibilities. Preferred are binomial values with 0 = control and 1 = case.
#' @param print.auc logical value to decide whether auc should be printed within plot. Default is TRUE.
#' @param print.cutoff logical value to decide whether cutoff should be printed within plot. Default is TRUE.
#' @param cutoff.method method to be used to calculate cutoff. Allowed values are "youden" and "closest.topleft".
#' @param auc.label.x x-coordinate of auc label.
#' @param auc.label.y y-coordinate of auc label.
#' @param cutoff.label.x x-coordinate of cutoff label.
#' @param cutoff.label.y y-coordinate of cutoff label
#' @param textsize argument to define size of axes title, axes text and plot text.
#' @export

ROCplot_eumelareg <- function(predictions, labels, print.auc = TRUE, print.cutoff = TRUE, cutoff.method = c("youden", "closest.topleft"), auc.label.x = 0.7, auc.label.y = 0.1, cutoff.label.x = 0.15, cutoff.label.y = 0.9, textsize = 12){

  df <- data.frame(predictions = predictions, labels = labels)

  # calculate youden index
  rocobj <- pROC::roc(labels, predictions)
  youden_ind <- pROC::coords(rocobj, "best", best.method = cutoff.method)

  #
  rocplot <- ggplot(df, aes(m = predictions, d = labels)) +
    geom_roc(cutoffs.at = youden_ind$threshold, cutoff.labels = "")

  # define labels for auc and cutoff
  if(print.cutoff == TRUE){
    cutoff_label <- paste(round(youden_ind$threshold, 2), " (",round(youden_ind$specificity, 2),";", round(youden_ind$sensitivity, 2), ")", sep = "")
  } else {
    cutoff_label <- ""
  }

  if(print.auc == TRUE){
    auc_label <- paste("AUC:", round(calc_auc(rocplot)$AUC, 3))
  } else {
    auc_label <- ""
  }

  # plot ROC curve
  rocplot +
    xlab("1 - Specificity") +
    ylab ("Sensitivity") +
    scale_x_continuous(breaks = seq(0,1,0.2), guide = ggh4x::guide_axis_minor(), labels = scales::percent, expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,1,0.2), guide = ggh4x::guide_axis_minor(), labels = scales::percent, expand = c(0,0)) +
    theme_bw() +
    geom_abline(lty = 2, color = "red") +
    geom_text(aes(x = auc.label.x, y = auc.label.y), label = auc_label, size = (textsize-1)/2.54) +
    geom_text(aes(x = cutoff.label.x, y = cutoff.label.y), label = cutoff_label, size = (textsize-1)/2.54) +
    theme(axis.title = element_text(size = textsize, face = "bold"),
          axis.text = element_text(size = textsize))

}

