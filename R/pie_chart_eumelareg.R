#' Pie Chart in EuMelaReg style
#'
#' This code generates a pie chart to show distribution of variables.
#' @param y Vector with group names (as character or factor)
#' @param text.color Color of the text within the chart to display percentages.
#' @param text.size Size of the text within the chart.
#' @param cols Color scheme for the pie chart. Can be an \code{emr_cols} object or manual scale.
#' @param title Plot title.
#' @export

pie_chart_eumelareg <- function(y, text.color = "white", text.size = 6, cols = NULL, title=NULL){
  df <- group <- value <- NULL
  df <- as.data.frame(table(y)/length(y))
  colnames(df) <- c("group", "value")

  ggplot(df, aes(x="", y=value, fill=group)) +
    geom_bar(stat="identity", width=1, color = "white")+
    coord_polar("y", start=0) +
    geom_text(aes(label = paste0(round(value*100), "%")),
              position = position_stack(vjust = 0.5), color = text.color, size = text.size) +
    labs(x = NULL, y = NULL, fill = NULL, title = title) +
    theme_eumelareg_pie_chart() +
    if (!is.null(cols)) scale_fill_manual(values=cols)
}



