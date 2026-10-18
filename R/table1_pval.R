#' Print table1 with pvalues
#'
#' This function calculates statistics and prints results in a table generated with [table1()]. For categorical data
#' chi² test is used, for numerical data Welch ANOVA is conducted
#' @param data data.frame or data.table containing patient characteristics and demographics
#' @param strat strata to group by
#' @param vars character vector specifying columns that shall be included in the table
#' @inheritParams table1::table1
#' @inheritParams rstatix::anova_test
#' @param ... additional arguments passed on to [table1()] function.
#' @export

table1_pval <- function(data, strat, vars, footnote = NULL, white.adjust = TRUE, ...) {
  if (!is.factor(data[[strat]])) {
    data[[strat]] <- factor(data[[strat]])
  }
  if (anyNA(data[[strat]])) {
    warning("Missing values in the grouping variable are excluded from the table and from statistical tests.")
    data <- data[!is.na(data[[strat]]), , drop = FALSE]
    data[[strat]] <- droplevels(data[[strat]])
  }
  pvalue <- function(x, name, ...) {
    y <- unlist(x, use.names = FALSE)
    g <- factor(rep(names(x), times = lengths(x)))
    g <- droplevels(g)
    if (length(unique(g)) < 2L) {
      p <- NA_real_
    } else if (is.numeric(y)) {
      dat <- data.frame(y = y, g = g)
      p <- suppressWarnings(
        rstatix::anova_test(dat, formula = y ~ g, white.adjust = white.adjust)$p
      )
    } else {
      p <- suppressWarnings(chisq.test(table(y, g))$p.value)
    }
    c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
  }
  formula <- as.formula(paste("~", paste(vars, collapse = " + "), "|", strat))
  table1(
    formula, data,
    extra.col = list(`P-value` = pvalue),
    render.missing = NULL,
    footnote = footnote,
    ...
  )
}


