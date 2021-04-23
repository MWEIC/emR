library(htmlTable)
library(Gmisc)

getT1Stat <- function(data,strata, varname, digits=1){
  data <- as.data.frame(data)
  getDescriptionStatsBy(data[, varname], 
                        strata, 
                        add_total_col="last",
                        show_all_values=TRUE, 
                        hrzl_prop=FALSE,
                        statistics=FALSE, 
                        html=TRUE, 
                        header_count = TRUE,
                        digits=digits,
                        continuous_fn=describeMedian)
}

PFS_surv <- add_median_survival(dat_response1, time = "time1", status = "PFS1ZENS", var = "BRAF")
OS_surv <- add_median_survival(dat_response1, time = "OS1", status = "OSCENS", var = "BRAF")
med_surv <- cbind(PFS_surv, OS_surv)
med_surv <- t(med_surv)
rownames(med_surv) <- c("Median PFS (95% CI)", "Median OS (95% CI)")



table_data <- list()

# Get the basic stats
table_data[["Best response"]] <- getT1Stat(data = dat_response1, strata = dat_response1$BRAF,"BESTRES1")
table_data[["ORR"]] <- getT1Stat(data = dat_response1, strata = dat_response1$BRAF,"ORR1")
table_data[["DCR"]] <- getT1Stat(data = dat_response1, strata = dat_response1$BRAF,"DCR1")
table_data[["Survival"]] <- med_surv



rgroup <- c()
n.rgroup <- c()
output_data <- NULL
for (varlabel in names(table_data)){
  output_data <- rbind(output_data, 
                       table_data[[varlabel]])
  rgroup <- c(rgroup, 
              varlabel)
  n.rgroup <- c(n.rgroup, 
                nrow(table_data[[varlabel]]))
}

tmp <-sapply(colnames(output_data), gsub, pattern = "No. ", replacement = "(N = ")
tmp <- gsub(",",".",tmp)
colnames(output_data) <- sapply(tmp, paste, ")", sep = "")

fn_res <- "CR: complete response. PR: partial response. SD: stable disease. PD: progressive disease. 
            ORR: overall response rate. DCR: disease control rate. PFS: Progression-free survival. OS: overall survival"

output_data_style <- addHtmlTableStyle(output_data, css.table = "font-family: calibri")
htmlTable(output_data_style, align="cccc",
          rgroup=rgroup, n.rgroup=n.rgroup, 
          rgroupCSSseparator="", 
          rowlabel="", 
          tfoot=fn_res, 
          ctable=TRUE)  


