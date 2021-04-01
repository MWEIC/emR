#' An emR function
#'
#' This function
#' @param data data.frame containing the different strata with one row for each patient
#' @export

sankey_plot_eumelareg <- function(data){

  # calculate frequency table
  df <- as.data.frame(table(data))
  # remove combinations that occur 0 times
  df <- df[df$Freq!= 0,]

  # define number of strata (number of columns of the frequency table minus column "Freq")
  n_strata <- dim(df)[2]-1
  # calculate links between the strata and transform into character values
  links <- data.frame(source = c(sapply(1:(n_strata-1),function(x){paste0(df[,x], "_",x)})),
                      target = c(sapply(2:n_strata,function(x){paste0(df[,x], "_",x)})))
  links$source <- as.character(links$source)
  links$target<- as.character(links$target)

  # vector from 1 til n (number of strata)
  strata <- 1:n_strata
  # matrix with consecutive number pairs (1:2, 2:3, etc.)
  input <- cbind(strata[-length(strata)], strata[-1])
  # applying function calc_sankey_flow over the rows of input to calculate the strata frequencies between number pairs
  res <- mapply(calc_sankey_flow, input[,1], input[,2], MoreArgs = list(data=data))
  # transform mapply results to a list object
  ls <- lapply(1:(n_strata-1),function(x){as.data.frame(res[,x])})
  # bind list into data.frame
  df2 <- rbindlist(ls)

  # remove duplicate links
  links$tmp <- paste0(links$source, links$target)
  links <- links[which(!duplicated(links$tmp)),]
  links$tmp <- NULL

  # combine links with frequency value from df2
  links <- merge(links, df2, by = c("source","target"), all.x = T)
  # define names for nodes
  nodes <- data.frame(name = unique(c(links$source, links$target)))
  # transform links into matched objects
  links$source <- match(links$source, nodes$name) - 1
  links$target <- match(links$target, nodes$name) - 1

  networkD3::sankeyNetwork(Links = links, Nodes = nodes, Source = 'source',
                Target = 'target', Value = 'Freq', NodeID = 'name')
}


