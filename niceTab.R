#data(infert)
# Use length of dimensions to correctly format rows and columns?
niceTab <- function(v1, v2, data, percs.by = "row", chisq = FALSE){
  arguments = as.list(match.call())
  #print(arguments)
  v1 = eval(arguments$v1, data)
  v2 = eval(arguments$v2, data)
  # get the basic table
  t <- table(v1,v2)
  row.names <- rownames(t)
  col.names <-colnames(t)
  if(percs.by == "row"){
    row.sums <- apply(t, 1, sum)
    # Display
    cat("Cross-tabulation of", arguments$v1, " by ", arguments$v2, "\n")
    cat(formatC("", width = 20), formatC(col.names, width = 10), "\n")
    for(i in 1:dim(t)[1]){
      row.pc <- (t[i, ]/row.sums[i])*100
      cat(formatC(row.names[i], width = 10), formatC("n", width = 10), formatC(t[i,], width = 10), "\n")
      cat(formatC("", width = 10),formatC("%", width = 10), formatC(row.pc, width = 10), "\n")
      cat(formatC("", width = 10), rep("-----------", times = dim(t)[2]+1), "\n")
    }
  } else if (percs.by == "column") {
    col.sums <- apply(t, 2, sum)
    #Display
    cat("Cross-tabulation of", arguments$v1, " by ", arguments$v2, "\n")
    cat(formatC("", width = 10), formatC(col.names, width = 10), formatC("", width = 10),  "\n")
    cat(formatC("", width = 10), formatC(col.names[1], width = 10),formatC("", width = 10), formatC(col.names[2], width = 10), "\n")
    for(i in 1:dim(t)[1]){
      col.pc <- (t[i, ]/col.sums[i])*100
      cat(formatC(row.names[i], width = 10), formatC(t[i,], width = 10), formatC(col.pc, width = 10), "\n")
      cat(formatC("", width = 10), rep("-----------", times = dim(t)[2]+1), "\n")
    }
  }
  else 
    cat("Please provide either 'row' or 'column' for perc.by, thanks.")
  
}


niceTab(induced, case, infert, percs.by = "row")
niceTab(induced, case, infert, percs.by = "column")
