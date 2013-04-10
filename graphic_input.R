require(tcltk)
Sys.Date()

dateDialog<-function(){
  datevar<-tclVar("")
  
  # Create a variable to keep track of the state of the dialog window:
  # done = 0; If the window is active
  # done = 1; If the window has been closed using the OK button
  # done = 2; If the window has been closed using the Cancel button or destroyed
  done <- tclVar(0)
  
  tt <- tktoplevel()
  tkwm.title(tt, "Date selection")
  newdate <- tkentry(tt, textvariable=datevar)
  
  reset <- function(){
    tclvalue(datevar)<-""
  }
  reset.but <- tkbutton(tt, text="Reset", command=reset)
  
  submit<-function(){
    results <- as.Date(tclvalue(newdate), format="%d/%m/%Y")
    tclvalue(done) <- 1
    #    tkmessageBox(message=d)
  }
  submit.but <- tkbutton(tt, text="submit", command=submit)
  
  quit.but <- tkbutton(tt, text="quit", 
                       command=function(){
                         tclvalue(done) <- 2
                       })
  
  tkgrid(tklabel(tt, text="Please enter date in ddmmyyyy format"), columnspan=3, pady = 10)
  tkgrid(tklabel(tt, text="Date:"), newdate, pady = 10, padx = 10)
  tkgrid(quit.but, reset.but, submit.but, pady = 10, padx = 10)
  tkfocus(tt)
  tkwait.variable(done)
  if(tclvalue(done)!=1){
    results <- NULL
  }
  tkdestroy(tt)
  return(results)
}

dateDialog()
