require(tcltk)
Sys.Date()

dateDialog<-function(){
  datevar<-tclVar("")
  
  tt <- tktoplevel()
  tkwm.title(tt, "Date selection")
  newdate <- tkentry(tt, textvariable=datevar)
  
  reset <- function(){
    tclvalue(datevar)<-""
  }
  reset.but <- tkbutton(tt, text="Reset", command=reset)
  
  submit<-function(){
    d <- as.Date(tclvalue(datevar), format="%d/%m/%Y")
#    tkmessageBox(message=d)
  }
  submit.but <- tkbutton(tt, text="submit", command=submit)
  
  quit.but <- tkbutton(tt, text="quit", 
                       command=function(){
                         q(save="no")
                         tkdestroy(tt)
                       })
  tkgrid(tklabel(tt, text="Please enter date in ddmmyyyy format"), columnspan=3, pady = 10)
  tkgrid(tklabel(tt, text="Date:"), newdate, pady = 10, padx = 10)
  tkgrid(quit.but, reset.but, submit.but, pady = 10, padx = 10)
  tkwait.window(tt)
  tkdestroy(tt)
  return(e=d)
}

dateDialog()
