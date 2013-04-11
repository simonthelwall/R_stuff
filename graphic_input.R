getDate <- function() { 
  require(tcltk) 
  tt <- tktoplevel() 
  tkwm.title(tt, "Date selection")
  ds <- tclVar('') 
  
  f1 <- tkframe(tt) 
  tkpack(f1, side='top') 
  tkpack(tklabel(f1, text='dd/mm/yyyy: '), side='left') 
  tkpack(tkentry(f1, textvariable=ds), side='left') 
  
  tkpack(tkbutton(tt, text='Submit', command=function() tkdestroy(tt)), 
         side='right', anchor='s') 
  
  tkwait.window(tt) 
  return(ds=as.Date(tclvalue(ds),format="%d/%m/%Y") ) 
} 

out <- getDate() 
out
