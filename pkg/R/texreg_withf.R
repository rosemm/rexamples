texreg_withf <- function(l, ...){
  if( is.list(l) ){
    for(m in 1:length(l)){
      f <- summary(l[[m]])$fstatistic[[1]]
      numdf <- summary(l[[m]])$fstatistic[[2]]
      dendf <- summary(l[[m]])$fstatistic[[3]]
      
      tr <- texreg::extract(l[[m]])
      tr@gof <- c(tr@gof, c(f=f, numdf=numdf, dendf=dendf))
      tr@gof.names <- c(tr@gof.names, c("F", "num df", "res df"))
      tr@gof.decimal <- c(tr@gof.decimal, TRUE, FALSE, FALSE)
      
    }
  } else {
    f <- summary(l)$fstatistic[[1]]
    numdf <- summary(l)$fstatistic[[2]]
    dendf <- summary(l)$fstatistic[[3]]
    
    tr <- texreg::extract(l)
    tr@gof <- c(tr@gof, c(f=f, numdf=numdf, dendf=dendf))
    tr@gof.names <- c(tr@gof.names, c("F", "num df", "res df"))
    tr@gof.decimal <- c(tr@gof.decimal, TRUE, FALSE, FALSE)
    
    texreg(l=tr, ...)
  }
}