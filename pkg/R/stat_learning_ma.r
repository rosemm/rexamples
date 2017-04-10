library(RefManageR)
bib <- ReadBib("savedrecs_all.bib", check = "error")
# when entries failed to read in because of missing editors, I added: editor = {{RM: unknown}}

# code for suitability for meta-analysis
CodeBibs <- function(bib){
  Nbibs <- length(bib)
  
  all_done <- FALSE
  for(b in 1:Nbibs){
    
    if( is.null(bib[b]$ma_use) & !is.null(bib[b]$abstract) ){
      
      message(paste("article", b, "of", Nbibs)) 
      message(bib[b]$title)
      message(paste("journal:", bib[b]$journal))
      message(bib[b]$abstract)
      
      confirm <- "n"
        while(!grepl("y", confirm)){
          ma_use <- paste("{", readline("Suitable for meta-analysis? (y/n/m) "), "}", sep="")
          confirm <- readline("Confirm? (y/n) ")
          if(!grepl("y", confirm))  message("Please re-enter.")
        }
      
      bib[b]$ma_use <- ma_use
      
      if(!grepl("n", ma_use)) bib[b]$ma_age <- paste("{", readline("What age participants? "), "}", sep="")

      bib[b]$ma_notes <- paste("{", readline("Notes? "), "}", sep="") 
      
      all_done <- grepl("n", readline("Keep going? "))
    } 
    if(all_done==TRUE) break
  }
  return(bib)
}


coded.bib <- CodeBibs(bib) # first time

coded.bib <- CodeBibs(coded.bib) # subsequent times

# what's the current list of articles that should (maybe) be included? (y or m answer to "suitable for meta-analysis?")
for(b in 1:length(coded.bib)){
  if(!is.null(coded.bib[[b]]$ma_use)){
    if( !grepl("n", coded.bib[[b]]$ma_use) ) print(coded.bib[[b]])
  }
}    
