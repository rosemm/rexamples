# library(RefManageR)
# bib <- ReadBib("/Users/TARDIS/Documents/STUDIES/contexts_in_IDS/reports/savedrecs_all.bib", check = "error")

# code for suitability for meta-analysis
code_bibs <- function(bib, dir = getwd(), use_bib_file="ma_bib.csv", return=TRUE, save=!return){
  stopifnot(require(RefManageR))
  
  Nbibs <- length(bib)
  
  if(!use_bib_file %in% list.files(dir)){
    use_bib <- data.frame(author=NULL, year=NULL,title=NULL, journal=NULL, use=NULL, Notes=NULL)
  } else {
    use_bib <- read.csv(file=file.path(dir, use_bib_file), header=TRUE)
  }
  
  coded <- NULL
  
  all_done <- FALSE
  for(b in 1:Nbibs){
    
    if( is.null(bib[b]$ma_use) & !is.null(bib[b]$abstract) ){
      
      message(paste("article", b, "of", Nbibs)) 
      message(bib[b]$title)
      message(paste("journal:", bib[b]$journal))
      message(paste("keywords:", bib[b]$keyword))
      message(bib[b]$abstract)
      
      confirm <- "n"
        while(!grepl(pattern="y", x=tolower(confirm))){
          ma_use <- readline("Type q to quit.\nSuitable for meta-analysis? (y/n/m) ")
          confirm <- readline("Confirm? (y/n) ")
          if(!grepl(pattern="y", x=tolower(confirm)))  message("Please re-enter.")
        }
      
      if(grepl(pattern = "y|m", x=tolower(ma_use))){
        title <- gsub(x=bib[b]$title, pattern = "[{](.*)[]}]", replacement = "\\1") # remove brackets
        title <- gsub(x=title, pattern = "([[:space:]]+)", replacement = " ") # remove extra white space
        
        this_bib <- data.frame(author=paste(bib[b]$author, collapse = "; "), 
                               year=as.numeric(gsub(x=bib[b]$year, pattern = ".*([[:digit:]]{4}).*", replacement = "\\1")),
                               title=title, 
                               journal=gsub(x=bib[b]$journal, pattern = "[{](.*)[]}]", replacement = "\\1"), 
                               use=ma_use, 
                               Notes=readline("Notes? "))
        use_bib <- rbind(use_bib, this_bib)
      } 
      
      if(grepl(pattern="q", x=tolower(ma_use))){
        all_done <- grepl(pattern="n", x=readline("Keep going? "))
      } else {
        # keep track of which records have already been coded
        coded <- c(coded, b)
      }
      
    } 
    if(all_done==TRUE) {
      if(save){
        write.csv(use_bib, file=file.path(dir, use_bib_file), row.names = FALSE)
        bib_in_progress <- bib[-coded]
        save(bib_in_progress, file=file.path(dir, "ma_bib_in_progress.RData"))
      }
      break
    }
  }
  if(return) return(list(bib=bib, use_bib=use_bib))
}


# code_bibs(bib, dir="/Users/TARDIS/Documents/STUDIES/contexts_in_IDS/reports", return=FALSE, save=TRUE)
