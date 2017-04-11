# library(RefManageR)
# bib <- ReadBib("/Users/TARDIS/Documents/STUDIES/contexts_in_IDS/reports/savedrecs_all.bib", check = "error")

# Results: 1,208
# (from Web of Science Core Collection)
# You searched for: TOPIC: (context) AND TOPIC: (infants)
# Refined by: WEB OF SCIENCE CATEGORIES: ( PSYCHOLOGY DEVELOPMENTAL ) AND DOCUMENT TYPES: ( ARTICLE )
# Timespan: All years. Indexes: SCI-EXPANDED, SSCI, ESCI.

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
    
    if( !is.null(bib[b]$abstract) ){
      
      message(paste("article", b, "of", Nbibs)) 
      message(bib[b]$title)
      message(paste("journal:", bib[b]$journal))
      message(paste("keywords:", bib[b]$keyword))
      message(bib[b]$abstract)
      
      confirm <- "n"
        while(!grepl(pattern="y", x=tolower(confirm))){
          ma_use <- readline("Type q to quit.\nSuitable for meta-analysis? (y/n/m) ")
          confirm <- readline("Confirm? (y/n) ")
          if(!grepl(pattern ="^y|n|m" , x=tolower(ma_use))) confirm <- "n"
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
        keep_going <- readline("Keep going? ")
        all_done <- grepl(pattern="n", x=tolower(keep_going))
        if(grepl(pattern="y", x=tolower(keep_going))) {
          # if user decides to keep going, step back one to re-evaluate the previous article
          b <- b - 1
        } 
      } else {
        # keep track of which records have already been coded
        coded <- c(coded, b)
      }
      
    } else {
      # if abstract is not available for review, add the article to the use_bib dataframe for futher evaluation
      title <- gsub(x=bib[b]$title, pattern = "[{](.*)[]}]", replacement = "\\1") # remove brackets
      title <- gsub(x=title, pattern = "([[:space:]]+)", replacement = " ") # remove extra white space
      
      this_bib <- data.frame(author=paste(bib[b]$author, collapse = "; "), 
                             year=as.numeric(gsub(x=bib[b]$year, pattern = ".*([[:digit:]]{4}).*", replacement = "\\1")),
                             title=title, 
                             journal=gsub(x=bib[b]$journal, pattern = "[{](.*)[]}]", replacement = "\\1"), 
                             use="m", 
                             Notes="Abstract not available.")
      use_bib <- rbind(use_bib, this_bib)
      # keep track of which records have already been coded
      coded <- c(coded, b)
    }
    if(all_done==TRUE) {
      if(save){
        write.csv(unique(use_bib), file=file.path(dir, use_bib_file), row.names = FALSE)
        bib_in_progress <- bib[-coded]
        save(bib_in_progress, file=file.path(dir, "ma_bib_in_progress.RData"))
      }
      break
    }
  }
  if(return) return(list(bib=bib, use_bib=use_bib))
}


# code_bibs(bib, dir="/Users/TARDIS/Documents/STUDIES/contexts_in_IDS/reports", return=FALSE, save=TRUE)
