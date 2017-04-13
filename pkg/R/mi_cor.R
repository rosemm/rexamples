#' Add significance stars to a correlations table
#' 
#' @param cor A dataframe wit the correlations, as resulting from \code{\link[corrr]{correlate}}
#' @param stars A dataframe or matrix with the same dimensions as \code{cor} (omitting the rowname column) of significance stars
#' 
#' @export
add_cor_stars <- function(cor, stars){
  if("rowname" %in% colnames(cor)){
    rows <- cor$rowname
    cor <- dplyr::select(cor, -rowname)
  } else {
    rows <- NULL
  }
  
  for(r in 1:nrow(cor)){
    for(c in 1:ncol(cor)){
      if(cor[r,c] != "" & cor[r,c] != 1) cor[r,c] <- paste0(cor[r,c], stars[r,c])
    }
  }
  if(!is.null(rows)){
    cor <- mutate(cor, rowname = rows)  
  }
  return(cor)
}

#' Correlations table from multiply imputed datasets
#' 
#' @param data Either the output from \code{\link[Amelia]{amelia}} or a dataframe with a column \code{Imputation_} to mark each imputed dataset, as is produced by the imputation procedure in SPSS
#' @param vars An optional character vector of the columns to include in the correlations table. If omited, all columns will be used (except the \code{Imputation_} column when \code{format}  = "SPSS)
#' @param format Either "Amelia" or "SPSS", indicating which format \code{data} is in.
#' @param means A logical indicating whether or not to append means and SEs to the bottom of the correlations table. Default is TRUE.
#' @param caption A string to be passed to \code{\link[htmlTable]{htmlTable}} with the caption for the table. 
#' @param method A string indicating the method for calculating correlations, to be passed to \code{\link[stats]{cor}}, either "pearson", "kendall", or "spearman". Default is "pearson".
#' 
#' @export
mi_cor <- function(data, vars=NULL, format=c("Amelia", "SPSS"), means = TRUE, caption = "Correlations", method = "pearson"){
  
  stopifnot(require(Amelia), require(htmlTable), require(dplyr), require(tidyr), require(corrr))
  
  if(format == "Amelia") {
    m <- data$m
    if(is.null(vars)) vars <- data$orig.vars 
  } else if(toupper(format) == "SPSS") {
    m <- length(which(unique(data$Imputation_) != 0))
    if(is.null(vars)) vars <- colnames(data)[colnames(data) != "Imputation_"] 
  } else {
    warning("format must be either 'Amelia' or 'SPSS'")
    stop()
  }
  
  r.out <- vector("list", length(vars)); names(r.out) <- vars
  r.se.out <- vector("list", length(vars)); names(r.se.out) <- vars
  mean.out <- matrix(NA, nrow=5, ncol=length(vars)); colnames(mean.out) <- vars
  mean.se.out <- matrix(NA, nrow=5, ncol=length(vars)); colnames(mean.se.out) <- vars
  
  for(i in 1:m) {
    if(format == "Amelia") {
      this.data <- data$imputations[[i]]
    } else if(format == "SPSS") {
      this.data <- dplyr::filter(data, Imputation_ == i)
    }
    
    # descriptives
    mean.out[i,] <- this.data %>% 
      dplyr::select(one_of(vars)) %>% 
      summarize_all(mean) %>% 
      as.matrix()
    mean.se.out[i,] <- this.data %>% 
      dplyr::select(one_of(vars)) %>% 
      summarize_all(function(x) sqrt(stats::var(x)/length(x))) %>% 
      as.matrix()
    cors <- this.data %>% 
      dplyr::select(one_of(vars)) %>% 
      cor(method = method) %>% 
      as.data.frame() 
    
    for(var in vars){
      r.out[[var]] <- rbind(r.out[[var]], t(cors[row.names(cors) == var]))
    }
    r.se <- matrix(NA, nrow=length(vars), ncol = length(vars)); colnames(r.se) <- vars; rownames(r.se) <- vars
    for(r in 1:length(vars)){
      var2 <- vars[r]
      for(c in 1:length(vars)){
        var1 <- vars[c]
        if(var1 != var2) {
          temp <- cor.test(this.data[[var1]], this.data[[var2]])
          r.se[r,c] <- unname(sqrt((1 - temp$estimate^2)/temp$parameter))
        }
      }
    } # end of r.se.out loops
    for(var in vars){
      r.se.out[[var]] <- rbind(r.se.out[[var]], t(r.se[row.names(r.se) == var]))
    }
  } # end of imputations for loop
  
  combined.means <- mi.meld(q=mean.out, se=mean.se.out)
  
  temp.r <- vector("list", length(vars)); names(temp.r) <- vars
  for(i in 1:length(vars)){
    temp.r[[i]] <- mi.meld(q = r.out[[i]], se = r.se.out[[i]])
    # replace regular averages with avergaes using Fisher's r-to-z
    temp.r[[i]]$q.mi <- apply(r.out[[i]], 2, ave_r)
  }
  # reformat correlation output into two matrices
  # cor matrix
  combined.r <- vector("list", 2); names(combined.r) <- c("cor", "se")
  cor <- matrix(NA, ncol=length(vars), nrow = length(vars))
  colnames(cor) <- vars
  row.names(cor)<- vars
  # se matrix
  se <- matrix(NA, ncol=length(vars) , nrow = length(vars))
  colnames(se) <- vars
  rownames(se) <- vars
  # reformat from temp.r into combined.r
  for(var in vars){
    cor[row.names(cor) == var, ] <- temp.r[[var]]$q.mi
    se[row.names(se) == var, ] <- temp.r[[var]]$se.mi
  }
  combined.r$cor <- cor
  combined.r$se <- se
  
  # means and correlations
  cor <- as.data.frame(combined.r$cor)
  cor$rowname <- colnames(cor)
  class(cor) <- c("cor_df", "tbl_df", "tbl", "data.frame")
  
  # add stars to correlations
  t <- combined.r$cor/combined.r$se
  p.vals <- 2*pt(abs(t), lower.tail=FALSE, nrow(this.data)-2)
  stars <- ifelse(p.vals < .001, "***", 
                  ifelse(p.vals < .01, "** ", 
                         ifelse(p.vals < .05, "*  ", "   ")))
  
  cor.table <- cor %>% 
    corrr::shave(upper = FALSE) %>% 
    corrr::fashion() %>% 
    add_cor_stars(stars)
  
  row.names(cor.table) <- paste(1:length(vars), row.names(cor.table), sep = ". ")
  means.table <- data.frame(Mean=as.vector(combined.means$q.mi), SE=as.vector(combined.means$se.mi)) %>% 
    t() %>% 
    round(3)
  table <- rbind(cor.table, means.table)
  # use row row grouping in the table
  n.rgroup <- c(length(vars), 2)
  
  htmlTable::htmlTable(table,
                       caption = caption,
                       header = 1:length(vars),
                       rgroup = rep("", length(n.rgroup)),
                       n.rgroup = n.rgroup)
}