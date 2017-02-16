#' Pretty table for descriptives of categorical variables
#'
#' Uses \code{\link[htmlTable]{htmlTable}} to produce nicely formated tables summarizing a set of categorical variables.
#'
#' @param cat_vars a dataframe of the categorical variables (factors) to include in the table
#' @param var.names an (optional) vector of strings the variable names (will use the column names in cat_vars if none are provided here)
#' @param caption an (optional) caption to add to the table
#' 
#' @examples
#' library(dplyr)
#' warpbreaks %>%
#' select(wool, tension) %>%
#' cat_descriptives_table(var.names=c("Type of wool", "Level of tension"))
#' @export
cat_descriptives_table <- function(cat_vars, var.names = NULL, caption=NULL, show.missing = TRUE){
  stopifnot(require(dplyr), require(tidyr), require(htmlTable))
  
  table <- cat_vars %>% 
    tidyr::gather(factor_key=TRUE) %>% 
    dplyr::mutate(value=dplyr::recode_factor(value, a="a", .missing = "Missing")) %>% 
    dplyr::count(key, value) %>% 
    dplyr::mutate(perc = 100 * round(n/nrow(cat_vars), 3),
                  perc = paste0("(", format(perc, nsmall = 1, trim = TRUE), "%)")) %>% 
    dplyr::ungroup()
  if(!show.missing){
    table <- table %>% 
      dplyr::filter(value != "Missing") 
  }
  if(is.null(var.names)){
    var.names <- unique(table$key)
  } else {
    stopifnot(length(var.names) == length(unique(table$key)))
  }
  n.rgroup <- dplyr::count(table, key)$nn
  htmlTable::htmlTable(dplyr::select(table, -key), 
                       header = c("", "", ""),
                       rnames = FALSE, 
                       align = "lrr",
                       rgroup = var.names, 
                       n.rgroup = n.rgroup,
                       caption=caption)
}

#' Binary vectors
#'
#' Is a general test of whether a vector is coded like a binary variable, with only 0's and 1's. 
#'
#' @param x the vector to test
#' 
#' @examples
#' is.binary(mtcars$vs) # TRUE
#' is.binary(mtcars$gear) # FALSE
#' 
#' # missing values don't prevent it from identifying a vector as binary
#' is.binary(c(mtcars$vs, NA)) # TRUE
#'
#' @export
is.binary <- function(x){
  bin <- all(unique(na.omit(x)) %in% c(0,1))
  return(bin)
}

#' Binary descriptives table
#'
#' @export
bin_descriptives_table <- function(bin_vars, var.names = NULL, header = "Percent above threshold", caption=NULL, show.missing = TRUE, show.n = FALSE, show.n.sucess = FALSE){
  stopifnot(require(dplyr), require(tidyr), require(htmlTable))
  
  # convert factors to numeric
  factors <- data.frame(bin_vars[, sapply(bin_vars, is.factor)])
  colnames(factors) <- colnames(bin_vars)[sapply(bin_vars, is.factor)]
  if(ncol(factors) > 0){
    factors <- mutate_all(factors, funs(as.numeric(.) - 1))
    bin_vars <- cbind(bin_vars[, !sapply(bin_vars, is.factor)], factors)
  }
  
  stopifnot(all(as.matrix(dplyr::summarize_all(bin_vars, is.binary))))
    
  
  table <- bin_vars %>% 
    tidyr::gather(factor_key=TRUE) %>% 
    dplyr::group_by(key) %>% 
    dplyr::summarise_all(funs(prop = mean, count_missing, count_obs, count_sucess = sum), na.rm=TRUE) %>% 
    dplyr::mutate(perc_missing = 100 * round(count_missing/nrow(bin_vars), 3),
                  perc_missing = paste0("(", format(perc_missing, nsmall = 1, trim = TRUE), "%)"),
                  perc = paste0(format(100 * round(prop, 3), nsmall = 1, trim = TRUE), "%")) %>% 
    tidyr::unite("Missing", count_missing, perc_missing, sep = " ") %>% 
    dplyr::ungroup() 
  
    # full table
    table <- table %>% 
      dplyr::select(key, count_sucess, perc,  Missing, count_obs)
    tab_header <- c("n", header, "Missing", "N")
    
    # remove unnecessary columns
    if(!show.missing){
      table <- table %>% 
        dplyr::select(-Missing)
      tab_header <- tab_header[tab_header != "Missing"]
    }
    if(!show.n){
      table <- table %>% 
        dplyr::select(-count_obs)
      tab_header <- tab_header[tab_header != "N"]
    }
    if(!show.n.sucess){
      table <- table %>% 
        dplyr::select(-count_sucess)
      tab_header <- tab_header[tab_header != "n"]
    }
  if(is.null(var.names)){
    var.names <- unique(table$key)
  } else {
    stopifnot(length(var.names) == length(unique(table$key)))
  }
  
  ncol <- 1 + sum(show.missing, show.n, show.n.sucess)
  align <- paste(rep("r", ncol), collapse = "")
  
  
    htmlTable::htmlTable(dplyr::select(table, -key), 
                         header = tab_header,
                         align = align,
                         rnames = var.names, 
                         caption=caption)
}


count_missing <- function(x, ...){
  length(which(is.na(x)))
}

count_obs <- function(x, ...){
  length(which(!is.na(x)))
}

#' Continuous descriptives table
#'
#' @export
cont_descriptives_table <- function(cont_vars, var.names = NULL, caption=NULL, show.missing = TRUE){
  stopifnot(require(dplyr), require(tidyr), require(htmlTable))
  
  table <- cont_vars %>% 
    dplyr::mutate_all(as.numeric) %>% 
    tidyr::gather(factor_key=TRUE) %>% 
    dplyr::group_by(key) %>% 
    dplyr::summarise_all(funs(mean, sd, count_missing), na.rm=TRUE) %>% 
    dplyr::mutate(perc_missing = 100 * round(count_missing/nrow(cont_vars), 3),
                  perc_missing = paste0("(", format(perc_missing, nsmall = 1, trim = TRUE), "%)"),
                  mean = format(round(mean, 2), nsmall = 2, trim = TRUE),
                  sd = format(round(sd, 2), nsmall = 2, trim = TRUE)) %>% 
    tidyr::unite("Missing", count_missing, perc_missing, sep = " ") %>% 
    dplyr::ungroup()
  if(is.null(var.names)){
    var.names <- unique(table$key)
  } else {
    stopifnot(length(var.names) == length(unique(table$key)))
  }
  
  if(show.missing){
    htmlTable::htmlTable(dplyr::select(table, -key), 
                         header = c("Mean", "SD", "Missing"),
                         rnames = var.names, 
                         align = "rrr",
                         caption = caption)
  } else {
    htmlTable::htmlTable(dplyr::select(table, -key, -Missing), 
                         header = c("Mean", "SD"),
                         rnames = var.names, 
                         align = "rr",
                         caption = caption)
  }
}

#' Correlations table, with optional descriptives
#'
#' @export
corr_table <- function(cont_vars, var.names = NULL, caption = NULL, plot = FALSE, show.means = FALSE, digits = 2, stars = FALSE){
  stopifnot(require(dplyr), require(tidyr), require(htmlTable), require(corrr))
  
  if(!is.null(var.names)){
    stopifnot(length(var.names) == length(colnames(cont_vars)))
    colnames(cont_vars) <- var.names
  }
  
  table <- cont_vars %>% 
    correlate(use="pairwise.complete.obs") %>% 
    shave(upper = FALSE) %>% 
    fashion(digits = digits)
  row.names(table) <- paste(1:ncol(cont_vars), row.names(table), sep = ". ")
  # n.rgroup sets how to add horizontal lines to the table (only needed if adding descriptive stats below)
  n.rgroup <- NULL 
  
  if(stars){
    stopifnot(require(Hmisc))
    p_vals <- rcorr(as.matrix(cont_vars), type="pearson")$P 
    p_vals <- ifelse(p_vals < .001, "***", 
                     ifelse(p_vals < .01, "** ",
                            ifelse(p_vals < .05, "*  ",
                                   ifelse(p_vals >= .05, "   ", NA))))
    p_vals <- ifelse(upper.tri(p_vals), p_vals, "")
    
    for(i in 1:ncol(table)){
      table[,i] <- paste0(table[,i], p_vals[,i])
    }
    detach("package:Hmisc", unload=TRUE)
  }
  
  if(plot){
    stopifnot(require(ggplot2))
    
    p <- cont_vars %>% 
      correlate(use="pairwise.complete.obs") %>% 
      rearrange(absolute = FALSE) %>% 
      shave(upper = FALSE) %>% 
      rplot()
    print(p)
  }
  if(show.means){
    means.table <- cont_vars %>% 
      tidyr::gather("key", "value", factor_key = TRUE) %>% 
      dplyr::group_by(key) %>% 
      dplyr::summarize(Mean = round(mean(value, na.rm = TRUE), digits), 
                SD = round(sd(value, na.rm = TRUE), digits), 
                min = round(min(value, na.rm = TRUE), digits), 
                max = round(max(value, na.rm = TRUE), digits)) %>% 
      # convert min and max to Range
      tidyr::unite(Range, min, max, sep = " - ") %>% 
      # transpose table so variables are across the top and each row is a summary stat
      dplyr::select(-key) %>% 
      t()
    
    # add descriptive stats as additional rows below the correlations table
    table <- rbind(table, means.table)
    # use row row grouping in the table
    n.rgroup <- c(ncol(cont_vars), 3)
  }
    htmlTable::htmlTable(table,
                         caption = caption,
                         header = 1:ncol(cont_vars),
                         rgroup = rep("", length(n.rgroup)),
                         n.rgroup = n.rgroup)
}

#' Creates a clean crosstabs table 
#' 
#' Uses \code{\link[htmlTable]{htmlTable}} to make a clean, pretty html-formatted table from a \code{xtabs} object.
#'
#' @param xtab a contingency table with class "xtabs"
#' @param var.names an optional vector of variable names to use in table
#' @param caption an optional caption to include for table
#' 
#' @return An \code{\link[htmlTable]{htmlTable}} table, which will appear in the Viewer. 
#' Display it in your browswer, and then copy-paste it into Word or Libre Office.
#'
#' @examples
#' x <- xtabs(~ cyl + gear, data = mtcars)
#' xtabs_table(x, var.names = c("Number of cylinders", "Number of gears"))
#'
#' @export
xtabs_table <- function(xtab, var.names = NULL, caption = NULL){
  stopifnot(require(dplyr), require(tidyr), require(htmlTable))
  
  if(!is.null(var.names)){
    stopifnot(length(var.names) == length(names(attr(xtab, "dimnames"))))
  } else {
    var.names <- names(attr(xtab, "dimnames"))
  }
  
  table <- as.data.frame(xtab)
  colnames(table) <- c("var1", "var2", "Freq")
  levels1 <- unique(table$var1)
  levels2 <- unique(table$var2)
  
  table <- table %>% 
    tidyr::spread(var2, Freq)
 
  message("Note that you'll need to clean up the footnote to report the chi-squared test in proper APA format")
  htmlTable::htmlTable(dplyr::select(table, -var1),
                       rnames = levels1,
                       header = levels2,
                       rgroup = var.names[1],
                       n.rgroup = length(levels1),
                       cgroup = rep(var.names[2], length(levels2)),
                       n.cgroup = length(levels2),
                       caption = caption,
                       tfoot = paste0("chi-sq = ", round(summary(xtab)$statistic, 2),", df = ", summary(xtab)$parameter, ", p = ", round(summary(xtab)$p.value, 3)))
}
