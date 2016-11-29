#' Simple effects tests for factorial ANOVAs
#'
#' Slices an interaction by levels of one factor, \code{by}, testing for the simple main effect of the other factor \code{test} at each level of \code{by}.
#' This is different from simply splitting the data by \code{by} and then running one-way ANOVAs on each split because \code{simple_effects} uses the Mean Sq residual from the full model.
#'
#' @param outcome the outcome variable for the models (dependent variable)
#' @param test the factor to get simple main effects for
#' @param by the factor to split the models by
#' @param data a data frame
#' 
#' @return An object with classes 'anova' and 'data.frame' that includes tests for the main effect of \code{test} within each level of \code{by}, using the Mean Sq residual from the full model.
#'
#' @examples
#' # simple main effect of tension at each type of wool
#' simple_effects(data = warpbreaks, outcome = "breaks", by = "wool", test = "tension")
#' # simple main effect of wool at each level of tension
#' simple_effects(data = warpbreaks, outcome = "breaks", by = "tension", test = "wool")
#'
#' @export
simple_effects <- function(outcome, test, by, data){
  model <- eval(substitute(lm(dv ~ iv1 * iv2 , data=data), list(dv = as.name(outcome), iv1 = as.name(test), iv2 = as.name(by))))
  df.res <- anova(model)[4,1] # Save df for the effect as a new variable
  SSW <- anova(model)[4,2]
  MSW <- anova(model)[4,3] # Save the MSW (in the 4th row, 3rd column) from the source table as a new variable
  
  models <- by(data=data, INDICES=data[[by]], FUN=function(x) eval(substitute(anova(lm(dv ~ iv , data=x)), list(dv = as.name(outcome), iv = as.name(test)))))
  
  simple.effects <- data.frame(Df=NULL, 'Sum Sq' = NULL, 'Mean Sq' = NULL, 'F value' = NULL, p = NULL)
  
  for(by.var in unique(data[[by]])){
    df.num <- models[[by.var]][1,1]
    SS <- models[[by.var]][1,2]
    MS <- models[[by.var]][1,3]
    F.stat <- MS/MSW
    p <- pf(q=F.stat, df1=df.num, df2=df.res, lower.tail = FALSE) 
    # add to simple effects data frame
    simple.effects <- rbind(simple.effects, data.frame(Df=df.num, 'Sum Sq'=SS, 'Mean Sq' = MS, 'F value' = F.stat, p = p))
  }
  # Add error/resid info at the bottom
  simple.effects <- rbind(simple.effects, data.frame(Df=df.res, 'Sum Sq'=SSW, 'Mean Sq' = MSW, 'F value' = NA, p = NA))
  
  row.names(simple.effects) <- c(paste("Simple effect of", test, "at", by, "=", unique(data[[by]])), "Residuals from full model")
  class(simple.effects) <- c("anova", "data.frame")
  attr(simple.effects, "heading") <- c("Analysis of Variance Table\nSimple Effects\n", "Response: breaks")
return(simple.effects)
}

