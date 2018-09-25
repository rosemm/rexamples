#' @export
plot_coeffs <- function(model, coef.names=NULL, standardize=TRUE){
  stopifnot(require(ggplot2))
  
  if(standardize){
    d <- lapply(model$model, scale)
    model <- lm(model$terms, data=d)
    ylab = "Standardized Regression Coefficients"
  } else {
    ylab = "Regression Coefficients"
  }
  
  b_names <- row.names(summary(model)$coefficients)
  
  betas <- data.frame(b=b_names, 
                      est=summary(model)$coefficients[,1], 
                      se=summary(model)$coefficients[,2], stringsAsFactors = FALSE) %>% 
    dplyr::mutate(b = factor(b, levels=b_names)) %>% 
    dplyr::filter(b != "(Intercept)") %>% 
    dplyr::mutate(bar=se*qt(0.975, df=model$df.residual),
                  lo=est-bar, hi=est+bar) 
  
  p1 <- ggplot(betas, aes(y=est, x=b)) + 
    geom_bar(stat = "identity") + 
    geom_errorbar(aes(ymin=lo, ymax=hi), width=.2) + 
    theme_classic() +
    labs(y="Standardized Regression Coefficients", x=NULL)
  ggsave(filename="regression_all_bar.png", plot=p1, path="graphs", width = 5, height = 3, units = "in")
  p2 <- ggplot(betas, aes(y=est, x=b)) + 
    geom_point(size=2) + 
    geom_errorbar(aes(ymin=lo, ymax=hi), width = 0) + 
    geom_hline(yintercept=0, lty=2) + 
    theme_classic() +
    labs(y="Standardized Regression Coefficients", x=NULL) + 
    coord_flip()
  ggsave(filename="regression_all_lines.png", plot=p2, path="graphs", width = 5, height = 3, units = "in")
}