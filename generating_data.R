# making some toy data sets for examples

N <- 20 # sample size

# with Lickert scale
resp1 <- sample(c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly Agree"), N, replace=T)
resp2 <- sample(c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly Agree"), N, replace=T)
resp3 <- sample(c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly Agree"), N, replace=T)
resp4 <- sample(c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly Agree"), N, replace=T)
resp5 <- sample(c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly Agree"), N, replace=T)

Lickert.data <- data.frame(resp1, resp2, resp3, resp4, resp5, Subj=1:N)

