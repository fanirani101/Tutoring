# attach both packages and load the data set
if (!require("AER")) install.packages("AER")
if (!require("sandwich")) install.packages("sandwich")
if (!require("MASS"))install.packages("MASS")
if (!require("stargazer"))install.packages("stargazer")
if (!require("mosaic"))install.packages("mosaic")
library(AER)
library(MASS) 
library(sandwich)
library(stargazer)
library(mosaic)
data("Boston")

write.csv(Boston,"C:/Users/Kitten/Downloads/stats3/Boston.csv", row.names = FALSE)

# obtain an overview over the data set
summary(Boston)
# or
str(Boston)
# or
head(Boston)
# estimate the simple regression model
bh_mod <- lm(medv ~ lstat, data = Boston)
# print the summary to the console
coeftest(bh_mod, vcov. = vcovHC)
# compute the coefficient of determination
R2_res <- summary(bh_mod)$r.squared
# compute the confidence intervals for individual coefficents
confint(bh_mod)

# conduct the regression
bh_mult_mod <- lm(medv ~ lstat + crim + age, data = Boston)

# obtain a robust coefficient summary
coeftest(mod, vcov. = vcovHC)

# compute the confidence intervals for individual coefficents
confint(bh_mult_mod)

# compare both coefficients of determination
R2_unres <- summary(mod)$r.squared
R2_unres < R2_res

# correction Factor
n <- nrow(bh_mult_mod$model)
k <- ncol(bh_mult_mod$model)-1
CF <- (n-1)/(n-k-1)

# obtain both R^2 and the adj. R^2
summary(bh_mult_mod)$r.squared
summary(bh_mult_mod)$adj.r.squared

# check that the adj. R^2 can be computed as stated above
1 - (1 - summary(bh_mult_mod)$r.squared) * CF == summary(bh_mult_mod)$adj.r.squared


# run a regression of medv on all remaining variables in the Boston data set
full_mod <- lm(medv ~., data = Boston)

# obtain a robust summary of the coefficients
coeftest(full_mod, vcov. = vcovHC, type = "HC1")

# compute the confidence intervals for individual coefficents
confint(full_mod)

# what is the R^2 of the model?
summary(full_mod)$adj.r.squared

# this solution is a bit technical but efficient

# loop estimation of models
l <- list()
for (i in 1:13) {
  d <- Boston[, -i]
  # save each adj. R^2 as a list entry in l
  l[[i]] <- summary(lm(medv ~., data=d))$adj.r.squared 
}

# assign variable names to the list entries
names(l) <- names(Boston[, 1:13]) 

# select the variable whose omission leads to the highest improvement in adj. R^2
which.max(l) # 7th column this is "age"

# hence a model that fits the data better is
better_model <- lm(medv ~., data = Boston[, -7])

# execute the function on the model object and provide both linear restrictions 
# to be tested as strings
linearHypothesis(bh_mult_mod, c("age=0"))
linearHypothesis(bh_mult_mod, c("age=0"), white.adjust = "hc1")

linearHypothesis(bh_mult_mod, c("age=0", "crim=0"))
linearHypothesis(bh_mult_mod, c("age=0", "crim=0"), white.adjust = "hc1")

linearHypothesis(full_mod, c("age=0", "indus=0", "chas=0"))
linearHypothesis(full_mod, c("age=0", "indus=0", "chas=0"), white.adjust = "hc1")

confidenceEllipse(bh_mult_mod, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("age", "crim"),
                  main = "95% Confidence Set")

confidenceEllipse(bh_mult_mod, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("age", "lstat"),
                  main = "95% Confidence Set")

confidenceEllipse(bh_mult_mod, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("crim", "lstat"),
                  main = "95% Confidence Set")

