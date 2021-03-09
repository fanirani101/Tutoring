myData <- read.csv('http://static.lib.virginia.edu/statlab/materials/data/hierarchicalRegressionData.csv')

cor(myData[1], myData[2])
cor(myData[1], myData[4])
cor(myData[1], myData[5])

# Build models
m0 <- lm(happiness ~ 1, data=myData)  # to obtain Total SS
m1 <- lm(happiness ~ pets, data=myData)  # Model 1
m2 <- lm(happiness ~ age + gender + friends, data=myData)  # Model 2
m3 <- lm(happiness ~ age + gender + friends + pets, data=myData)  # Model 3

anova(m0)
anova(m1)
anova(m1,m2,m3)
summary(m0)
summary(m1)
summary(m2)
summary(m3)