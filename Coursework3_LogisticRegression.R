#importing data and putting it in the varialble titan
titan <- read.csv("P:/ML_and_stats/Week3/titanic.csv", header = TRUE)
attach(titan)                                     #to be able to get objects in the dataset can be accessed by name
dim(titan)                                        #number of rows and coloums
head(titan)                                       #to look at the first rows in the data

#1 fitting a logistic regression model using sex as predictor
Titanic_survival_sex <- glm(Survived ~  Sex, data = titan, family = binomial)
summary(Titanic_survival_sex)

#3 Fit logistic model using predictors sex, age and passenger class
#Using the factor function as Pclass is a categorical variable and not continuous
TitanicSurvivalJack <- glm(Survived ~  Sex + factor(Pclass) + Age, data = titan, family = binomial)
summary(TitanicSurvivalJack)

#diagnostics
set.seed(1)
jitter = rnorm(nrow(titan), sd = 0.08)
plot(TitanicSurvivalJack$fitted.values, Survived + jitter, xlim = 0:1, ylim = c(-0.5,1.5), axes = FALSE, xlab = "Predicted probability", ylab = "", col =adjustcolor("blue",0.2), pch = 16)
axis(1)
axis(2, at = c(0,1), labels = c("0 (Did not survive)" , "1 (Survived)"))

plot(factor(Survived,labels = c ("Didn't survive", "Survived")), TitanicSurvivalJack$fitted.values)