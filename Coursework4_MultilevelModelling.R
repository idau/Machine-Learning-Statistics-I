#coursework4

#installing and taking a look at the data
library(ggplot2)
library(dplyr)
library(lattice)
library(moderndive)
library(skimr)
library(lme4)
install.packages("gapminder")
library(gapminder)
attach(gapminder)
str(gapminder)                          #taking a look at the data

#1 Simple linear regression, lifeExp ~ gdpPercap
model1 <- lm(lifeExp ~ gdpPercap)
summary(model1)

#plotting model - see that the data does not follow a linear path
d <- gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm", se= FALSE)
d + labs(x = "GDP per Capita", y = "Life Expectancy", title = "Model 1: GDP per Capita as predictor for Life Expectancy")

#investigating the data - see that gdp per capita is not normally distributed
hist(gdpPercap)                        #only this is included in report
par(mfrow=c(2,2))           
plot(model1)

#do a logtrans of the function to see if it improves - looks better
hist(log(gdpPercap))                                #included in report           
gapminder <- gapminder %>%
  mutate(log_gdp = log(gdpPercap))
model3 <- lm(formula = lifeExp ~ log(gdpPercap))
summary(model3)
par(mfrow=c(2,2))
plot(model3)                                        #to check the model with transformed data further

#plotting model with log transformation
d <- gapminder %>%
  ggplot(aes(x = log_gdp, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm", se= FALSE)
d + labs(x = "log(GDP per Capita)", y = "Life Expectancy", title = "Model with log transformation of GDP per Capita")


#2 Plotting to check if the fitted line will vary if continent is included
p <- gapminder %>%                                  #separating by continent
  ggplot(aes(x = gdpPercap, y = lifeExp, colour = as.factor(continent))) +
  geom_point() +
  geom_smooth(method = "lm", se= FALSE)
p + labs(x = "GDP per Capita", y = "Life Expectancy", title = "Model 2: Grouped by continent")

#3 Fit a random intercept model with 'lifeExp' as dependent variable and 'log(gdpPercap)' as predictor
rand_int_model <- lmer(lifeExp ~ log(gdpPercap) + (1|continent), REML = FALSE)      
summary(rand_int_model)

rand_int_model2 <- lmer(lifeExp ~ gdpPercap + (1|continent), REML = FALSE)  #without logtrans for figure    
predlifeexp <-fitted(rand_int_model2)                                 
xyplot(predlifeexp ~ gdpPercap , gapminder, 
       groups=continent, main="Random intercept model grouped by continent",
       xlab="GDP per Capita",
       ylab="Life Expectancy",
       auto.key=list(space= 'right', title='Continent', cex.title=1,
                     lines=TRUE, points=FALSE), 
       type = c("p", "smooth"))

#4 Fit a random slope and intercept model with 'lifeExp' as dependent variable and 'log(gdpPercapita)' as predictor
rand_slopint_model<-lmer(lifeExp ~ log(gdpPercap) + (1 + log(gdpPercap)|continent), REML = FALSE)
summary(rand_slopint_model)

rand_slopint_model2<-lmer(lifeExp ~ gdpPercap + (1 + gdpPercap|continent), REML = FALSE) #without logtrans for figure
predlifeexp2 <-fitted(rand_slopint_model2)                                 
xyplot(predlifeexp2 ~ gdpPercap , gapminder, 
       groups=continent, main="Random slope and intercept model grouped by continent",
       xlab="GDP per Capita",
       ylab="Life Expectancy",
       auto.key=list(space= 'right', title='Continent', cex.title=1,
                     lines=TRUE, points=FALSE), 
       type = c("p", "smooth"))

