library(car)

lmfit1 <- glm(wages ~ age + sex + education, data = SLID, family = gaussian)
#Gaussian is same as OLS
summary(lmfit1)

lmfit2 <- lm(wages~ age + sex + education, data = SLID)
summary(lmfit2)
#use ANOVA to compare two models
anova(lmfit1, lmfit2)
aov(lmfit1); aov(lmfit2)

#Generalized Linear models allow reponse variables that have error dis other than normal (Gaussian)
data(warpbreaks)
head(warpbreaks)

#Apply Poisson as family object for ind variables - tension, and independent variable, breaks
#poisson is for # of events occuring during a specific time, with decay, i.e. 10 calls in 1 hour
rs1 = glm(breaks ~ tension, data=warpbreaks, family="poisson")
summary(rs1)
hist(warpbreaks$tension)

#Binomial Model for GLM - each code written as 1 or 0.
head(mtcars$vs)
hist(mtcars$vs)

lm1 <- glm(vs ~ hp + mpg + gear, data = mtcars, family=binomial)
summary(lm1)

####Generalized Additive Model
#used to fit generalized additive models, can be viewed as semiparametric extension of GLM. 
#While GLM holds assumption of linear, GAM fits model on account of local behaviour of data.
library(mgcv)
library(MASS)
attach(Boston)
str(Boston)

fit <- gam(dis ~s(nox)) #dis - distance to 5 empl centers as dep, and NOX as independent variable
summary(fit)
#Visualize GAM
plot(nox, dis)
x = seq(0,1,length = 500)
y = predict(fit, data.frame(nox=x))
lines(x,y,col="red",lwd=2)
#Alternative plot of fit
plot(fit)

#vis.Gam function can produce contour plot views of GAM model predictions
fit2 <- gam(medv ~ crim + zn + crim:zn, data = Boston)
vis.gam(fit2)

#Diagnosing a GAM model
#generate diagnostic plot using gam.check of fitted model
gam.check(fit)