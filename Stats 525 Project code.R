
install.packages("leaps")
library(leaps)
install.packages("MASS")
library(MASS)
install.packages("ALSM")
library(ALSM)
install.packages("caret")
library(caret)
install.packages("scatterplot3d") 
library("scatterplot3d")
install.packages("olsrr")
library("olsrr")
library(ggplot2)
library(gridExtra)



insurance = read.csv(file = "insurance.csv", sep="")
names(insurance) = c("zip","fire","theft","age","income","race","vol","invol")
attach(insurance)


### Correlation Scatterplot
### Useful to figure out which variables are correlated
pairs(insurance)
cor(insurance)

### Scatter Plots of Invol vs all predictor variables
par(mfrow = c(3,3), mar = c(3,3,3,3), pch = 19)
plot(invol~zip)
title("Involuntary Market Activity Variable vs. Zipcode")
plot(invol~fire)
title("Involuntary Market Activity Variable vs. Fires")
plot(invol~theft)
title("Involuntary Market Activity Variable vs. Theft")
plot(invol~age)
title("Involuntary Market Activity Variable vs. Age")
plot(invol~income)
title("Involuntary Market Activity Variable vs. Median Family Income")
plot(invol~race)
title("Involuntary Market Activity Variable vs. Race")
plot(invol~vol)
title("Involuntary Market Activity Variable vs. Vol")

### Boxplots of all variables
par(mfrow = c(3,3), mar = c(3,3,3,3), pch = 19)
boxplot(zip)
title("Boxplot of Zipcodes")
boxplot(fire)
title("Boxplot of Fires")
boxplot(theft)
title("Boxplot of Theft")
boxplot(age)
title("Boxplot of Age")
boxplot(income)
title("Boxplot of Median Income")
boxplot(race)
title("Boxplot of Race")
boxplot(vol)
title("Boxplot of Vol")
boxplot(invol)
title("Boxplot of Invol")




### Descriptive Statistics of all Variables 
#Standard Deviations
summary(insurance)
sd(insurance$zip)
sd(insurance$fire)
sd(insurance$theft)
sd(insurance$age)
sd(insurance$income)
sd(insurance$race)
sd(insurance$vol)
sd(insurance$invol)

#Variances
var(insurance$zip)
var(insurance$fire)
var(insurance$theft)
var(insurance$age)
var(insurance$income)
var(insurance$race)
var(insurance$vol)
var(insurance$invol)

#Anova Table, SSR, MSR, SSE, MSE

anova(zipmodel)
anova(firemodel)
anova(theftmodel)
anova(agemodel)
anova(incomemodel)
anova(racemodel)
anova(volmodel)
anova(involmodel)



##Residual Plots##
par(mfrow = c(3,3), mar = c(4,4,4,4), pch = 19)
plot(modzip$residuals,ylab="Residuals", xlab="Zip Code")
abline(0,0)
plot(modfire$residuals,ylab="Residuals", xlab="Fire")
abline(0,0)
plot(modtheft$residuals,ylab="Residuals", xlab="Theft")
abline(0,0)
plot(modage$residuals,ylab="Residuals", xlab="Age" )
abline(0,0)
plot(modincome$residuals,ylab="Residuals", xlab="Income")
abline(0,0)
plot(modrace$residuals,ylab="Residuals", xlab="Race")
abline(0,0)
plot(modvol$residuals,ylab="Residuals", xlab="Voluntary") 
abline(0,0)

##Sum Residuals##


##qqnorm plots##
par(mfrow = c(3,3), mar = c(4,4,4,4), pch = 19)
qqnorm(modzip$residuals)
qqnorm(modfire$residuals)
qqnorm(modtheft$residuals)
qqnorm(modage$residuals)
qqnorm(modincome$residuals)
qqnorm(modrace$residuals)
qqnorm(modvol$residuals)



### Linear Models of Data

### Stepwise Method
# "zip","fire","theft","age","income","race","vol","invol"
mod_full = lm(invol ~., data=insurance)
mod_stepwise = stepAIC(mod_full, direction = "both", trace = TRUE)
mod_forward = stepAIC(mod_full, direction = "forward", trace = TRUE)
mod_backward = stepAIC(mod_full, direction = "backward", trace = TRUE)

res <- regsubsets(invol ~ ., data=insurance, nbest=3, really.big=T)
summres <- summary(res)
selected <- summres$which # this gives a 1/0 (TRUE/FALSE) for included/excluded predictor variables
par(mfrow=c(1,3))
plot(res,scale="adjr2", main = "Adjusted R2")
plot(res,scale="bic", main = "BIC")
plot(res,scale="Cp", main="Cp")

##Cooks distance
ols_plot_cooksd_bar(mod_full)
cooksd <- cooks.distance(mod_full)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

### Variance Inflation Factors
modzip = lm(zip~fire+theft+age+income+race+vol) 
modfire = lm(fire~zip+theft+age+income+race+vol)
modtheft = lm(theft~zip+fire+age+income+race+vol)
modage = lm(age~zip+fire+theft+income+race+vol)
modincome = lm(income~zip+fire+theft+age+race+vol)
modrace = lm(race~zip+fire+theft+age+income+vol)
modvol = lm(vol~zip+fire+theft+age+income+race)





modincome_no_vol = lm(income ~ zip + fire + theft + age + race)
Rsqmodincome_no_vol = summary(modincome_no_vol)$r.squared
VIFmodincome_no_vol = 1/(1 - Rsqmodincome_no_vol)

Rsqzip = summary(modzip)$r.squared 
Rsqfire = summary(modfire)$r.squared
Rsqtheft = summary(modtheft)$r.squared
Rsqage = summary(modage)$r.squared
Rsqincome = summary(modincome)$r.squared
Rsqrace = summary(modrace)$r.squared
Rsqvol = summary(modvol)$r.squared

VIFzip = 1/(1-Rsqzip)
VIFfire = 1/(1-Rsqfire)
VIFtheft = 1/(1-Rsqtheft)
VIFage = 1/(1-Rsqage)
VIFincome = 1/(1-Rsqincome)
VIFrace = 1/(1-Rsqrace)
VIFvol = 1/(1-Rsqvol)

VIFcolumn = cbind(VIFzip,VIFfire,VIFtheft,VIFage,VIFincome,VIFrace,VIFvol)

mod_that_we_probably_use = update(mod_full, .~. - zip - income - vol)





