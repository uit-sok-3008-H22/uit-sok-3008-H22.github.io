
#' POE5 - Chapter 16
rm(list=ls())

#' Data definition file:
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/transport.def")

# Obs:   21 
# 
# autotime	commute time via auto, minutes
# bustime		commute time via bus, minutes
# dtime		=(bus time - auto time)/10, 10 minute units
# auto		= 1 if auto chosen

library(mosaic)

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/transport.rdata"))

names(transport)
head(transport)

summary(transport)
tally(~auto, data=transport) # frequency
tally(~auto, data=transport, format="percent")
gf_histogram(~auto, data=transport)


# linear probability Model

LPM <- lm(auto ~ dtime, data = transport)
summary(LPM)

confint(LPM)

#' We estimate that if travel times by public transportation and automobile
#' are equal, so that DTime =0, then the probability of a person choosing
#' automobile travel is 0.4848, close to 50-50, with 95% interval estimates
#' of [0.34, 0.63]
#' 
#' We estimate that, holding all else constant, an increase of 10 minutes
#' in the difference in travel time,increasing public transportation 
#' travel time relative to automobile travel time, increases the probability
#' of choosing automobile travel by 0.07, with a 95% interval estimates
#' of [0.05, 0.09], which seems relatively preise. 

#' The fitted model can be used to estimate the probability of 
#' automobile travel for any commuting time differentials. 
#' 
#' For Example, if dtime=0, no comutting time differentils 
f <- makeFun(LPM) 
f(dtime=0)
f(0)

#' If dtime =1, a 10 minute longer commute by public transportation
#' we estimate the probability of automobile travel to be 0.5551. 
f(dtime=1)
f(1)


#' Probit model
?glm
p1 <- glm(auto ~ dtime, x=TRUE, family = binomial(link = "probit"), data = transport)
summary(p1)

#' The negative sign of the intercept implies that when commuting times
#' by bus and auto are equal so that dtime =0, individuals have a bias 
#' against driving to work, relative to public transportation. 

#' The estimated probability of a person choosing to drive to work when 
#' dtime = 0, is: 
f <- makeFun(p1) 
f(dtime=0)
f(0)

#' The positive sign of b2 indicates that an increase in public 
#' transportation travel time, relative to auto travel time, increase 
#' the probability that an individual will choose to drive to work, and 
#' this coeff is statistically significant. 

confint(p1) # confidence interval on parameters



#' Plot the predicted probabilities as a function of dtime
require(rockchalk) || {install.packages("rockchalk"); require(rockchalk)}
predictOMatic(p1) # predicted probabilities
predictOMatic(p1, interval="confidence")

plotCurves(p1, plotx = "dtime")
plotCurves(p1, plotx = "dtime", opacity=80, col="red", interval="confidence") # Wald CI

# erer::maTrend - Plot the predicted probabilities as a function of dtime
require(erer) || {install.packages("erer"); require(erer)}
dp=maBina(w = p1, x.mean = TRUE, rev.dum = TRUE)
tdp <- maTrend(q = dp, nam.c = "dtime", simu.c = FALSE)
tdp
plot(tdp)

# Finding the marginal effect at dtime=2 
#' (i.e., assuming travel via public transportation 
#' takes 20 minutes longer than auto travel)
f(2) 
qnorm(f(2)) # qnorm calculates the inverse of the cdf
dnorm(qnorm(f(2))) # then calculating the dnorm of the inverse gives you the pdf
dnorm(qnorm(f(2)))*coef(p1)[2] # finally, the marginal effect

#' An 10-minutes increase in the travel time via public
#' transportation increases the probability of travel via auto by
#' approximately 0.1037, given that taking the bus already requires 
#' 20 minutes more travel time than driving. 

g <- function(x) {dnorm(qnorm(f(x)))*coef(p1)[2]}
g(2)
curve(g(x), -10,10, main="Plot of the marginal effect of dtime", xlab="dtime") 
segments(2,0,2,g(2), col="blue", lty=2)
segments(2,g(2),-10,g(2), col="red", lty=2)

#' Average marginal effect
#' <http://www.rdocumentation.org/packages/erer/functions/maBina>
maBina(w = p1, x.mean = TRUE, rev.dum = TRUE) # calculated at the mean of a variable 
maBina(w = p1, x.mean = FALSE, rev.dum = TRUE) # AME, POE4, p. 594, calculated as the mean of a each data point

#' <http://www.rdocumentation.org/packages/mfx/functions/probitmfx>
require(mfx) || {install.packages("mfx"); require(mfx)}
probitmfx(formula=auto ~ dtime, data=transport)  # calculated at the mean of a variable 
probitmfx(formula=auto ~ dtime, atmean = FALSE, data=transport) # AME, POE4, p. 594, calculated as the mean of a each data point


#' Logit model
l1 <- glm(auto ~ dtime, family = binomial(link = "logit"), data = transport)
summary(l1)

car::compareCoefs(p1,l1)
coef(l1)[2]/coef(p1)[2]




# Flip coding on auto
transport <- transport %>% mutate(inv.auto = ifelse(auto==0,1,0))
il1 <- glm(inv.auto ~ dtime, family = binomial(link = "logit"), data = transport)

car::compareCoefs(l1,il1)


rm(list=ls())

######################################
#' Coke
#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/coke.def")
# Obs:   1140 individuals
# 
# coke       	=1 if coke chosen, =0 if pepsi chosen
# pr_pepsi        price of 2 liter bottle of pepsi
# pr_coke         price of 2 liter bottle of coke
# disp_pepsi      = 1 if pepsi is displayed at time of purchase, otherwise = 0
# disp_coke       = 1 if coke is displayed at time of purchase, otherwise = 0
# pratio          price coke relative to price pepsi

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/coke.rdata"))

#logit <- glm(coke~I(pr_coke/pr_pepsi)+disp_coke+disp_pepsi, family = binomial(link = "logit"), data = coke)
logit <- glm(coke~pratio+disp_coke+disp_pepsi, family = binomial(link = "logit"), data = coke, x=TRUE)
summary(logit)

library(mosaic)
# CIs using profiled log-likelihood
confint(logit)
# CIs using standard errors
confint.default(logit)

# Wald hypothesis test, POE4, p. 597
# <http://www.rdocumentation.org/packages/aod/functions/wald.test>
require(aod) || {install.packages("aod");require(aod)}
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 2:4) # overall significance (except intercept)

probit <- glm(coke~pratio+disp_coke+disp_pepsi, family = binomial(link = "probit"), data = coke)
summary(probit)

require(multcomp)
summary(glht(probit, linfct = ("disp_coke+disp_pepsi = 0"))) # POE4, p. 597

wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 3:4) # Joint hypothesis, disp_coke=0 & disp_pepsi=0

library(stargazer)
stargazer(probit,logit, type = "text", intercept.bottom = FALSE)

library(rockchalk)
plotCurves(logit, plotx = "pratio")
plotCurves(logit, plotx = "pratio", opacity=80, col="red", interval="confidence") # Wald CI
plotCurves(logit, plotx = "pratio", modx = "disp_pepsi")
abline(v=mean(coke$pratio), col="red")
plotCurves(logit, plotx = "pratio", modx = "disp_coke")
abline(v=mean(coke$pratio), col="red")

require(erer)
maBina(w = logit, x.mean = TRUE, rev.dum = TRUE)
library(mfx)
logitmfx(formula=coke~pratio+disp_pepsi+disp_coke, data=coke)

# Predicted probabilities
plotCurves(logit, plotx = "pratio", modx = "disp_pepsi")
abline(v=mean(coke$pratio), col="red")

f <- makeFun(logit) 
f(pratio=mean(coke$pratio), disp_pepsi = 1, disp_coke = mean(coke$disp_coke))
f(pratio=mean(coke$pratio), disp_pepsi = 0, disp_coke = mean(coke$disp_coke))

P1=f(pratio=mean(coke$pratio), disp_pepsi = 1, disp_coke = mean(coke$disp_coke))
P0=f(pratio=mean(coke$pratio), disp_pepsi = 0, disp_coke = mean(coke$disp_coke))
P1-P0

plotCurves(logit, plotx = "pratio", modx = "disp_pepsi")
abline(v=mean(coke$pratio))
abline(h=P1, col="red")
abline(h=P0, col="blue")

# What is the effect of disp_pepsi when pratio is 1.5?

# A closer look at elasticities in logit models
p <- function(l) {1/(1+(1/exp(-l)))}
p(2)

g <- function(l) {exp(-l)/(1+exp(-l))}
g(2)

h <- function(l) {1/(1+exp(l))}
h(2)

i <- function(l) {1/(1+exp(-l))}
1-i(2)

x <- seq(-2,2, length.out = 100)
curve(p, -2,2)
lines(x,g(x), col="red")
lines(x,h(x), col="blue", add=TRUE)
lines(x,1-i(x), col="green", add=TRUE)

rm(x,p,g,h,i)

logit
# Elasticity as a function of pratio
e <- function(pratio,disp_coke,disp_pepsi) {1/(1+exp(coef(logit)[1]+pratio*coef(logit)[2]+disp_coke*coef(logit)[3]+disp_pepsi*coef(logit)[4]))*pratio*coef(logit)[2]}
summary(coke$pratio)
#pratio=(pr_coke/pr_pepsi)

# No advertising
curve(e(x,0,0), min(coke$pratio), max(coke$pratio), ylim=c(-4,0), xlab = "pratio", ylab = "elasticity",
      main = "Plot of the pratio elasticity")
# Coke advertising
curve(e(x,1,0), min(coke$pratio), max(coke$pratio), add=T, col="blue") # Own advertising make choice of coke more inelastic
# Pepsi advertising
curve(e(x,0,1), min(coke$pratio), max(coke$pratio), add=T, col="red") # Competitor advertising make choice of coke more inelastic
# Both advertise
curve(e(x,1,1), min(coke$pratio), max(coke$pratio), add=T, col="green")
legend("topright", legend=c("No advertising", "Coke advertising", "Pepsi advertising","Both advertise"),
       col=c("black","blue","red", "green"), lty=c(1,1,1,1))

# At equal price, pepsi advertising makes choice of coke more elastic
e(1,1,0)
e(1,1,1)

# Elasticity as a function of pratio, ver. 2
e2 <- function(pratio,disp_coke,disp_pepsi) {(1-f(pratio,disp_coke,disp_pepsi))*pratio*coef(logit)[2]}
e2(1,1,0)
e2(1,1,1)

# Estimate a logit model with individual prices
names(coke)
logit2 <- glm(coke~pr_coke+pr_pepsi+disp_coke+disp_pepsi, family = binomial(link = "logit"), data = coke, x=TRUE)
summary(logit2)

g <- makeFun(logit2)

# Own price elasticity
e3 <- function(pr_coke,pr_pepsi,disp_coke,disp_pepsi) {(1-g(pr_coke,pr_pepsi,disp_coke,disp_pepsi))*pr_coke*coef(logit2)[2]}
curve(e3(x,mean(coke$pr_pepsi),0,0), min(coke$pr_coke), max(coke$pr_coke), ylim=c(-2,0), xlab = "pr_coke", main="Coke own price elasticity") 
curve(e3(x,mean(coke$pr_pepsi),1,0), min(coke$pr_coke), max(coke$pr_coke), add=T, col="red") # Own advertising make choice of coke more inelastic
# Substitute price elasticity
e4 <- function(pr_coke,pr_pepsi,disp_coke,disp_pepsi) {(1-g(pr_coke,pr_pepsi,disp_coke,disp_pepsi))*pr_pepsi*coef(logit2)[3]}
curve(e4(mean(coke$pr_coke),x,0,0), min(coke$pr_pepsi), max(coke$pr_pepsi), ylim=c(0,2), xlab = "pr_pepsi", main="Coke: substitute pepsi price elasticity") 

#browseURL("https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html")
#install.packages("margins")
library(margins)
margins(logit2)
# AME
summary(margins(logit2, type = "response"))
cplot(logit2, "pr_coke")
#plotCurves(logit2, plotx = "pr_coke", ylim=c(0.2,0.7))

# Make a plot of the own price elasticity of pepsi, from the choice of pepsi as the dependent variable
# What is the own price elasticity at the mean price of pepsi, with no advertising?














######################################
#' Multinomial logit
#' 
rm(list=ls())
source("http://ansatte.uit.no/oystein.myrland/poe4/source/poe4read.R")
nels_small <- poe4read("nels_small")
head(nels_small)

# Obs:   1000 observations 
# 
# psechoice	= 1 if first postsecondary education was no college
# = 2 if first postsecondary education was a 2-year college
# = 3 if first postsecondary education was a 4-year college
# hscath		= 1 if catholic high school graduate
# grades		= average grade in math, english and social studies on 13 point scale with 1 = highest
# faminc		= gross 1991 family income (in $1000)
# famsiz		= number of family members
# parcoll		= 1 if most educated parent graduated from college or had an advanced degree
# female		= 1 if female
# black		= 1 if black


#install.packages("mlogit")
require(mlogit)

#dataframe
tally(~psechoice, data=nels_small)
summary(nels_small)
sd(nels_small$psechoice)
mean(nels_small$grades)
sd(nels_small$grades)
DF <- mlogit.data(nels_small, choice = "psechoice", shape="wide", alt.levels = c("no college","two-year college","four-year college"))
head(DF,12)
summary(DF)
# grades are individual specific
f1 <- mFormula(psechoice~0|grades)
head(model.matrix(f1,DF))
m1 <- mlogit(f1,DF, reflevel = "no college")
# Table 16.2
summary(m1)

tally(~psechoice, format = "percent", data=nels_small) # actual
apply(fitted(m1, outcome=FALSE),2,mean) # predicted

head(fitted(m1))
head(fitted(m1, outcome=FALSE))


######################################
#' Conditional logit
#' 
rm(list=ls())
source("http://ansatte.uit.no/oystein.myrland/poe4/source/poe4read.R")
cola <- poe4read("cola")
head(cola)

#dataframe
DF <- mlogit.data(cola, choice = "choice", shape = "long", alt.levels = c("Pepsi","7-Up","Coke"))

head(DF,12)
f1 <- mFormula(choice ~ price)

head(model.matrix(f1,DF))

m1 <- mlogit(f1,DF, reflevel = "Coke")
summary(m1) # Table 16.4a

z <- with(DF, data.frame(price = tapply(price, index(m1)$alt, mean)))
z                           

#install.packages("effects")
require(effects)
effects(m1, covariate = "price", data = z)  # mean prices


# Prices in Table 16.4b
z[1,1]=1.1
z[2,1]=1
z[3,1]=1.25
z

effects(m1, covariate = "price", data = z)



