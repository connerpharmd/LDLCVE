

#_________________________________________________________________________________________________
#_________________________________________________________________________________________________
#_________________________________________________________________________________________________
#    NOTE: if you have not already downloaded data to your local machine UNCOMMENT git hub code
#   below.  After you have downloaded source you can point read.csv to correct directory.
#_________________________________________________________________________________________________
#_________________________________________________________________________________________________
#_________________________________________________________________________________________________


##
##  SET UP CODE----
##
# NOTE: install if necessary
library(RColorBrewer)
library(RCurl)
library(data.table)
library(ggplot2)
library(splines)
library(boot)

# set options
options(stringsAsFactors=F, scipen=999)

# set random number generator to allow reproduction
set.seed(11)


# loading data from local 
statins <- read.csv("C:/Users/connerc/Documents/Admin/Pers/ispor_2015/secprevstatin_yrly.csv")


##
##  NOTE!!!  UNCOMMENT BELOW IF YOU HAVE NOT DOWNLOADED DATA YET
##

# url <- "https://raw.githubusercontent.com/connerpharmd/LDLCVE/master/secprevstatin_yrly.csv"
# statins <- getURL(url,  ssl.verifypeer = FALSE)                
# statins<- read.csv(textConnection(statins))


statins$year <- as.numeric(substr(statins$lastrand, 1, 4)) + round(as.numeric(substr(statins$lastrand, 5, 6))/13, 2)




##
##  TEST FOR HIGH LEVERAGE DATA----
##

## indication that 4s reprsents high leverage data in our case
lmmod <- lm(eventrate~LDL, data=statins)
lmcook <- cooks.distance(lmmod)
plot(lmcook)
# text(x=1:length(lmcook),y=lmcook, labels=1:length(lmcook), pos=1)
text(xpd=T, x=1:length(lmcook),y=lmcook, labels=paste(statins$Study, statins$Cohort, sep="-"), pos=4, cex=.7)
abline(h=(4*2)/nrow(statins), col="red", lty=2)
title(main="Cooks distance for linear function with (4*p)/n threshold")
library(MASS)
rlm <- stdres(lmmod)
##points with high cooks distance values
lmcook[ lmcook > (4*2)/nrow(statins)]


##  create statin vs pbo variable
##  just in case I ever want to do anything with it
statins$statin <- ifelse( statins$Cohort == "PBO", 0, 1)


##
##  PLOTS OF ALL DATA----
##



df1 <- statins
yval <- 'eventrate'
pyval <- 'Event (%/Year)'
xval <- 'LDL'
pxval <- 'LDL Cholesterol (mg/dL)'

##  slightly tweaked (I think this just looks better)

p.ylim <- c(0, 8)
p.xlim <- c(40, 210)
# ppi <- 300
# png(paste('tweakedtntplot_no_mods.png', sep="_"), width=10*ppi, height=6*ppi, res=ppi)
par(mar=c(5.1, 4.1, 4.1, 4))
df1$pchvec <- ifelse( grepl("PBO", df1$Cohort), 1, 19 )
plot( df1[, xval], df1[, yval], type="n", ylim= p.ylim, xlim= p.xlim , yaxt='n', xaxt='n', ylab="", xlab="")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = "grey88", border = "black")
par(new=T)
abline(h = pretty(p.ylim), col='white', lwd=2) ##  draw h lines
abline(v = c(50,70, 90, 110, 130, 150, 170, 190, 210), col='white', lwd=2) ##  draw v lines
par(new=T)
x1 <- brewer.pal(length(unique(df1$Study)), 'Paired') # grab some colors
cpal <- x1[rep(1:length(unique(df1$Study)),
               
               rle(df1$Study)[[1]])]
plot( df1[, xval], df1[, yval], pch=df1$pchvec, col= cpal , bg= df1$pchfill, cex=1.5  ,yaxt='n', xaxt='n', ylab="", xlab="", ylim= p.ylim, xlim= p.xlim , lwd=1)
axis(side=2, at=pretty(p.ylim), labels=pretty(p.ylim), las=1 )
axis(side=1, at=c(50, 70, 90, 110, 130, 150, 170, 190, 210), labels=c("50" , "70", "90", "110", "130", "150", "170", "190", "210")  )
# legend( "topleft", pch=c(19, 1, NA, NA, NA), lwd=c(NA, NA, 2, 2, 3), lty=c(NA, NA, 1, 2, 3), col=c("black", "black", "black", "red", "darkgreen")  , legend=c("Statin", "Placebo", "OLS", "Quadr", "Spline"), cex=1.2, border= "n", bty='n')
text(df1[, xval], df1[, yval], labels = df1$Study, pos = 3, font=2, col=cpal)
title(main="Event Rate/Year Plotted against LDL-C (mg/dL)\n in Secondary-Prevention Studies 4S to PROVE-IT.", ylab=pyval, xlab=pxval)
#
#
# abline(lm(df1$eventrate~df1$LDL), lwd=2)
# poly.plot <- lm(eventrate~poly(LDL, 2), data=df1)
# poly.pred <- predict(poly.plot, df1[xval])
# preddf <- data.frame(x = df1[,xval], y=poly.pred)
# preddf <- preddf[ order(preddf$x),]
# lines(x=preddf$x, y=preddf$y, type="l", col="red", lwd=3, lty=2)
# 
# 
# spl.fit <- lm(eventrate~bs(LDL, df=2, degree=1), data=df1) # or with bs you can define number of knots using df= (number of parameters including intercept)
# spl.pred <- predict(spl.fit , newdata=df1[xval])
# preddf2 <- data.frame(x = df1[,xval], y=spl.pred)
# preddf2 <- preddf2[ order(preddf2$x), ]
# lines(x=preddf2$x, preddf2$y, col='darkgreen', lty=3, lwd=3)
# 
# 
# 
# spl.fit <- lm(eventrate~bs(LDL, df=3, degree=2), data=df1) # or with bs you can define number of knots using df= (number of parameters including intercept)
# spl.pred <- predict(spl.fit , newdata=df1[xval])
# preddf2 <- data.frame(x = df1[,xval], y=spl.pred)
# preddf2 <- preddf2[ order(preddf2$x), ]
# lines(x=preddf2$x, preddf2$y, col='orange', lty=4, lwd=3)
# dev.off()




df1 <- statins
yval <- 'eventrate'
pyval <- 'Event (%/Year)'
xval <- 'LDL'
pxval <- 'LDL Cholesterol (mg/dL)'

##  slightly tweaked (I think this just looks better)
p.ylim <- c(0, 8)
p.xlim <- c(40, 210)
# ppi <- 300
# png(paste('tweakedtntplot_with_mods.png', sep="_"), width=10*ppi, height=6*ppi, res=ppi)
par(mar=c(5.1, 4.1, 4.1, 4))
df1$pchvec <- ifelse( grepl("PBO", df1$Cohort), 1, 19 )
plot( df1[, xval], df1[, yval], type="n", ylim= p.ylim, xlim= p.xlim , yaxt='n', xaxt='n', ylab="", xlab="")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = "grey88", border = "black")
par(new=T)
abline(h = pretty(p.ylim), col='white', lwd=2) ##  draw h lines
abline(v = c(50,70, 90, 110, 130, 150, 170, 190, 210), col='white', lwd=2) ##  draw v lines
par(new=T)
x1 <- brewer.pal(length(unique(df1$Study)), 'Paired') # grab some colors
cpal <- x1[rep(1:length(unique(df1$Study)), rle(df1$Study)[[1]])]

plot( df1[, xval], df1[, yval], pch=df1$pchvec, col= cpal , bg= df1$pchfill, cex=1.5  ,yaxt='n', xaxt='n', ylab="", xlab="", ylim= p.ylim, xlim= p.xlim , lwd=1)
axis(side=2, at=pretty(p.ylim), labels=pretty(p.ylim), las=1 )
axis(side=1, at=c(50, 70, 90, 110, 130, 150, 170, 190, 210), labels=c("50" , "70", "90", "110", "130", "150", "170", "190", "210")  )
legend( "topleft", pch=c(19, 1, NA, NA, NA, NA), lwd=c(NA, NA, 2, 3, 3, 3), lty=c(NA, NA, 1, 2, 3, 4), col=c("black", "black", "black", "red", "darkgreen", 'orange')  , legend=c("Statin", "Placebo", "L", "Q", "S1L", "S2Q"), cex=1.2, border= "n", bty='n')
text(df1[, xval], df1[, yval], labels = df1$Study, pos = 3, font=2, col=cpal)
title(main="Event Rate/Year Plotted against LDL-C (mg/dL)\n in Secondary-Prevention Studies (ALL).", ylab=pyval, xlab=pxval)
abline(lm(df1$eventrate~df1$LDL), lwd=2)
poly.plot <- lm(eventrate~poly(LDL, 2), data=df1)
poly.pred <- predict(poly.plot, df1[xval])
preddf <- data.frame(x = df1[,xval], y=poly.pred)
preddf <- preddf[ order(preddf$x),]
lines(x=preddf$x, y=preddf$y, type="l", col="red", lwd=3, lty=2)


spl.fit <- lm(eventrate~bs(LDL, df=2, degree=1), data=df1) # or with bs you can define number of knots using df= (number of parameters including intercept)
spl.pred <- predict(spl.fit , newdata=df1[xval])
preddf2 <- data.frame(x = df1[,xval], y=spl.pred)
preddf2 <- preddf2[ order(preddf2$x), ]
lines(x=preddf2$x, preddf2$y, col='darkgreen', lty=3, lwd=3)


spl.fit <- lm(eventrate~bs(LDL, df=3, degree=2), data=df1) # or with bs you can define number of knots using df= (number of parameters including intercept)
spl.pred <- predict(spl.fit , newdata=df1[xval])
preddf2 <- data.frame(x = df1[,xval], y=spl.pred)
preddf2 <- preddf2[ order(preddf2$x), ]
lines(x=preddf2$x, preddf2$y, col='orange', lty=4, lwd=3)

# dev.off()




##
##  GENERATE RSQUARED--ADJ FOR # PARAMETERS----
##


## models for paper (all data)
L <- lm(eventrate~LDL, data=df1)
Q <- lm(eventrate~poly(LDL, 2), data=df1)
S1L <-  lm(eventrate~bs(LDL, df=2, degree=1), data=df1)
S2Q <- lm(eventrate~bs(LDL, df=3, degree=2), data=df1)


##  run adjusted Rsq
sumL <- summary(L)
sumL$adj.r.squared
Lrsq <- sumL$adj.r.squared

sumQ <- summary(Q)
sumQ$adj.r.squared
Qrsq <- sumQ$adj.r.squared

sumS1L <- summary(S1L)
sumS1L$adj.r.squared
S1Lrsq <- sumS1L$adj.r.squared

sumS2Q <- summary(S2Q)
sumS2Q$adj.r.squared
S2Qrsq <- sumS2Q$adj.r.squared



##
##  GENERATE RMSE FOR ALL DATA----
##


## tun RMSE
Lrmse <- round(sqrt(mean(L$residuals^2)), 3)
Qrmse <- round(sqrt(mean(Q$residuals^2)), 3)
S1Lrmse <- round(sqrt(mean(S1L$residuals^2)), 3)
S2Qrmse <- round(sqrt(mean(S2Q$residuals^2)), 3)




##
## RUN LOOCV FOR ALL DATA----
##


glm.L <- glm(eventrate~LDL, data=df1)
glm.Q <- glm(eventrate~poly(LDL, 2), data=df1)
glm.S1L <-  glm(eventrate~bs(LDL, df=2, degree=1), data=df1)
glm.S2Q <- glm(eventrate~bs(LDL, df=3, degree=2), data=df1)


#  create function 
loormse <- function(data, glmmodel){
  sqrt(cv.glm(data, glmmodel)$delta[1])
}


Lloormse <- round(loormse(df1, glm.L), 3)
Qloormse <- round(loormse(df1, glm.Q), 3)
S1Lloormse <- round(loormse(df1, glm.S1L), 3)
S2Qloormse <- round(loormse(df1, glm.S2Q), 3)




##
#3  1,000 replicate MC CV RMSE ALL DATA----
##


repl <- 1000 # number of replications
modpct <- .9 # set % of sample used to train
mc.cv.df <- statins # what dataset you are using


mc.cv.rmse <- (rep(NA, repl))
for (i in 1:repl){
  train.size <- as.integer(modpct*nrow(mc.cv.df))
  train.index <- sample(1:nrow(mc.cv.df), size = train.size, replace = F)
  train.index <- train.index[ order(train.index)]
  test.index <- as.vector(1:nrow(mc.cv.df))[-train.index]
  fit=lm(eventrate~LDL, data=mc.cv.df[train.index,])
  plot.pred <- predict(fit, data=mc.cv.df[train.index, ])
  preddf <- data.frame(x = mc.cv.df[train.index,]["LDL"], y=plot.pred)
  preddf <- preddf[ order(preddf$y),]
  plot(mc.cv.df[train.index, ]$eventrate~mc.cv.df[train.index, ]$LDL)
  lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,mc.cv.df[test.index, ])
  cv.mse = mean((mc.cv.df$eventrate[test.index]-pred)^2)
  mc.cv.rmse[i] <- sqrt(cv.mse)
}
lm1.cv.rmse <- mc.cv.rmse
lm1.cv.q90 <- quantile(lm1.cv.rmse, probs=c(0.9))
median(lm1.cv.rmse)
hist(lm1.cv.rmse)
lm1.mc.cv.rmse <- mean(mc.cv.rmse)


mc.cv.rmse <- (rep(NA, repl))
for (i in 1:repl){
  train.size <- as.integer(modpct*nrow(mc.cv.df))
  train.index <- sample(1:nrow(mc.cv.df), size = train.size, replace = F)
  train.index <- train.index[ order(train.index)]
  test.index <- as.vector(1:nrow(mc.cv.df))[-train.index]
  fit=lm(eventrate~poly(LDL,2), data=mc.cv.df[train.index,])
  plot.pred <- predict(fit, data=mc.cv.df[train.index, ])
  preddf <- data.frame(x = mc.cv.df[train.index,]["LDL"], y=plot.pred)
  preddf <- preddf[ order(preddf$y),]
  plot(mc.cv.df[train.index, ]$eventrate~mc.cv.df[train.index, ]$LDL)
  lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,mc.cv.df[test.index, ])
  cv.mse = mean((mc.cv.df$eventrate[test.index]-pred)^2)
  mc.cv.rmse[i] <- sqrt(cv.mse)
}
poly1.cv.rmse <- mc.cv.rmse
poly1.cv.q90 <- quantile(poly1.cv.rmse, probs=c(0.9))
median(poly1.cv.rmse)
poly1.mc.cv.rmse <- mean(mc.cv.rmse)


mc.cv.rmse <- (rep(NA, repl))
for (i in 1:repl){
  train.size <- as.integer(modpct*nrow(mc.cv.df))
  train.index <- sample(1:nrow(mc.cv.df), size = train.size, replace = F)
  train.index <- train.index[ order(train.index)]
  test.index <- as.vector(1:nrow(mc.cv.df))[-train.index]
  fit=lm(eventrate~bs(LDL, df=2, degree=1), data=mc.cv.df[train.index,])
  plot.pred <- predict(fit, data=mc.cv.df[train.index, ])
  preddf <- data.frame(x = mc.cv.df[train.index,]["LDL"], y=plot.pred)
  preddf <- preddf[ order(preddf$y),]
  plot(mc.cv.df[train.index, ]$eventrate~mc.cv.df[train.index, ]$LDL)
  lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,mc.cv.df[test.index, ])
  cv.mse = mean((mc.cv.df$eventrate[test.index]-pred)^2)
  mc.cv.rmse[i] <- sqrt(cv.mse)
}
spl1.cv.rmse <- mc.cv.rmse
spl1.cv.q90 <- quantile(spl1.cv.rmse, probs=c(0.9))
spl1.mc.cv.rmse <- mean(mc.cv.rmse)


mc.cv.rmse <- (rep(NA, repl))
for (i in 1:repl){
  train.size <- as.integer(modpct*nrow(mc.cv.df))
  train.index <- sample(1:nrow(mc.cv.df), size = train.size, replace = F)
  train.index <- train.index[ order(train.index)]
  test.index <- as.vector(1:nrow(mc.cv.df))[-train.index]
  fit=lm(eventrate~bs(LDL, df=3, degree=2), data=mc.cv.df[train.index,])
  plot.pred <- predict(fit, data=mc.cv.df[train.index, ])
  preddf <- data.frame(x = mc.cv.df[train.index,]["LDL"], y=plot.pred)
  preddf <- preddf[ order(preddf$LDL),]
  plot(mc.cv.df[train.index, ]$eventrate~mc.cv.df[train.index, ]$LDL)
  lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,mc.cv.df[test.index, ])
  cv.mse = mean((mc.cv.df$eventrate[test.index]-pred)^2)
  mc.cv.rmse[i] <- sqrt(cv.mse)
}
spld1.cv.rmse <- mc.cv.rmse
spld1.cv.q90 <- quantile(spld1.cv.rmse, probs=c(0.9))
spld1.mc.cv.rmse <- mean(mc.cv.rmse)




##
##  DIAGNOSTICS FOR ALL DATA----
##

models <- c("L", "Q", "S1L", "S2L")
rsq.vec <- round(c(Lrsq, Qrsq, S1Lrsq, S2Qrsq), 3)
rmse.vec <- c(Lrmse, Qrmse, S1Lrmse, S2Qrmse)
loormse.vec <- c(Lloormse, Qloormse, S1Lloormse, S2Qloormse)
mccvrmse.vec <- round( c(lm1.mc.cv.rmse, poly1.mc.cv.rmse, spl1.mc.cv.rmse, spld1.mc.cv.rmse), 3)
tab1 <- data.frame(models, rsq.vec, rmse.vec, loormse.vec, mccvrmse.vec)
write.csv(tab1, file="fit_stats.csv", row.names=F)





##
##  PREDICTED EVENTS AT LDL 50----
##

L <- lm(eventrate~LDL, data=df1)
Q <- lm(eventrate~poly(LDL, 2), data=df1)
S1L <-  lm(eventrate~bs(LDL, df=2, degree=1), data=df1)
S2Q <- lm(eventrate~bs(LDL, df=3, degree=2), data=df1)


#predictions of 50mg/dL
Lp50 <- predict(L, data.frame(LDL = 50))
Lp40 <- predict(L, data.frame(LDL = 40))
Qp50 <- predict(Q, data.frame(LDL = 50))
Qp40 <- predict(Q, data.frame(LDL = 40))
S1Lp50 <- predict(S1L, data.frame(LDL = 50))
S1Lp40 <- predict(S1L, data.frame(LDL = 40))
S2Qp50 <- predict(S2Q, data.frame(LDL = 50))
S2Qp40 <- predict(S2Q, data.frame(LDL = 40))
#

models <- c("L", "Q", "S1L", "S2Q")
LDL <- c("50", "50", "50", "50")
predict <- c(Lp50, Qp50, S1Lp50, S2Qp50)
tab2 <- data.frame(models, LDL, predict)
write.csv(tab2, file="predict50.csv", row.names=F)




##
##  ALL DATA PREDICTION LINES PLOTTED OVER DATA AND EXTRAPOLATION----
##  NOTE-- indicates that quadratic is NOT REASONABLE
##


p.ylim <- c(0, 8)
p.xlim <- c(10, 210)
LDL20.200 <- seq(20, 200, by=.5)
S2Q20.200 <- predict(S2Q, data.frame(LDL = LDL20.200))
plot(S2Q20.200~LDL20.200, type="n")
lines(x=LDL20.200, y=S2Q20.200, col='orange', lty=1, lwd=3)
par(new=T)
LDL20.200 <- seq(20, 200, by=.5)
Q20.200 <- predict(Q, data.frame(LDL = LDL20.200))
lines(x=LDL20.200, y=Q20.200, col='red', lty=1, lwd=3)
par(new=T)
L20.200 <- predict(L, data.frame(LDL = LDL20.200))
par(new=T)
lines(x=LDL20.200, y=L20.200, col='black', lty=1, lwd=3)
# ppi <- 300
# png(paste('3bestmods.png', sep="_"), width=10*ppi, height=6*ppi, res=ppi)
par(mar=c(5.1, 4.1, 4.1, 4))
plot(S2Q20.200~LDL20.200, type="n", ylim= p.ylim, xlim= p.xlim , yaxt='n', xaxt='n', ylab="", xlab="")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = "grey88", border = "black")
par(new=T)
abline(h = pretty(p.ylim), col='white', lwd=2) ##  draw h lines
abline(v = pretty(p.xlim), col='white', lwd=2) ##  draw v lines
par(new=T)
lines(x=LDL20.200, y=L20.200, col='black', lty=1, lwd=3)
par(new=T)
lines(x=LDL20.200, y=Q20.200, col='blue', lty=1, lwd=3)
par(new=T)
lines(x=LDL20.200, y=S2Q20.200, col='orange', lty=1, lwd=3)
axis(side=2, at=pretty(p.ylim), labels=pretty(p.ylim), las=1 )
axis(side=1, at=pretty(p.xlim), labels=pretty(p.xlim), las=1 )

legend( "topleft", pch=c(NA), lwd=c(3,3,3), lty=c(1,1,1), col=c("black", "blue", "orange")  , legend=c("L", "Q", "S2Q"), cex=1.2, border= "n", bty='n')
title(main="Q, L and S2Q Modeled Event Rates/Year by LDL-C (20-200 mg/dL)", ylab=pyval, xlab=pxval)
# dev.off()




##
##  RERUN TEST FOR HIGH LEVERAGE----
##  NOTE: only difference vs above is nicer looking plot
##


#models
lm1 <-lm(eventrate~LDL, data=statins)
lm1alt1 <-lm(eventrate~LDL+year, data=statins)
poly1 <- (lm(eventrate~LDL+I(LDL^2), data=statins))
lm2 <- lm(eventrate~LDL, data=statinsno4s)
poly2 <- (lm(eventrate~LDL+I(LDL^2), data=statinsno4s))


## indication that 4s reprsents high leverage data in our case
lmcook <- cooks.distance(lm1)
plot(lmcook)
# text(x=1:length(lmcook),y=lmcook, labels=1:length(lmcook), pos=1)
text(x=1:length(lmcook),y=lmcook, labels=statins$Study, pos=4, cex=.7)
abline(h=4*(length(lm1$coefficients))/nrow(statins), col="red", lty=2)
title(main="Cooks distance for LM1 with (4*p)/n threshold")
library(MASS)
rlm <- stdres(lmmod)
##points with high cooks distance values
lmcook[ lmcook > 4*(length(lm1$coefficients))/nrow(statins)]



##
##  CRLEATE CENSORED DATA SET
##


statinsno4s <- statins[ which(statins$Study != "4S"), ] ## consolidate all code and run all test models at same time

## of note 4S is the oldest study -- in terms of last randomized pt
## suggests why this may be a high leverage study--in terms of advances in non-statin CV dz care/guidelines
unique(statins$Study[ order(statins$lastrand)])
(statins[ statins$Study == "4S", "lastrand"])




##
##  PLOT CENSORED DATA
##

df1 <- statinsno4s
yval <- 'eventrate'
pyval <- 'Event (%)'
xval <- 'LDL'
pxval <- 'LDL Cholesterol (mg/dL)'

##  slightly tweaked (I think this just looks better)
p.ylim <- c(0, 5)
p.xlim <- c(40, 210)
# ppi <- 300
# png(paste('tweakedtntplotno4s.png', sep="_"), width=10*ppi, height=6*ppi, res=ppi)
par(mar=c(5.1, 4.1, 4.1, 4))
df1$pchvec <- ifelse( grepl("PBO", df1$Cohort), 1, 19 )
plot( df1[, xval], df1[, yval], type="n", ylim= p.ylim, xlim= p.xlim, yaxt='n', xaxt='n', ylab="", xlab="")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = "grey88", border = "black")
par(new=T)
abline(h = pretty(p.ylim), col='white', lwd=2) ##  draw h lines
abline(v = c(50,70, 90, 110, 130, 150, 170, 190, 210), col='white', lwd=2) ##  draw v lines
par(new=T)
x1 <- brewer.pal(length(unique(df1$Study)), 'Set2') # grab some colors
cpal <- x1[rep(1:length(unique(df1$Study)), rle(df1$Study)[[1]])]
plot( df1[, xval], df1[, yval], pch=df1$pchvec, col= cpal , bg= df1$pchfill, cex=1.5  ,yaxt='n', xaxt='n', ylab="", xlab="", ylim= p.ylim, xlim= p.xlim, lwd=2)
axis(side=2, at=pretty(p.ylim), labels=pretty(p.ylim), las=1 )
axis(side=1, at=c(50, 70, 90, 110, 130, 150, 170, 190, 210), labels=c("50" , "70", "90", "110", "130", "150", "170", "190", "210")  )
legend( "topleft", pch=c(19, 1, NA, NA, NA), lwd=c(NA, NA, 2, 2, 3), lty=c(NA, NA, 1, 2, 3), col=c("black", "black", "black", "red", "darkgreen")  , legend=c("Statin", "Placebo", "OLS", "Quadr", "Spline"), cex=1.2, border= "n", bty='n')
text(df1[, xval], df1[, yval], labels = df1$Study, pos = 3, font=2, col=cpal)
title(main="Event Rates Plotted against LDL Cholesterol Levels\nduring Statin Therapy in Secondary-Prevention Studies (CENSORED).", ylab=pyval, xlab=pxval)
abline(lm(df1$eventrate~df1$LDL), lwd=1, col="black")
poly.plot <- lm(eventrate~poly(LDL, 2), data=df1)
poly.pred <- predict(poly.plot, df1[xval])
preddf <- data.frame(x = df1[,xval], y=poly.pred)
preddf <- preddf[ order(preddf$x),]
lines(x=preddf$x, y=preddf$y, type="l", col="red", lwd=3, lty=2)

spl.fit <- lm(eventrate~bs(LDL, df=2, degree=1), data=df1) # or with bs you can define number of knots using df= (number of parameters including intercept)
spl.pred <- predict(spl.fit , newdata=df1[xval])
preddf2 <- data.frame(x = df1[,xval], y=spl.pred)
preddf2 <- preddf2[ order(preddf2$x), ]
lines(x=preddf2$x, preddf2$y, col='darkgreen', lty=2, lwd=3)

spl.fit <- lm(eventrate~bs(LDL, df=3, degree=2), data=df1) # or with bs you can define number of knots using df= (number of parameters including intercept)
spl.pred <- predict(spl.fit , newdata=df1[xval])
preddf2 <- data.frame(x = df1[,xval], y=spl.pred)
preddf2 <- preddf2[ order(preddf2$x), ]
lines(x=preddf2$x, preddf2$y, col='orange', lty=4, lwd=3)

# dev.off()




##
##  DIAGNOSTICS FOR ALL MODELS ALL+CENS----
##

#models
lm1 <-lm(eventrate~LDL, data=statins)
lm2 <- lm(eventrate~LDL, data=statinsno4s)

poly1 <- (lm(eventrate~LDL+I(LDL^2), data=statins))
poly2 <- (lm(eventrate~LDL+I(LDL^2), data=statinsno4s))

spl1 <- lm(eventrate~bs(LDL, df=2, degree=1), data=statins)
spl2 <- lm(eventrate~bs(LDL, df=2, degree=1), data=statinsno4s)

spld1 <- lm(eventrate~bs(LDL, df=3, degree=2), data=statins)
spld2 <- lm(eventrate~bs(LDL, df=3, degree=2), data=statinsno4s)


# predicted values
lm1pred <- predict(lm1, statins)
poly1pred <- predict(poly1, statins)
spl1pred <- predict(spl1, statins)
lm2pred <- predict(lm2, statinsno4s)
poly2pred <- predict(poly2, statinsno4s)
spl2pred <- predict(spl2, statins)

# rmse
lm1rmse <- sqrt(mean(lm1$residuals^2))
lm2rmse <- sqrt(mean(lm2$residuals^2))
poly1rmse <- sqrt(mean(poly1$residuals^2))
poly2rmse <- sqrt(mean(poly2$residuals^2))
spl1rmse <- sqrt(mean(spl1$residuals^2))
spl2rmse <- sqrt(mean(spl2$residuals^2))
spld1rmse <- sqrt(mean(spld1$residuals^2))
spld2rmse <- sqrt(mean(spld2$residuals^2))





##
##  PREDICTED EVENTS FOR ALL MODELS ALL+CENS----
##


#predictions of 50mg/dL
lm1p50 <- predict(lm1, data.frame(LDL = 50))
lm2p50 <- predict(lm2, data.frame(LDL = 50))
poly1p50 <- predict(poly1, data.frame(LDL = 50))
poly2p50 <- predict(poly2, data.frame(LDL = 50))
spl1p50 <- predict(spl1, data.frame(LDL = 50))
spl2p50 <- predict(spl2, data.frame(LDL = 50))

spld1p50 <- predict(spld1, data.frame(LDL = 50))
spld2p50 <- predict(spld2, data.frame(LDL = 50))




##
##  CREATE PREDICTION TABLE FOR ALL+CENS----
##


pred50df <- data.frame(
  model=c(
    "L-A", 
    "L-C", 
    "Q-A", 
    "Q-C", 
    "S-A", 
    "S-C",
    "S2-A",
    "S2-C"), 
  pred50mgdl=c(
    lm1p50, 
    lm2p50, 
    poly1p50, 
    poly2p50, 
    spl1p50, 
    spl2p50,
    spld1p50,
    spld2p50))

pred50df$pred50mgdl <- round(pred50df$pred50mgdl, 2)
write.csv( pred50df, file="pred50.a_and_c.csv", row.names=F)




##
##  FUN STUFF
##


#bootstrapped CI of coefficients
mylboot = function(data){
  funlm <- lm(eventrate~LDL, data=data[  sample(1:nrow(data), replace = TRUE), ])
  c(int=funlm$coefficients[1], LDL1=funlm$coefficients[2])
}
mypboot = function(data){
  funlm <- lm(eventrate~LDL+I(LDL^2), data=data[  sample(1:nrow(data), replace = TRUE), ])
  c(int=funlm$coefficients[1], LDL1=funlm$coefficients[2], LDL2=funlm$coefficients[3])
}

mylboot(statins)#test working
mypboot(statins)

## create bootsrapped CIs 
bootcilm1 <- replicate(1000, mylboot(statins) )
apply(bootcilm1, MARGIN = 1, function (x)  quantile(x, probs = c(0.025, 0.975))  )
bootcilm2 <- replicate(1000, mylboot(statinsno4s) )
apply(bootcilm2, MARGIN = 1, function (x)  quantile(x, probs = c(0.025, 0.975))  )

bootcip1 <- replicate(1000, mypboot(statins) )
apply(bootcip1, MARGIN = 1, function (x)  quantile(x, probs = c(0.025, 0.975))  )
bootcip2 <- replicate(1000, mypboot(statinsno4s) )
apply(bootcip2, MARGIN = 1, function (x)  quantile(x, probs = c(0.025, 0.975))  )




##
##  RUN K-FOLD X-VALIDATION FOR ALL + CENSORED
##


## Run K-fold cross validation to test model fit 

kf <- 10
## lm1
cvdf <- statins
set.seed(11)
folds <- sample(rep(1:kf, length=nrow(cvdf)))
cv.rmse=(rep(NA,kf))
for(k in 1:kf){
  fit=lm(eventrate~LDL, data=cvdf[folds!= k,])
    plot.pred <- predict(fit, data=cvdf[folds!= k,])
    preddf <- data.frame(x = cvdf[folds!= k,]["LDL"], y=plot.pred)
    preddf <- preddf[ order(preddf$y),]
    plot(cvdf[folds!= k,]$eventrate~cvdf[folds!= k,]$LDL)
    lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,cvdf[folds==k,])
  cv.mse = mean((cvdf$eventrate[folds==k]-pred)^2)
  cv.rmse[k] <- sqrt(cv.mse)
}
lm1cvrmse <- mean(cv.rmse)

# lm2
cvdf <- statinsno4s
set.seed(12)
folds <- sample(rep(1:kf, length=nrow(cvdf)))
cv.rmse=(rep(NA,kf))
for(k in 1:kf){
  fit=lm(eventrate~LDL, data=cvdf[folds!= k,])
  #   abline(fit)
  pred=predict(fit,cvdf[folds==k,])
  cv.mse = mean((cvdf$eventrate[folds==k]-pred)^2)
  cv.rmse[k] <- sqrt(cv.mse)
}
lm2cvrmse <- mean(cv.rmse)


## poly1
cvdf <- statins
set.seed(14)
folds <- sample(rep(1:kf, length=nrow(cvdf)))
cv.rmse=(rep(NA,kf))
for(k in 1:kf){
  fit=lm(eventrate~poly(LDL,2), data=cvdf[folds!= k,])
  plot.pred <- predict(fit, data=cvdf[folds!= k,])
  preddf <- data.frame(x = cvdf[folds!= k,]["LDL"], y=plot.pred)
  preddf <- preddf[ order(preddf$y),]
  plot(cvdf[folds!= k,]$eventrate~cvdf[folds!= k,]$LDL)
  lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,cvdf[folds==k,])
  cv.mse = mean((cvdf$eventrate[folds==k]-pred)^2)
  cv.rmse[k] <- sqrt(cv.mse)
}
poly1cvrmse <- mean(cv.rmse)


## poly2
cvdf <- statinsno4s
set.seed(14)
folds <- sample(rep(1:kf, length=nrow(cvdf)))
cv.rmse=(rep(NA,kf))
for(k in 1:kf){
  fit=lm(eventrate~LDL+I(LDL^2), data=cvdf[folds!= k,])
  #   abline(fit)
  pred=predict(fit,cvdf[folds==k,])
  cv.mse = mean((cvdf$eventrate[folds==k]-pred)^2)
  cv.rmse[k] <- sqrt(cv.mse)
}
poly2cvrmse <- mean(cv.rmse)


## spline1
cvdf <- statins
set.seed(14)
folds <- sample(rep(1:kf, length=nrow(cvdf)))
cv.rmse=(rep(NA,kf))
for(k in 1:kf){
  fit=lm(eventrate~bs(LDL, df=2, degree=1), data=cvdf[folds!= k,])
  plot.pred <- predict(fit, data=cvdf[folds!= k,])
  preddf <- data.frame(x = cvdf[folds!= k,]["LDL"], y=plot.pred)
  preddf <- preddf[ order(preddf$y),]
  plot(cvdf[folds!= k,]$eventrate~cvdf[folds!= k,]$LDL)
  lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,cvdf[folds==k,])
  cv.mse = mean((cvdf$eventrate[folds==k]-pred)^2)
  cv.rmse[k] <- sqrt(cv.mse)
}
spl1cvrmse <- mean(cv.rmse)


## spline2
cvdf <- statinsno4s
set.seed(14)
folds <- sample(rep(1:kf, length=nrow(cvdf)))
cv.rmse=(rep(NA,kf))
for(k in 1:kf){
  fit=lm(eventrate~bs(LDL, df=2, degree=1), data=cvdf[folds!= k,])
  plot.pred <- predict(fit, data=cvdf[folds!= k,])
  preddf <- data.frame(x = cvdf[folds!= k,]["LDL"], y=plot.pred)
  preddf <- preddf[ order(preddf$y),]
  plot(cvdf[folds!= k,]$eventrate~cvdf[folds!= k,]$LDL)
  preddf <- preddf[ order(preddf$LDL), ]
  lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,cvdf[folds==k,])
  cv.mse = mean((cvdf$eventrate[folds==k]-pred)^2)
  cv.rmse[k] <- sqrt(cv.mse)
}
spl2cvrmse <- mean(cv.rmse)




##
#3  LOOCV FOR ALL+CENS----
##


glm1 <-glm(eventrate~LDL, data=statins)
gpoly1 <- (glm(eventrate~LDL+I(LDL^2), data=statins))
gspl1 <- glm(eventrate~bs(LDL, df=2, degree=1), data=statins)
gspld1 <- glm(eventrate~bs(LDL, df=3, degree=1), data=statins)
glm2 <- glm(eventrate~LDL, data=statinsno4s)
gpoly2 <- (glm(eventrate~LDL+I(LDL^2), data=statinsno4s))
gspl2 <- glm(eventrate~bs(LDL, df=2, degree=1), data=statinsno4s)
gspld2 <- glm(eventrate~bs(LDL, df=3, degree=2), data=statinsno4s)


loormse <- function(data, glmmodel){
  sqrt(cv.glm(data, glmmodel)$delta[1])
}


lm1.loormse <- loormse(statins, glm1)
poly1.loormse <- loormse(statins, gpoly1)
spl1.loormse <- loormse(statins, gspl1)
spld1.loormse <- loormse(statins, gspld1)
lm2.loormse <- loormse(statinsno4s, glm2)
poly2.loormse <- loormse(statinsno4s, gpoly2)
spl2.loormse <- loormse(statinsno4s, gspl2)
spld2.loormse <- loormse(statinsno4s, gspld2)





#3
#3  1,000 MC CV RMSE FOR ALL+CENS----
##


repl <- 1000
modpct <- .9
mc.cv.df <- statins


mc.cv.rmse <- (rep(NA, repl))
for (i in 1:repl){
  train.size <- as.integer(modpct*nrow(mc.cv.df))
  train.index <- sample(1:nrow(mc.cv.df), size = train.size, replace = F)
  train.index <- train.index[ order(train.index)]
  test.index <- as.vector(1:nrow(mc.cv.df))[-train.index]
  fit=lm(eventrate~LDL, data=mc.cv.df[train.index,])
#   plot.pred <- predict(fit, data=mc.cv.df[train.index, ])
#   preddf <- data.frame(x = mc.cv.df[train.index,]["LDL"], y=plot.pred)
#   preddf <- preddf[ order(preddf$y),]
#   plot(mc.cv.df[train.index, ]$eventrate~mc.cv.df[train.index, ]$LDL)
#   lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,mc.cv.df[test.index, ])
  cv.mse = mean((mc.cv.df$eventrate[test.index]-pred)^2)
  mc.cv.rmse[i] <- sqrt(cv.mse)
}
lm1.cv.rmse <- mc.cv.rmse
lm1.cv.q90 <- quantile(lm1.cv.rmse, probs=c(0.9))
# median(lm1.cv.rmse)
# hist(lm1.cv.rmse)
lm1.mc.cv.rmse <- mean(mc.cv.rmse)
lm1.mc.cv.rmse



mc.cv.rmse <- (rep(NA, repl))
for (i in 1:repl){
  train.size <- as.integer(modpct*nrow(mc.cv.df))
  train.index <- sample(1:nrow(mc.cv.df), size = train.size, replace = F)
  train.index <- train.index[ order(train.index)]
  test.index <- as.vector(1:nrow(mc.cv.df))[-train.index]
  fit=lm(eventrate~poly(LDL,2), data=mc.cv.df[train.index,])
#   plot.pred <- predict(fit, data=mc.cv.df[train.index, ])
#   preddf <- data.frame(x = mc.cv.df[train.index,]["LDL"], y=plot.pred)
#   preddf <- preddf[ order(preddf$y),]
#   plot(mc.cv.df[train.index, ]$eventrate~mc.cv.df[train.index, ]$LDL)
#   lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,mc.cv.df[test.index, ])
  cv.mse = mean((mc.cv.df$eventrate[test.index]-pred)^2)
  mc.cv.rmse[i] <- sqrt(cv.mse)
}
poly1.cv.rmse <- mc.cv.rmse
poly1.cv.q90 <- quantile(poly1.cv.rmse, probs=c(0.9))
# median(poly1.cv.rmse)
poly1.mc.cv.rmse <- mean(mc.cv.rmse)



mc.cv.rmse <- (rep(NA, repl))
for (i in 1:repl){
  train.size <- as.integer(modpct*nrow(mc.cv.df))
  train.index <- sample(1:nrow(mc.cv.df), size = train.size, replace = F)
  train.index <- train.index[ order(train.index)]
  test.index <- as.vector(1:nrow(mc.cv.df))[-train.index]
  fit=lm(eventrate~bs(LDL, df=2, degree=1), data=mc.cv.df[train.index,])
#   plot.pred <- predict(fit, data=mc.cv.df[train.index, ])
#   preddf <- data.frame(x = mc.cv.df[train.index,]["LDL"], y=plot.pred)
#   preddf <- preddf[ order(preddf$y),]
#   plot(mc.cv.df[train.index, ]$eventrate~mc.cv.df[train.index, ]$LDL)
#   lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,mc.cv.df[test.index, ])
  cv.mse = mean((mc.cv.df$eventrate[test.index]-pred)^2)
  mc.cv.rmse[i] <- sqrt(cv.mse)
}
spl1.cv.rmse <- mc.cv.rmse
spl1.cv.q90 <- quantile(spl1.cv.rmse, probs=c(0.9))
spl1.mc.cv.rmse <- mean(mc.cv.rmse)



mc.cv.rmse <- (rep(NA, repl))
for (i in 1:repl){
  train.size <- as.integer(modpct*nrow(mc.cv.df))
  train.index <- sample(1:nrow(mc.cv.df), size = train.size, replace = F)
  train.index <- train.index[ order(train.index)]
  test.index <- as.vector(1:nrow(mc.cv.df))[-train.index]
  fit=lm(eventrate~bs(LDL, df=3, degree=2), data=mc.cv.df[train.index,])
#   plot.pred <- predict(fit, data=mc.cv.df[train.index, ])
#   preddf <- data.frame(x = mc.cv.df[train.index,]["LDL"], y=plot.pred)
#   preddf <- preddf[ order(preddf$LDL),]
#   plot(mc.cv.df[train.index, ]$eventrate~mc.cv.df[train.index, ]$LDL)
#   lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,mc.cv.df[test.index, ])
  cv.mse = mean((mc.cv.df$eventrate[test.index]-pred)^2)
  mc.cv.rmse[i] <- sqrt(cv.mse)
}
spld1.cv.rmse <- mc.cv.rmse
spld1.cv.q90 <- quantile(spld1.cv.rmse, probs=c(0.9))
spld1.mc.cv.rmse <- mean(mc.cv.rmse)





mc.cv.df <- statinsno4s
repl <- 1000
modpct <- .9
mc.cv.rmse <- (rep(NA, repl))


mc.cv.rmse <- (rep(NA, repl))
for (i in 1:repl){
  train.size <- as.integer(modpct*nrow(mc.cv.df))
  train.index <- sample(1:nrow(mc.cv.df), size = train.size, replace = F)
  train.index <- train.index[ order(train.index)]
  test.index <- as.vector(1:nrow(mc.cv.df))[-train.index]
  fit=lm(eventrate~LDL, data=mc.cv.df[train.index,])
#   plot.pred <- predict(fit, data=mc.cv.df[train.index, ])
#   preddf <- data.frame(x = mc.cv.df[train.index,]["LDL"], y=plot.pred)
#   preddf <- preddf[ order(preddf$LDL),]
#   plot(mc.cv.df[train.index, ]$eventrate~mc.cv.df[train.index, ]$LDL)
#   lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,mc.cv.df[test.index, ])
  cv.mse = mean((mc.cv.df$eventrate[test.index]-pred)^2)
  mc.cv.rmse[i] <- sqrt(cv.mse)
}
lm2.cv.rmse <- mc.cv.rmse
lm2.cv.q90 <- quantile(lm2.cv.rmse, probs=c(0.9))
lm2.mc.cv.rmse <- mean(mc.cv.rmse)


mc.cv.rmse <- (rep(NA, repl))
for (i in 1:repl){
  train.size <- as.integer(modpct*nrow(mc.cv.df))
  train.index <- sample(1:nrow(mc.cv.df), size = train.size, replace = F)
  train.index <- train.index[ order(train.index)]
  test.index <- as.vector(1:nrow(mc.cv.df))[-train.index]
  fit=lm(eventrate~poly(LDL,2), data=mc.cv.df[train.index,])
#   plot.pred <- predict(fit, data=mc.cv.df[train.index, ])
#   preddf <- data.frame(x = mc.cv.df[train.index,]["LDL"], y=plot.pred)
#   preddf <- preddf[ order(preddf$LDL),]
#   plot(mc.cv.df[train.index, ]$eventrate~mc.cv.df[train.index, ]$LDL)
#   lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,mc.cv.df[test.index, ])
  cv.mse = mean((mc.cv.df$eventrate[test.index]-pred)^2)
  mc.cv.rmse[i] <- sqrt(cv.mse)
}
poly2.cv.rmse <- mc.cv.rmse
poly2.cv.q90 <- quantile(poly2.cv.rmse, probs=c(0.9))
poly2.mc.cv.rmse <- mean(mc.cv.rmse)


mc.cv.rmse <- (rep(NA, repl))
for (i in 1:repl){
  train.size <- as.integer(modpct*nrow(mc.cv.df))
  train.index <- sample(1:nrow(mc.cv.df), size = train.size, replace = F)
  train.index <- train.index[ order(train.index)]
  test.index <- as.vector(1:nrow(mc.cv.df))[-train.index]
  fit=lm(eventrate~bs(LDL, df=2, degree=1), data=mc.cv.df[train.index,])
#   plot.pred <- predict(fit, data=mc.cv.df[train.index, ])
#   preddf <- data.frame(x = mc.cv.df[train.index,]["LDL"], y=plot.pred)
#   preddf <- preddf[ order(preddf$LDL),]
#   plot(mc.cv.df[train.index, ]$eventrate~mc.cv.df[train.index, ]$LDL)
#   lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,mc.cv.df[test.index, ])
  cv.mse = mean((mc.cv.df$eventrate[test.index]-pred)^2)
  mc.cv.rmse[i] <- sqrt(cv.mse)
}
spl2.cv.rmse <- mc.cv.rmse
spl2.cv.q90 <- quantile(spl2.cv.rmse, probs=c(0.9))
spl2.mc.cv.rmse <- mean(mc.cv.rmse)


mc.cv.rmse <- (rep(NA, repl))
for (i in 1:repl){
  train.size <- as.integer(modpct*nrow(mc.cv.df))
  train.index <- sample(1:nrow(mc.cv.df), size = train.size, replace = F)
  train.index <- train.index[ order(train.index)]
  test.index <- as.vector(1:nrow(mc.cv.df))[-train.index]
  fit=lm(eventrate~bs(LDL, df=3, degree=2), data=mc.cv.df[train.index,])
#   plot.pred <- predict(fit, data=mc.cv.df[train.index, ])
#   preddf <- data.frame(x = mc.cv.df[train.index,]["LDL"], y=plot.pred)
#   preddf <- preddf[ order(preddf$LDL),]
#   plot(mc.cv.df[train.index, ]$eventrate~mc.cv.df[train.index, ]$LDL)
#   lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,mc.cv.df[test.index, ])
  cv.mse = mean((mc.cv.df$eventrate[test.index]-pred)^2)
  mc.cv.rmse[i] <- sqrt(cv.mse)
}
spld2.cv.rmse <- mc.cv.rmse
spld2.cv.q90 <- quantile(spld2.cv.rmse, probs=c(0.9))
spld2.mc.cv.rmse <- mean(mc.cv.rmse)




##
##  MORE DIAGNOSTICS ALL+CENSORED----
##



moddata <- statins
LArsq <- summary(lm(eventrate~LDL, data=moddata))$adj.r.squared
QArsq <- summary(lm(eventrate~poly(LDL, 2), data=moddata))$adj.r.squared
S1LArsq <-  summary(lm(eventrate~bs(LDL, df=2, degree=1), data=moddata))$adj.r.squared
S2QArsq <- summary(lm(eventrate~bs(LDL, df=3, degree=2), data=moddata))$adj.r.squared


moddata <- statinsno4s
LCrsq <- summary(lm(eventrate~LDL, data=moddata))$adj.r.squared
QCrsq <- summary(lm(eventrate~poly(LDL, 2), data=moddata))$adj.r.squared
S1LCrsq <-  summary(lm(eventrate~bs(LDL, df=2, degree=1), data=moddata))$adj.r.squared
S2QCrsq <- summary(lm(eventrate~bs(LDL, df=3, degree=2), data=moddata))$adj.r.squared




##
##  CREATE DIAGNOSTICS TABLE ALL+CENS----
##


cv.rmse.df <- data.frame(
  model=c(
    "L-A", 
    "L-C", 
    "Q-A", 
    "Q-C", 
    "S1L-A", 
    "S1L-C",
    "S2Q-A",
    "S2Q-C"),
  
  adjrsq=c(
    LArsq,
    LCrsq,
    QArsq,
    QCrsq,
    S1LArsq,
    S1LCrsq,
    S2QArsq,
    S2QCrsq),
  
  rmse=c(
    lm1rmse,
    lm2rmse,
    poly1rmse,
    poly2rmse,
    spl1rmse,
    spl2rmse,
    spld1rmse,
    spld2rmse),
  loormse=c(
    lm1.loormse,
    lm2.loormse,
    poly1.loormse,
    poly2.loormse,
    spl1.loormse,
    spl2.loormse,
    spld1.loormse,
    spld2.loormse),
  mccvrmse=c(
    lm1.mc.cv.rmse, 
    lm2.mc.cv.rmse, 
    poly1.mc.cv.rmse, 
    poly2.mc.cv.rmse, 
    spl1.mc.cv.rmse, 
    spl2.mc.cv.rmse,
    spld1.mc.cv.rmse,
    spld2.mc.cv.rmse),
  cvrmseq90=c(
    lm1.cv.q90,
    lm2.cv.q90,
    poly1.cv.q90,
    poly2.cv.q90,
    spl1.cv.q90,
    spl2.cv.q90,
    spld1.cv.q90,
    spld2.cv.q90)
  )

write.csv(cv.rmse.df, file="all_cens_cvrmsedf.csv", row.names=F)


##
##
##  Yet another way to view the world--perhaps on statin and no-statin are 2 different models
##
##





df1 <- statinsno4s
yval <- 'eventrate'
pyval <- 'Event (%)'
xval <- 'LDL'
pxval <- 'LDL Cholesterol (mg/dL)'

##  slightly tweaked (I think this just looks better)
p.ylim <- c(0, 5)
p.xlim <- c(40, 210)
ppi <- 300

# png(paste('tweakedtntplot.png', sep="_"), width=10*ppi, height=6*ppi, res=ppi)

par(mar=c(5.1, 4.1, 4.1, 4))
df1$pchvec <- ifelse( grepl("PBO", df1$Cohort), 1, 19 )
plot( df1[, xval], df1[, yval], type="n", ylim= p.ylim, xlim= p.xlim , yaxt='n', xaxt='n', ylab="", xlab="")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = "grey88", border = "black")
par(new=T)
abline(h = pretty(p.ylim), col='white', lwd=2) ##  draw h lines
abline(v = c(50,70, 90, 110, 130, 150, 170, 190, 210), col='white', lwd=2) ##  draw v lines
par(new=T)
x1 <- brewer.pal(length(unique(df1$Study)), 'Set3') # grab some colors
cpal <- x1[rep(1:length(unique(df1$Study)), rle(df1$Study)[[1]])]
axis(side=2, at=pretty(p.ylim), labels=pretty(p.ylim), las=1 )
axis(side=1, at=c(50, 70, 90, 110, 130, 150, 170, 190, 210), labels=c("50" , "70", "90", "110", "130", "150", "170", "190", "210")  )
##  line for pbo
par(new=T)
plot( df1[ df1$pchvec ==1 , xval], df1[df1$pchvec ==1 , yval], pch=df1$pchvec[df1$pchvec ==1], col= cpal[df1$pchvec ==1] , bg= df1$pchfill[df1$pchvec ==1], cex=1.5  ,yaxt='n', xaxt='n', ylab="", xlab="", ylim= p.ylim, xlim= p.xlim , lwd=3)
abline(lm(df1$eventrate[df1$pchvec ==1 ]~df1$LDL[df1$pchvec ==1 ]), lwd=2)

par(new=T)
plot( df1[ df1$pchvec ==19 , xval], df1[df1$pchvec ==19 , yval], pch=df1$pchvec[df1$pchvec ==19], col= cpal[df1$pchvec ==19] , bg= df1$pchfill[df1$pchvec ==19], cex=1.5  ,yaxt='n', xaxt='n', ylab="", xlab="", ylim= p.ylim, xlim= p.xlim , lwd=3)
abline(lm(df1$eventrate[df1$pchvec ==19 ]~df1$LDL[df1$pchvec ==19 ]), lwd=2)


legend( "topleft", pch=c(19, 1, NA, NA, NA), lwd=c(NA, NA, 2, 2, 3), lty=c(NA, NA, 1, 2, 3), col=c("black", "black", "black", "red", "darkgreen")  , legend=c("Statin", "Placebo", "OLS", "Quadr", "Spline"), cex=1.2, border= "n", bty='n')
text(df1[, xval], df1[, yval], labels = df1$Study, pos = 3, font=2, col=cpal)
title(main="Event Rates Plotted against LDL Cholesterol Levels\nduring Statin Therapy in Secondary-Prevention Studies 4S to PROVE-IT.", ylab=pyval, xlab=pxval)
abline(lm(df1$eventrate~df1$LDL), lwd=2)
poly.plot <- lm(eventrate~poly(LDL, 2), data=df1)
poly.pred <- predict(poly.plot, df1[xval])
preddf <- data.frame(x = df1[,xval], y=poly.pred)
preddf <- preddf[ order(preddf$x),]
lines(x=preddf$x, y=preddf$y, type="l", col="red", lwd=3, lty=2)


spl.fit <- lm(eventrate~bs(LDL, df=2, degree=1), data=df1) # or with bs you can define number of knots using df= (number of parameters including intercept)
spl.pred <- predict(spl.fit , newdata=df1[xval])
preddf2 <- data.frame(x = df1[,xval], y=spl.pred)
preddf2 <- preddf2[ order(preddf2$x), ]
lines(x=preddf2$x, preddf2$y, col='darkgreen', lty=3, lwd=3)



spl.fit <- lm(eventrate~bs(LDL, df=3, degree=2), data=df1) # or with bs you can define number of knots using df= (number of parameters including intercept)
spl.pred <- predict(spl.fit , newdata=df1[xval])
preddf2 <- data.frame(x = df1[,xval], y=spl.pred)
preddf2 <- preddf2[ order(preddf2$x), ]
lines(x=preddf2$x, preddf2$y, col='orange', lty=4, lwd=3)

# dev.off()


