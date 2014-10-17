
# load libraries
library(RColorBrewer)
library(RCurl)

# set options
options(stringsAsFactors=F, scipen=999)

# statins <- read.csv("~/HSLLC/La_Rossa_line/rawdat/secprevstatin.csv")
# file <- "http://dl.dropboxusercontent.com/u/27644144/secprevstatin.csv"
# # file <- "<a href="http://www.ndbc.noaa.gov/view_text_file.php?filename=44025h2011.txt.gz&dir=data/historical/stdmet/">http://www.ndbc.noaa.gov/view_text_file.php?filename=44025h2011.txt.gz&dir=data/historical/stdmet/</a>"
# statins <- read.csv(file, header=T)

# load data

url <- "https://raw.githubusercontent.com/connerpharmd/LDLCVE/master/secprevstatin.csv"
statins <- getURL(url,  ssl.verifypeer = FALSE)                
statins<- read.csv(textConnection(statins))
statins <- statins[ which(statins$Study != "AtoZ"),]# remove AtoZ


statins$year <- as.numeric(substr(statins$lastrand, 1, 4)) + round(as.numeric(substr(statins$lastrand, 5, 6))/13, 2)
#View(secprevstatin)


library(ggplot2)
qplot(x=LDL, y=eventrate, data=statins, color=Study)


##plot in boring bnw
# df1 <- statins
# df1 <- statins
# yval <- 'eventrate'
# pyval <- 'Event (%)'
# xval <- 'LDL'
# pxval <- 'LDL Cholesterol (mg/dL)'
# 
# 
# df1 <- statins
# df1$pchvec <- ifelse( grepl("PBO", df1$Cohort), 1, 19 ) # plotting character
# df1$pchfill <- ifelse(df1$pchvec == 1, 'white', 'black') # plotting character fill --not sure it works
# x1 <- brewer.pal(length(unique(df1$Study)), 'Set2') # grab some colors
# cpal <- x1[rep(1:length(unique(df1$Study)), rle(df1$Study)[[1]])]
# 
# par(mar=c(5.1, 4.1, 4.1, 12))
# df1$pchvec <- ifelse( grepl("^PBO$", df1$Cohort), 1, 19 )
# df1$pchfill <- ifelse(df1$pchvec == 1, 'white', 'black')
# 
# plot( df1[, xval], df1[, yval], pch=df1$pchvec, col='black', cex=1.5  ,yaxt='n', xaxt='n', ylab="", xlab="", ylim=c(0,30), xlim=c(50,210))
# axis(side=2, at=c(0, 5, 10, 15, 20, 25, 30), labels=c("0","5",'10', '15', '20', '25', '30'), las=1 )
# axis(side=1, at=c(70, 90, 110, 130, 150, 170, 190, 210), labels=c("70", "90", "110", "130", "150", "170", "190", "210")  )
# legend( "topleft", pch=c(19, 1), legend=c("Statin", "Placebo"), cex=1.2, border= "n", bty='n')
# text(df1[, xval], df1[, yval], labels = df1$Study, pos = 3)
# title(main="Figure 4. Event Rates Plotted against LDL Cholesterol Levels\nduring Statin Therapy in Secondary-Prevention Studies.", ylab=pyval, xlab=pxval)
# abline(lm(df1$eventrate~df1$LDL), lwd=2)




df1 <- statins
yval <- 'eventrate'
pyval <- 'Event (%)'
xval <- 'LDL'
pxval <- 'LDL Cholesterol (mg/dL)'

##  slightly tweaked (I think this just looks better)
ppi <- 300
png(paste('tweakedtntplot.png', sep="_"), width=10*ppi, height=6*ppi, res=ppi)
par(mar=c(5.1, 4.1, 4.1, 4))
df1$pchvec <- ifelse( grepl("PBO", df1$Cohort), 1, 19 )
plot( df1[, xval], df1[, yval], type="n", ylim=c(0,30), xlim=c(40,210), yaxt='n', xaxt='n', ylab="", xlab="")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = "grey88", border = "black")
par(new=T)
abline(h = c(0, 5, 10, 15, 20, 25, 30), col='white', lwd=2) ##  draw h lines
abline(v = c(50,70, 90, 110, 130, 150, 170, 190, 210), col='white', lwd=2) ##  draw v lines
par(new=T)
x1 <- brewer.pal(length(unique(df1$Study)), 'Set2') # grab some colors
cpal <- x1[rep(1:length(unique(df1$Study)), rle(df1$Study)[[1]])]
plot( df1[, xval], df1[, yval], pch=df1$pchvec, col= cpal , bg= df1$pchfill, cex=1.5  ,yaxt='n', xaxt='n', ylab="", xlab="", ylim=c(0,30), xlim=c(40,210), lwd=2)
axis(side=2, at=c(0, 5, 10, 15, 20, 25, 30), labels=c("0","5",'10', '15', '20', '25', '30'), las=1 )
axis(side=1, at=c(50, 70, 90, 110, 130, 150, 170, 190, 210), labels=c("50" , "70", "90", "110", "130", "150", "170", "190", "210")  )
legend( "topleft", pch=c(19, 1, NA, NA), lwd=c(NA, NA, 2, 2), lty=c(NA, NA, 1, 2), col=c("black", "black", "black", "red")  , legend=c("Statin", "Placebo", "OLS", "Quadr"), cex=1.2, border= "n", bty='n')
text(df1[, xval], df1[, yval], labels = df1$Study, pos = 3, font=2, col=cpal)
title(main="Event Rates Plotted against LDL Cholesterol Levels\nduring Statin Therapy in Secondary-Prevention Studies 4S to PROVE-IT.", ylab=pyval, xlab=pxval)
abline(lm(df1$eventrate~df1$LDL), lwd=2)
poly.plot <- lm(eventrate~poly(LDL, 2), data=df1)
poly.pred <- predict(poly.plot, df1[xval])
preddf <- data.frame(x = df1[,xval], y=poly.pred)
preddf <- preddf[ order(preddf$x),]
lines(x=preddf$x, y=preddf$y, type="l", col="red", lwd=3, lty=2)
dev.off()




df1 <- statinsno4s
yval <- 'eventrate'
pyval <- 'Event (%)'
xval <- 'LDL'
pxval <- 'LDL Cholesterol (mg/dL)'

##  slightly tweaked (I think this just looks better)
ppi <- 300
png(paste('tweakedtntplot.png', sep="_"), width=10*ppi, height=6*ppi, res=ppi)
par(mar=c(5.1, 4.1, 4.1, 4))
df1$pchvec <- ifelse( grepl("PBO", df1$Cohort), 1, 19 )
plot( df1[, xval], df1[, yval], type="n", ylim=c(0,30), xlim=c(40,210), yaxt='n', xaxt='n', ylab="", xlab="")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = "grey88", border = "black")
par(new=T)
abline(h = c(0, 5, 10, 15, 20, 25, 30), col='white', lwd=2) ##  draw h lines
abline(v = c(50,70, 90, 110, 130, 150, 170, 190, 210), col='white', lwd=2) ##  draw v lines
par(new=T)
x1 <- brewer.pal(length(unique(df1$Study)), 'Set2') # grab some colors
cpal <- x1[rep(1:length(unique(df1$Study)), rle(df1$Study)[[1]])]
plot( df1[, xval], df1[, yval], pch=df1$pchvec, col= cpal , bg= df1$pchfill, cex=1.5  ,yaxt='n', xaxt='n', ylab="", xlab="", ylim=c(0,30), xlim=c(40,210), lwd=2)
axis(side=2, at=c(0, 5, 10, 15, 20, 25, 30), labels=c("0","5",'10', '15', '20', '25', '30'), las=1 )
axis(side=1, at=c(50, 70, 90, 110, 130, 150, 170, 190, 210), labels=c("50" , "70", "90", "110", "130", "150", "170", "190", "210")  )
legend( "topleft", pch=c(19, 1, NA, NA), lwd=c(NA, NA, 2, 2), lty=c(NA, NA, 1, 2), col=c("black", "black", "black", "red")  , legend=c("Statin", "Placebo", "OLS", "Quadr"), cex=1.2, border= "n", bty='n')
text(df1[, xval], df1[, yval], labels = df1$Study, pos = 3, font=2, col=cpal)
title(main="Event Rates Plotted against LDL Cholesterol Levels\nduring Statin Therapy in Secondary-Prevention Studies CARE to PROVE-IT.", ylab=pyval, xlab=pxval)
abline(lm(df1$eventrate~df1$LDL), lwd=2)
poly.plot <- lm(eventrate~poly(LDL, 2), data=df1)
poly.pred <- predict(poly.plot, df1[xval])
preddf <- data.frame(x = df1[,xval], y=poly.pred)
preddf <- preddf[ order(preddf$x),]
lines(x=preddf$x, y=preddf$y, type="l", col="red", lwd=3, lty=2)
dev.off()




unique(statins$Study[ order(statins$lastrand)])


# ppi <- 300
# png(paste('Rtntplot.png', sep="_"), width=10*ppi, height=6*ppi, res=ppi)
# ##  slightly tweaked
# par(mar=c(5.1, 4.1, 4.1, 4))
# df1$pchvec <- ifelse( grepl("PBO", df1$Cohort), 1, 19 )
# plot( df1[, xval], df1[, yval], type="n", ylim=c(0,30), xlim=c(50,210), yaxt='n', xaxt='n', ylab="", xlab="")
# u <- par("usr")
# rect(u[1], u[3], u[2], u[4], col = "grey88", border = "black")
# par(new=T)
# abline(h = c(0, 5, 10, 15, 20, 25, 30), col='white', lwd=2) ##  draw h lines
# abline(v = c(50, 70, 90, 110, 130, 150, 170, 190, 210), col='white', lwd=2) ##  draw v lines
# par(new=T)
# plot( df1[, xval], df1[, yval], pch=df1$pchvec, col= cpal , bg= df1$pchfill, cex=1.5  ,yaxt='n', xaxt='n', ylab="", xlab="", ylim=c(0,30), xlim=c(70,210), lwd=2)
# axis(side=2, at=c(0, 5, 10, 15, 20, 25, 30), labels=c("0","5",'10', '15', '20', '25', '30'), las=1, cex=1.2 )
# axis(side=1, at=c(50,70, 90, 110, 130, 150, 170, 190, 210), labels=c("70", "90", "110", "130", "150", "170", "190", "210"), cex=1.2 )
# legend( "topleft", pch=c(19, 1), legend=c("Statin", "Placebo"), cex=1.2, border= "n", bty='n')
# text(df1[, xval], df1[, yval], labels = df1$Study, pos = 3, font=2, col=cpal)
# title(main="Figure 4. Event Rates Plotted against LDL Cholesterol Levels\nduring Statin Therapy in Secondary-Prevention Studies.", ylab=pyval, xlab=pxval, cex.lab=1.2, font=2)
# abline(lm(df1$eventrate~df1$LDL), lwd=2)
# dev.off()
# 
# poly<- (lm(eventrate~poly(LDL, 2), data=df1))
# yhat <- predict(poly, df1["LDL"])
# preddf <- data.frame(x=df1[,"LDL"], y=yhat)
# preddf <- preddf[ order(preddf$x),]
# lines(x=preddf$x, y=preddf$y, type="l", col="red", lwd=3, lty=2)




load("~/statins_2014_10_16.RData")
statinsno4s <- statins[ which(statins$Study != "4S"), ] ## consolidate all code and run all test models at same time

#models
lm1 <-lm(eventrate~LDL, data=statins)
poly1 <- (lm(eventrate~LDL+I(LDL^2), data=statins))
lm2 <- lm(eventrate~LDL, data=statinsno4s)
poly2 <- (lm(eventrate~LDL+I(LDL^2), data=statinsno4s))

# predicted values
lm1pred <- predict(lm1, statins)
poly1pred <- predict(poly1, statins)
lm2pred <- predict(lm2, statinsno4s)
poly2pred <- predict(poly2, statinsno4s)

# rmse
lm1rmse <- sqrt(mean(lm1$residuals^2))
lm2rmse <- sqrt(mean(lm2$residuals^2))
poly1rmse <- sqrt(mean(poly1$residuals^2))
poly2rmse <- sqrt(mean(poly2$residuals^2))

#predictions of 50mg/dL
lm1p50 <- predict(lm1, data.frame(LDL = 50))
lm2p50 <- predict(lm2, data.frame(LDL = 50))
poly1p50 <- predict(poly1, data.frame(LDL = 50))
poly2p50 <- predict(poly2, data.frame(LDL = 50))


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


## Run 5-fold cross validation to test model fit 

kf <- 10
## lm1
set.seed(11)
folds <- sample(rep(1:kf, length=nrow(statins)))
cv.rmse=(rep(NA,kf))
for(k in 1:kf){
  fit=lm(eventrate~LDL, data=statins[folds!= k,])
    plot.pred <- predict(fit, data=statins[folds!= k,])
    preddf <- data.frame(x = statins[folds!= k,]["LDL"], y=plot.pred)
    preddf <- preddf[ order(preddf$y),]
    plot(statins[folds!= k,]$eventrate~statins[folds!= k,]$LDL)
    lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,statins[folds==k,])
  cv.mse = mean((statins$eventrate[folds==k]-pred)^2)
  cv.rmse[k] <- sqrt(cv.mse)
}
lm1cvrmse <- mean(cv.rmse)

# lm2
set.seed(12)
folds <- sample(rep(1:kf, length=nrow(statinsno4s)))
cv.rmse=(rep(NA,kf))
for(k in 1:kf){
  fit=lm(eventrate~LDL, data=statinsno4s[folds!= k,])
  #   abline(fit)
  pred=predict(fit,statinsno4s[folds==k,])
  cv.mse = mean((statinsno4s$eventrate[folds==k]-pred)^2)
  cv.rmse[k] <- sqrt(cv.mse)
}
lm2cvrmse <- mean(cv.rmse)



plot(statins$eventrate~statins$LDL)
## poly1
set.seed(14)
folds <- sample(rep(1:kf, length=nrow(statins)))
cv.rmse=(rep(NA,kf))
for(k in 1:kf){
  fit=lm(eventrate~poly(LDL,2), data=statins[folds!= k,])
  plot.pred <- predict(fit, data=statins[folds!= k,])
  preddf <- data.frame(x = statins[folds!= k,]["LDL"], y=plot.pred)
  preddf <- preddf[ order(preddf$y),]
  plot(statins[folds!= k,]$eventrate~statins[folds!= k,]$LDL)
  lines(x=preddf$LDL, y=preddf$y, type="l", col="red", lwd=3, lty=2)
  pred=predict(fit,statins[folds==k,])
  cv.mse = mean((statins$eventrate[folds==k]-pred)^2)
  cv.rmse[k] <- sqrt(cv.mse)
}
poly1cvrmse <- mean(cv.rmse)


## lm1
set.seed(14)
folds <- sample(rep(1:kf, length=nrow(statinsno4s)))
cv.rmse=(rep(NA,kf))
for(k in 1:kf){
  fit=lm(eventrate~LDL+I(LDL^2), data=statinsno4s[folds!= k,])
  #   abline(fit)
  pred=predict(fit,statinsno4s[folds==k,])
  cv.mse = mean((statinsno4s$eventrate[folds==k]-pred)^2)
  cv.rmse[k] <- sqrt(cv.mse)
}
poly2cvrmse <- mean(cv.rmse)




#___________________________________________________________________________________________
#___________________________________________________________________________________________
#___________________________________________________________________________________________
##  OLD CODE BELOW THIS SECTOIN 
#___________________________________________________________________________________________
#___________________________________________________________________________________________
#___________________________________________________________________________________________


##
##  compare rmse for poly vs lm  on LDL alone
##

lmmod <- lm(eventrate~LDL, data=statins)
poly<- (lm(eventrate~poly(LDL, 2, raw = TRUE), data=statins))


yhat <- predict(poly, statins)
lmpred <- predict(lmmod, statins)

rmse.linear <- sqrt(mean((statins$eventrate-lmpred)^2))
rmse.quad <- sqrt(mean((statins$eventrate-yhat)^2))
rmse.linear
rmse.quad


## compute bootstrapped CIs for any model
myfunct = function(data){
  funlm <- lm(eventrate~LDL, data=data[  sample(1:nrow(data), replace = TRUE), ])
  c(int=funlm$coefficients[1], LDL1=funlm$coefficients[2])
}

myfunct(statins)

lmboot <- replicate(1000, myfunct(statins) )
dim(lmboot)

##
##  predict event rates at LDL 50 and 40 with linear vs. poly
##

lmp50 <- predict(lmmod, data.frame(LDL = 50)) 
lmp40 <- predict(lmmod, data.frame(LDL = 40))

qdp50 <- predict(poly, data.frame(LDL = 50)) 
qdp40 <- predict(poly, data.frame(LDL = 40))







##
##  5 fold cross validation
##





##  
##...need to walk through to ensure these are correct
##
set.seed(11)
folds <- sample(rep(1:5, length=nrow(statins)))
cv.rmse=(rep(NA,5))
for(k in 1:5){
  fit=lm(eventrate~poly(LDL, 2)+year, data=statins[folds!= k,])
#   abline(fit)
  pred=predict(fit,statins[folds==k,])
  cv.mse = mean((statins$eventrate[folds==k]-pred)^2)
  cv.rmse[k] <- sqrt(cv.mse)
}
cv.rmse_poly <- mean(cv.rmse)



set.seed(11)
folds <- sample(rep(1:5, length=nrow(statins)))
cv.rmse=(rep(NA,5))
for(k in 1:5){
  fit=lm(eventrate~LDL+year, data=statins[folds!= k,])
  pred=predict(fit,statins[folds==k,])
  cv.mse = mean((statins$eventrate[folds==k]-pred)^2)
  cv.rmse[k] <- sqrt(cv.mse)
}
cv.rmse_lin <- mean(cv.rmse)

cv.rmse_poly
cv.rmse_lin


##
##  Perhaps should run a 3 variable model with year of last rand included (to see what that does)
##










## indication that 4s reprsents high leverage data in our case
lmcook <- cooks.distance(lmmod)
plot(lmcook)
# text(x=1:length(lmcook),y=lmcook, labels=1:length(lmcook), pos=1)
text(x=1:length(lmcook),y=lmcook, labels=statins$Study, pos=4, cex=.7)
abline(h=4/nrow(statins), col="red", lty=2)
title(main="Cooks distance for linear function with 4/n threshold")
library(MASS)
rlm <- stdres(lmmod)
##points with high cooks distance values
lmcook[ lmcook > 4/nrow(statins)]


polycook <- cooks.distance(poly)
plot(polycook)
# text(x=1:length(polycook),y=polycook, labels=1:length(polycook), pos=3)
text(x=1:length(polycook),y=polycook, labels=statins$Study, pos=4, cex=.7)
abline(h=4/nrow(statins), col="red", lty=2)
title(main="Cooks distance for polynomial function with 4/n threshold")
rpoly <- stdres(poly)
##points with high cooks distance values
polycook[ polycook > 4/nrow(statins)]



##
##  recheck rmse with 4s removed
##

statinsno4s <- statins[ which(statins$Study != "4S"), ]
poly<- (lm(eventrate~poly(LDL, 2), data=statinsno4s))
lmmod <- lm(eventrate~LDL, data=statinsno4s)

yhat <- predict(poly, statinsno4s)
lmpred <- predict(lmmod, statinsno4s)

rmse1 <- sqrt(mean((statinsno4s$eventrate-lmpred)^2))
rmse2 <- sqrt(mean((statinsno4s$eventrate-yhat)^2))
rmse1
rmse2

lmp50 <- predict(lmmod, data.frame(LDL = 50)) 
lmp40 <- predict(lmmod, data.frame(LDL = 40))

qdp50 <- predict(poly, data.frame(LDL = 50)) 
qdp40 <- predict(poly, data.frame(LDL = 40))


##
##  replot with 4s removed
##



require(RColorBrewer)
df1 <- statinsno4s
yval <- 'eventrate'
pyval <- 'Event (%)'
xval <- 'LDL'
pxval <- 'LDL Cholesterol (mg/dL)'

##  slightly tweaked (I think this just looks better)
ppi <- 300
# png(paste('tweakedtntplot.png', sep="_"), width=10*ppi, height=6*ppi, res=ppi)
par(mar=c(5.1, 4.1, 4.1, 4))
df1$pchvec <- ifelse( grepl("PBO", df1$Cohort), 1, 19 )
plot( df1[, xval], df1[, yval], type="n", ylim=c(0,30), xlim=c(40,210), yaxt='n', xaxt='n', ylab="", xlab="")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = "grey88", border = "black")
par(new=T)
abline(h = c(0, 5, 10, 15, 20, 25, 30), col='white', lwd=2) ##  draw h lines
abline(v = c(50,70, 90, 110, 130, 150, 170, 190, 210), col='white', lwd=2) ##  draw v lines
par(new=T)
x1 <- brewer.pal(length(unique(df1$Study)), 'Set2') # grab some colors
cpal <- x1[rep(1:length(unique(df1$Study)), rle(df1$Study)[[1]])]
plot( df1[, xval], df1[, yval], pch=df1$pchvec, col= cpal , bg= df1$pchfill, cex=1.5  ,yaxt='n', xaxt='n', ylab="", xlab="", ylim=c(0,30), xlim=c(40,210), lwd=2)
axis(side=2, at=c(0, 5, 10, 15, 20, 25, 30), labels=c("0","5",'10', '15', '20', '25', '30'), las=1 )
axis(side=1, at=c(50, 70, 90, 110, 130, 150, 170, 190, 210), labels=c("50" , "70", "90", "110", "130", "150", "170", "190", "210")  )
legend( "topleft", pch=c(19, 1, NA, NA), lwd=c(NA, NA, 2, 2), lty=c(NA, NA, 1, 2), col=c("black", "black", "black", "red")  , legend=c("Statin", "Placebo", "OLS", "Quadr"), cex=1.2, border= "n", bty='n')
text(df1[, xval], df1[, yval], labels = df1$Study, pos = 3, font=2, col=cpal)
title(main="Event Rates Plotted against LDL Cholesterol Levels\nduring Statin Therapy in Secondary-Prevention Studies 4S to PROVE-IT.", ylab=pyval, xlab=pxval)
abline(lm(df1$eventrate~df1$LDL), lwd=2)
poly.plot <- lm(eventrate~poly(LDL, 2), data=df1)
poly.pred <- predict(poly.plot, df1[xval])
preddf <- data.frame(x = df1[,xval], y=poly.pred)
preddf <- preddf[ order(preddf$x),]
lines(x=preddf$x, y=preddf$y, type="l", col="red", lwd=3, lty=2)
# dev.off()

















##
##  compare rmse for poly vs lm  on LDL + last year of randomization
##


statins$year <- as.numeric(substr(statins$lastrand, 1, 4)) + round(as.numeric(substr(statins$lastrand, 5, 6))/13, 2)

poly<- (lm(eventrate~poly(LDL, 3)+year, data=statins))
lmmod <- lm(eventrate~LDL+year, data=statins)


## indication that 4s reprsents high leverage data in our case
lmcook <- cooks.distance(lmmod)
plot(lmcook)
text(x=1:length(lmcook),y=lmcook, labels=1:length(lmcook), pos=3)
abline(h=4/nrow(statins), col="red", lty=2)
library(MASS)
rlm <- stdres(lmmod)
##points with high cooks distance values
lmcook[ lmcook > 4/nrow(statins)]

polycook <- cooks.distance(poly)
plot(polycook)
text(x=1:length(polycook),y=polycook, labels=1:length(polycook), pos=3)
abline(h=4/nrow(statins), col="red", lty=2)
rpoly <- stdres(poly)
##points with high cooks distance values
polycook[ polycook > 4/nrow(statins)]

yhat <- predict(poly, statins)
lmpred <- predict(lmmod, statins)

rmse1 <- sqrt(mean((statins$eventrate-lmpred)^2))
rmse2 <- sqrt(mean((statins$eventrate-yhat)^2))




##
##  Now re-re check model fit with 4s removed
##



poly<- (lm(eventrate~poly(LDL, 3)+year, data=statinsno4s))
lmmod <- lm(eventrate~LDL+year, data=statinsno4s)


## indication that 4s reprsents high leverage data in our case
# lmcook <- cooks.distance(lmmod)
# plot(lmcook)
# text(x=1:length(lmcook),y=lmcook, labels=1:length(lmcook), pos=3)
# abline(h=4/nrow(statinsno4s), col="red", lty=2)
# library(MASS)
# rlm <- stdres(lmmod)
# ##points with high cooks distance values
# lmcook[ lmcook > 4/nrow(statinsno4s)]
# 
# polycook <- cooks.distance(poly)
# plot(polycook)
# text(x=1:length(polycook),y=polycook, labels=1:length(polycook), pos=3)
# abline(h=4/nrow(statinsno4s), col="red", lty=2)
# rpoly <- stdres(poly)
# ##points with high cooks distance values
# polycook[ polycook > 4/nrow(statinsno4s)]

yhat <- predict(poly, statinsno4s)
lmpred <- predict(lmmod, statinsno4s)

rmse1 <- sqrt(mean((statinsno4s$eventrate-lmpred)^2))
rmse2 <- sqrt(mean((statinsno4s$eventrate-yhat)^2))
















##
##  Now try with cubic regression
##



poly2 <- lm(eventrate~poly(LDL, 3)+poly(year,3), data=statins)
lmmod2 <- lm(eventrate~LDL, data=statins)

yhat <- predict(poly, statins)
lmpred <- predict(lmmod, statins)
yhat2 <- predict(poly2, statins)
yhat3 <- predict(lmmod2, statins)

rmse1 <- sqrt(mean((statins$eventrate-lmpred)^2))
rmse2 <- sqrt(mean((statins$eventrate-yhat)^2))
rmse3 <- sqrt(mean((statins$eventrate-yhat2)^2))
rmse4 <- sqrt(mean((statins$eventrate-yhat3)^2))

predict(lmmod, newdata=data.frame(LDL = 113, year = 1991.92))
predict(poly, newdata=data.frame(LDL = 113, year = 2014.92))

plot(statins$eventrate, yhat)
plot(poly$residuals)
poly$residuals
statins[9,]
yhat[9]




##
##  re check to see if 4s has high cooks distance after putting in year as a feature
##





##  
##...need to walk through to ensure these are correct
##
set.seed(11)
folds <- sample(rep(1:5, length=nrow(statins2)))
cv.rmse=(rep(NA,10))
for(k in 1:5){
  fit=lm(eventrate~poly(LDL, 2)+year, data=statins2[folds!= k,])
  abline(fit)
  pred=predict(fit,statins2[folds==k,])
  cv.mse = mean((statins2$eventrate[folds==k]-pred)^2)
  cv.rmse[k] <- sqrt(cv.mse)
}
cv.rmse_poly <- mean(cv.rmse)



set.seed(11)
folds <- sample(rep(1:10, length=nrow(statins2)))
cv.rmse=(rep(NA,10))
for(k in 1:10){
  fit=lm(eventrate~LDL+year, data=statins2[folds!= k,])
  pred=predict(fit,statins2[folds==k,])
  cv.mse = mean((statins2$eventrate[folds==k]-pred)^2)
  cv.rmse[k] <- sqrt(cv.mse)
}
cv.rmse_lin <- mean(cv.rmse)




table(folds)






preddf <- data.frame(x=df1[,"LDL"], y=yhat)
preddf <- preddf[ order(preddf$x),]
lines(x=preddf$x, y=preddf$y, type="l", col="red", lwd=3, lty=2)































df1 <- statins
yval <- 'eventrate'
pyval <- 'EVENT RATE (%)'
xval <- 'LDL'
pxval <- 'LDL (mg/dL)'

ppi <- 300
# png(paste(client,targetclass, 'pp_byMBRSHP_bymo.png', sep="_"), width=10*ppi, height=6*ppi, res=ppi)
# par(mar=c(5.1, 4.1, 4.1, 9.2))
ymax <-  max( df1[c(yval)])+(0.1* max( df1[c(yval)]))
ymin <-  min( df1[c(yval)])-(0.1* min( df1[c(yval)]))
xmax <-  max( df1[c(xval)])+(0.1* max( df1[c(xval)]))
xmin <-  min( df1[c(xval)])-(0.1* min( df1[c(xval)]))
library(RColorBrewer)
# cpal <- brewer.pal(length(ornamevec), 'Set2')
plot( df1[, xval], df1[,yval],yaxt='n', xaxt='n', ylab="", xlab="", ylim=c(ymin, ymax), xlim=c(xmin, xmax))
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = "gray88", border = "black")
par(new=T)
abline(h = pretty(df1[, yval]), col='white', lwd=2) ##  draw h lines
abline(v = pretty(df1[, xval]), col='white', lwd=2) ##  draw v lines
par(new=T)
listI <- unique(df1$Study)
cpal <- brewer.pal(length(listI), 'Set2')
pchI <- 13+seq(1:length(listI))
for (i in 1:length(listI)){
  listi <- listI[i]
  plot( df1[grep(listi, df1$Study), xval], df1[grep(listi, df1$Study), yval], pch=pchI[i], col=cpal[i]  ,yaxt='n', xaxt='n', ylab="", xlab="", ylim=c(ymin, ymax), xlim=c(xmin, xmax))
  if (i < length(listI))
    par(new=T)
}
##draw OLS for total
xtail <- tail(df1, n=12)
new <- data.frame(yearmon=xtail$yearmon) ##  dummy dataset for prediction line
lines(new$yearmon, predict(lm(xtail$TOTAL~xtail$yearmon), new), col='black', lty=3, lwd=2)  ##  draws line onlty throught the last 12 mos
tests <- summary(lm(xtail$TOTAL~xtail$yearmon))
tslp12 <- fmt.doll(round(tests[[4]][2,1],2)) ##  slope increase over 12 months

xtail <- tail(df1, n=12)
new <- data.frame(yearmon=xtail$yearmon) ##  dummy dataset for prediction line
lines(new$yearmon, predict(lm(xtail$CLOSED~xtail$yearmon), new), col='black', lty=3, lwd=2)  ##  draws line onlty throught the last 12 mos
tests <- summary(lm(xtail$CLOSED~xtail$yearmon))
clslp12 <- fmt.doll(round(tests[[4]][2,1],1)) ##  slope increase over 12 months

axis(side=2, at= pretty(range(ymin, ymax)), labels=fmt.doll(pretty(range(ymin, ymax))), cex.axis=.75)
title( main=paste(client, ptargetclass ,'Plan Pay by Membership by Month\n from', yrange))
legend(xpd=TRUE,'right', inset=c(-0.18,0), legend=ornamevec, lwd=rep(2, length(ornamevec)), pch=rep(1, length(ornamevec)), col=cpal, lty= 1:length(ornamevec) ,title="LEGEND", bty='n' , cex=.8)
title(sub = paste("Average growth in Plan Pay for TOTAL MEMBERSHIP =", (tslp12), "$/yr"), line = 2, cex.sub=.75)
title(sub = paste("Average growth in Plan Pay for CLOSED MEMBERSHIP =", (clslp12), "$/yr"), line = 3, cex.sub=.75)
# dev.off()




df1 <- statins
yval <- 'eventrate'
pyval <- 'EVENT RATE (%)'
xval <- 'LDL'
pxval <- 'LDL (mg/dL)'
# fit it with a polynomial (quadratic)
plot( df1[, xval], df1[, yval], pch=19, col='black'  ,yaxt='n', xaxt='n', ylab="", xlab="")
abline(lm(df1$eventrate~df1$LDL), lwd=3)
poly<- (lm(eventrate~poly(LDL, 2), data=df1))
yhat <- predict(poly, df1["LDL"])
preddf <- data.frame(x=df1[,"LDL"], y=yhat)
preddf <- preddf[ order(preddf$x),]
lines(x=preddf$x, y=preddf$y, type="l", col="red", lwd=3, lty=2)
lmmod <- (lm(df1$eventrate~df1$LDL))
lmpred <- predict(lmmod,df1["LDL"])
rmse1 <- sqrt(mean((df1$eventrate-lmpred)^2))
rmse2 <- sqrt(mean((df1$eventrate-yhat)^2))




# fit it with a sample weighted ls and sample weighted quadratic 
x1 <- statins$n
newvec <- rep(NA, sum(x1))
for(i in 1:length(x1)) {
  if (i ==1){
    start <- 1
  } else{ 
      start <- sum(!is.na(newvec))+1
  }
  tempvec <- rep(i, x1[i])
  newvec[start:(start+length(tempvec)-1)] <- tempvec
}

sampwstatins <- statins[newvec,]


df1 <- sampwstatins
yval <- 'eventrate'
pyval <- 'EVENT RATE (%)'
xval <- 'LDL'
pxval <- 'LDL (mg/dL)'

plot( (df1[, xval]), (df1[, yval]), pch=19, col='black'  ,yaxt='n', xaxt='n', ylab="", xlab="")
abline(lm(df1$eventrate~df1$LDL), lwd=3)
poly<- (lm(eventrate~poly(LDL, 2), data=df1))
yhat <- predict(poly, df1["LDL"])
preddf <- data.frame(x=df1[,"LDL"], y=yhat)
preddf <- preddf[ order(preddf$x),]
lines(x=preddf$x, y=preddf$y, type="l", col="red", lwd=3, lty=2)

lmmod <- lm(df1$eventrate~df1$LDL)
lmpred <- predict(lmmod,df1["LDL"])
rmse1 <- sqrt(mean((df1$eventrate-lmpred)^2))
rmse2 <- sqrt(mean((df1$eventrate-yhat)^2))


  
