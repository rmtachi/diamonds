#### diamond price daimonds analysis ####
## November 5, 2013

library(MASS)

par(mfrow=c(2,2), mar=c(3,3,1,0), mgp=c(2,1,0))

attach(daimonds)
plot(carat, cost)
plot(colour, cost)
plot(clarity, cost)
plot(certificate, cost)

par(mfrow=c(2,1))
plot(carat, log(cost))
plot(log(carat), log(cost))

## naive fit (data clearly non-linear)
fit1 <- lm(cost ~ carat)
summary(fit1)
par(mfrow=c(2,2))
plot(carat, cost)
abline(fit1)
plot(carat, fit1$residuals); abline(h=0)
plot(cost, fit1$residuals); abline(h=0)
qqnorm(fit1$residuals); qqline(fit1$residuals)


## log-log regression
fit2 <- lm(log(cost) ~ log(carat))
summary(fit2)
par(mfrow=c(2,2))
plot(log(carat), log(cost))
abline(fit2)
plot(log(carat), fit2$residuals); abline(h=0)
plot(log(cost), fit2$residuals); abline(h=0)
qqnorm(fit2$residuals); qqline(fit2$residuals)
shapiro.test(fit2$residuals)

## other terms included
daimonds$lcarat <- log(daimonds$carat)

## fit with factor colour & clarity (polynomial constrasts)
fit3 <- lm(log(cost) ~ lcarat+colour+clarity+certificate+store, data=daimonds)
summary(fit3)
## model suggests up to quartic function is significant for clarity


par(mfrow=c(2,2))
plot(daimonds$lcarat, log(daimonds$cost))
abline(fit3)
plot(daimonds$lcarat, fit3$residuals); abline(h=0)
plot(log(daimonds$cost), fit3$residuals); abline(h=0)
qqnorm(fit3$residuals); qqline(fit3$residuals)
shapiro.test(fit3$residuals)

daimonds$ncolour <- as.numeric(daimonds$colour)
daimonds$nclarity <- as.numeric(daimonds$clarity)

## fit with linear (non-factor) colour and clarity
fit4 <- lm(log(cost) ~ lcarat + ncolour + nclarity + certificate + store,
           data=daimonds)
summary(fit4)

par(mfrow=c(2,2))
plot(daimonds$lcarat, log(daimonds$cost))
lines(sort(daimonds$lcarat), fit4$fitted[order(daimonds$lcarat)], col='blue')
plot(daimonds$lcarat, fit4$residuals); abline(h=0)
plot(log(daimonds$cost), fit4$residuals); abline(h=0)
qqnorm(fit4$residuals); qqline(fit4$residuals)
shapiro.test(fit4$residuals)

## fit with higher order polynomial terms
daimonds$ncolour2 <- daimonds$ncolour^2; daimonds$ncolour3 <- daimonds$ncolour^3
daimonds$ncolour4 <- daimonds$ncolour^4; daimonds$ncolour5 <- daimonds$ncolour^5
daimonds$ncolour6 <- daimonds$ncolour^6
daimonds$nclarity2 <- daimonds$nclarity^2; daimonds$nclarity3 <- daimonds$nclarity^3
daimonds$nclarity4 <- daimonds$nclarity^4; daimonds$nclarity5 <- daimonds$nclarity^5
daimonds$nclarity6 <- daimonds$nclarity^6; daimonds$nclarity7 <- daimonds$nclarity^7

fit5 <- lm(log(cost) ~ lcarat + ncolour + ncolour2 + ncolour3 + 
             ncolour4 + ncolour5 + ncolour6 + nclarity + 
             nclarity2 + nclarity3 + nclarity4 + nclarity5 +
             nclarity6 + nclarity7 + certificate + store, data=daimonds)

fit5r <- stepAIC(fit5, direction='backward', k=log(nrow(daimonds)))

summary(fit5r)
## re-include lower polynomial orders
fit6 <- lm(log(cost) ~ lcarat + ncolour + ncolour2 + ncolour3 + 
             ncolour4 + ncolour5 + nclarity + nclarity2 + nclarity3 + 
             nclarity4  + certificate + store, data=daimonds)
summary(fit6)

par(mfrow=c(2,2))
plot(daimonds$lcarat, log(daimonds$cost))
lines(sort(daimonds$lcarat), fit6$fitted[order(daimonds$lcarat)], col='blue')
plot(daimonds$lcarat, fit6$residuals); abline(h=0)
plot(log(daimonds$cost), fit6$residuals); abline(h=0)
qqnorm(fit6$residuals); qqline(fit6$residuals)
shapiro.test(fit6$residuals)



#####
par(mfrow=c(1,1))
plot(daimonds$carat, daimonds$cost, xlab='carat', ylab='cost')
abline(fit1, lwd=2)

new <- daimonds.frame(carat = seq(0.2,2.6, by=0.1) )
pred <- predict.lm(fit1, new, interval='prediction')
lines(new$carat, pred[,3], lty='dotted', lwd=2, col='blue')
lines(new$carat, pred[,2], lty='dotted', lwd=2, col='red')

pred <- predict.lm(fit2, new, interval='prediction')
lines(new$carat, exp(pred[,1]), lwd=2, col='brown')
lines(new$carat, exp(pred[,2]), col='green', lwd=2, lty='dashed')
lines(new$carat, exp(pred[,3]), col='purple', lwd=2, lty='dashed')


