sbl <- data.frame(Seatbelts)
View(sbl)
summary(sbl)

BeforeLaw <- subset(sbl, law == 0)
AfterLaw <- subset(sbl, law != 0)

par(mfrow=c(1,2))
boxplot(BeforeLaw$DriversKilled, ylim=c(50,200), main="BL")
boxplot(AfterLaw$DriversKilled, ylim=c(50,200), main="AL")

install.packages("glmnet")

x <- model.matrix(DriversKilled~., BeforeLaw)[, -c(1,8)]
y <- BeforeLaw$DriversKilled
RidgeMod <- glmnet(x, y, alpha = 0, nlambda = 100, lambda.min.ratio = 0.0001)
CvRidgeMod <- cv.glmnet(x, y, nlambda = 100, lambda.min.ratio = 0.0001)
plot(CvRidgeMod)
predict(RidgeMod, s = best.lambda, type = "coefficients")[1:6,]
