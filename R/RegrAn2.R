setwd("C:/Repos/R/r-test1/R")
Printers <- read.csv("../data/Chapter02/SellingPrinters.csv", header = T, sep = ";")
plot(Printers$Sold.Items, Printers$Price, cex = 1.3, lwd = 10)
cov(Printers$Sold.Items, Printers$Price)
cor(Printers$Sold.Items, Printers$Price)
LinMod <- lm(Printers$Price ~ Printers$Sold.Items, data = Printers)
summary(LinMod)
abline(LinMod)


VehicleData <- read_excel("../data/Chapter02/VehiclesItaly.xlsx")
getwd()
setwd("R")
y <- VehicleData$Registrations
x <- VehicleData$Population
Alpha <- mldivide(x,y)
plot(x,y)
abline(a=0, b=Alpha)
VehicleRegFit <- c(Alpha)*x
Residual <- y - VehicleRegFit
plot(Residual)
abline(a=0, b=0)
stem(Residual)
Rsq <- 1 - sum((y-VehicleRegFit)^2) / sum((y-mean(y))^2)
x1 <- cbind(x,1)
alpha_beta <-mldivide(x1, y)
VehicleRegFit2 <- x1 %*% alpha_beta
plot(x, y, cex = 1.3, lwd = 10)
abline(a=0, b=Alpha, lwd=5, col="red")
abline(a=alpha_beta[2,], b=alpha_beta[1,], lwd=5, lty=3)
Rsq2 <- 1 - sum((y-VehicleRegFit2)^2) / sum((y-mean(y))^2)


CementData <- read.csv("../data/Chapter03/CementData.csv", header = T, sep = ",")
x <- as.matrix(cbind(rep(1, 13), CementData[,1:4]))
Beta <- solve(t(x)%*%x) %*% t(x) %*% y
HeatRegFit <- x %*% Beta[,1]
Residual <- y -HeatRegFit
plot(Residual, lwd = 8)
abline(a = 0, b = 0, lwd = 4)
Rsq <- 1 - sum((y-HeatRegFit)^2) / sum((y-mean(y))^2)

PetrolData <- read.csv("../data/Chapter03/EscapingHydrocarbons.csv", header = T, sep = ";")
n <- names(PetrolData)
form1 <- as.formula(paste("AmountEscapingHydrocarbons ~", paste(n[!n %in% "AmountEscapingHydrocarbons"], collapse = " + ")))
MLModel <- lm(form1, data = PetrolData)
Pred <- predict(MLModel)
plot(PetrolData[,5], Pred, xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1)

par(mfrow=c(2,2))
plot(MLModel)

################################
EmployeesSalary <- read_excel("../data/Chapter03/employees.xlsx")
EmployeesSalary$LevelOfEmployee <- as.factor(EmployeesSalary$LevelOfEmployee)
pch.list <- as.numeric(EmployeesSalary$LevelOfEmployee)
plot(EmployeesSalary$YearsExperience, EmployeesSalary$Salary, pch = c(pch.list))
LMCat <- lm("Salary ~ YearsExperience * LevelOfEmployee", data = EmployeesSalary)
LMCat.coef <- coef(LMCat)
LMCat.GeneralStaff <- c(LMCat.coef[1], LMCat.coef[2])
LMCat.TechnicalStaff <- c(LMCat.coef[1] + LMCat.coef[4], LMCat.coef[2] + LMCat.coef[6])
LMCat.Management <- c(LMCat.coef[1] + LMCat.coef[3], LMCat.coef[2] + LMCat.coef[5])
abline(coef = LMCat.GeneralStaff, lwd = 2)
abline(coef = LMCat.TechnicalStaff, lwd = 2)
abline(coef = LMCat.Management, lwd = 2)

set_config(use_proxy(url="http://proxy-nic.1msmu.ru", port=3128, username="appolonova_s_a", password="236873"))

############################################################
N <- 10000
d <- 10
set.seed(42)
X <- matrix(rnorm(N*d), ncol = d)
theta <- rep(5, d+1)
eps <- rnorm(N)
y <- cbind(1, X) %*% theta + eps
dat <- data.frame(y=y, x=X)
sgd.theta <- sgd(y ~ ., data = dat, model = "lm")
str(sgd.theta)
sprintf("Mean squared error: %0.3f", mean((theta - as.numeric(sgd.theta$coefficients))^2))
plot(sgd.theta, theta, type = "mse-param")






