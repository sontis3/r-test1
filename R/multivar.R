library()
data()
attach(attitude)
head(attitude, n = 10)

mydata <- t(attitude)

# install.packages("mvnormtest")
# library("mvnormtest", lib.loc="~/R/win-library/3.5")
mshapiro.test(mydata)

# install.packages("normtest")
# library("normtest", lib.loc="~/R/win-library/3.5")
jb.norm.test(attitude)

# install.packages("normwhn.test")
# library("normwhn.test", lib.loc="~/R/win-library/3.5")
normality.test1(attitude)


library("readxl", lib.loc="~/R/win-library/3.5")
setwd("./akdata/")
rawData <- read_excel("Metab_new1_uM.xlsx", sheet = "Микромоли")
tData <- t(rawData[,-1:-6])
# tData2 <- log(t(rawData[,-1:-6]))
tData2 <- log(t(rawData[,75:97]))
mshapiro.test(tData2)
jb.norm.test(tData2at)

# install.packages("MVA")
# library("MVA", lib.loc="~/R/win-library/3.5")
cov(measure[,  c("chest",  "waist",  "hips")])
cov(subset(measure,  gender  ==  "female")[, c("chest",  "waist",  "hips")])
cov(subset(measure,  gender  ==  "male")[, c("chest",  "waist",  "hips")])
cor(measure[,  c("chest",  "waist",  "hips")])
dist(scale(measure[,  c("chest",  "waist",  "hips")], center  =  FALSE))
x <- measure[, c("chest", "waist", "hips")]
cm <- colMeans(x)
S <- cov(x)
d <- apply(x, MARGIN = 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm))
qqnorm(measure[,"chest"], main = "chest"); qqline(measure[,"chest"])
qqnorm(measure[,"waist"], main = "waist"); qqline(measure[,"waist"])
qqnorm(measure[,"hips"], main = "hips"); qqline(measure[,"hips"])

###################################################################
# Alboukadel Kassambara - Practical Guide to Cluster Analysis in R. 
# загрузка данных
data("USArrests")
df <- USArrests
df <- na.omit(df)
df.scaled <- scale(df)
dist.eucl <- dist(df.scaled, method = "euclidean")
round(as.matrix(dist.eucl)[1:3, 1:3], 1)

head(df, n = 3)
install.packages("factoextra")
library("factoextra", lib.loc="~/R/win-library/3.5")
dist.cor <- get_dist(df.scaled, method = "pearson")
round(as.matrix(dist.cor)[1:3, 1:3], 1)

library("cluster", lib.loc="C:/Program Files/R/R-3.5.1/library")
data(flower)
head(flower, 3)
dd <- daisy(flower)
round(as.matrix(dd)[1:3,  1:3],  2)

# визуализация расстояний
fviz_dist(dist.eucl)

# Optimal number of clusters
fviz_nbclust(df, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

####################################################
library("readxl", lib.loc="~/R/win-library/3.5")
library("stats")
setwd("c:/Repos/R/r-test1/")
setwd("./akdata/")
rawData <- read_excel("Metab_new1_uM.xlsx", sheet = "Микромоли")
# tData <- t(rawData[,-1:-6])
pcaData2 <- prcomp(rawData[,-1:-6], center = T, scale. = T, rank. = 16)
summary(pcaData2)

df.scaled <- scale(rawData[,-1:-6])
fviz_nbclust(df.scaled, kmeans, method = "wss")



