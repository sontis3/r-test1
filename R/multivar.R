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

