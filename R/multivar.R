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

