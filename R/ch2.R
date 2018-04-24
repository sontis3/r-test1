# findud <- function(v) {
#   vud <- diff(v)
#   return(ifelse(vud > 0, 1, -1))
# }

udcorr <- function(x, y) {
  # ud <- lapply(list(x,y), findud)
  return(mean(sign(diff(x)) == sign(diff(y))))
}

x <- c(5,12,13,3,6,0,1,15,16,8,88)
y <- c(4,2,3,23,6,10,11,12,6,3,2)
udcorr(x,y)

aba <- read.csv("data/abalone.data",header=T,as.is=T)
# grps <- list()
# for (gen in c("M","F")) grps[[gen]] <- which(aba==gen)
# abam <- aba[grps$M,]
# abaf <- aba[grps$F,]
# plot(abam$Length,abam$Diameter)
# plot(abaf$Length,abaf$Diameter,pch="x",new=FALSE)

pchvec <- ifelse(aba$Gender == "M","o","x")
plot(aba$Length,aba$Diameter,pch=pchvec)

blurpart <- function(img,rows,cols,q) {
  lrows <- length(rows)
  lcols <- length(cols)
  newimg <- img
  randomnoise <- matrix(nrow=lrows, ncol=lcols,runif(lrows*lcols))
  newimg@grey[rows, cols] <- (1-q) * img@grey[rows, cols] + q * randomnoise
  return(newimg)
}

makecov <- function(rho, n) {
  m <- matrix(nrow = n, ncol = n)
  m <- ifelse(row(m) == col(m), 1, rho)
  return(m)
}

mind <- function(d) {
  n <- nrow(d)
  # add a column to identify row number for apply()
  dd <- cbind(d, 1:n)
  wmins <- apply(dd[-n,], 1, imin)
  # wmins will be 2xn, 1st row being indices and 2nd being values
  i <- which.min(wmins[2,])
  j <- wmins[1,i]
  return(c(d[i,j], i,j))
}

imin <- function(x) {
  lx <- length(x)
  i <- x[lx]  # original row number
  j <- which.min(x[(i+1):(lx-1)])
  k <- i+j
  return(c(k, x[k]))
}

