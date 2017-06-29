library(data.table)
library(microbenchmark)

#function to generate your data
getData <- function(){
  data.frame(x=rnorm(10000),y=rnorm(10000),z=rnorm(10000))
}

#using data table's rbindlist each iteration
fDT1 <- function(n){
  dat <- getData()
  for(i in 1:n){
    dat <- rbindlist(list(dat,getData()))
  }
  return(data.frame(dat))
}

#using data table's rbindlist all at once
fDT2 <- function(n){
  return(data.frame(rbindlist(lapply(1:n,function(x) getData()))))
}

#pre-allocating a data frame
fPre <- function(n){
  dat <- data.frame(x=rep(0,n*10000),y=rep(0,n*10000),z=rep(0,n*10000))
  j <- 1
  for(i in 1:n){
    dat[j:(j+10000-1),] <- getData()
    j <- j + 10000
  }
  return(dat)
}

#standard do.call rbind
f2 <- function(n){
  return(do.call(rbind,lapply(1:n,function(x) getData())))
}

#current approach
f <- function(n){
  dat <- getData()
  for(i in 1:n){
    dat <- rbind(dat,getData())
  }
  return(dat)
}
microbenchmark(fDT2(5),fDT1(5),fPre(5),f2(5),f(5),
               fDT2(25),fDT1(25),fPre(25),f2(25),f(25),
               times=10)