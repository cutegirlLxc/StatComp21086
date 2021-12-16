## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  library(depth)
#  data_depth<-function(data,method){
#    if(method==1){
#      madist<-mahalanobis(data,colMeans(data),cov(data))
#      madist
#    }else{
#      N<-nrow(data)
#      nc<-ncol(data)
#      dep<-numeric(N)
#      meth=ifelse(nc<3,"Tukey","Oja")
#      dep_comp<-function(y){
#        depth::depth(y,data,method = meth)
#      }
#      apply(data, 1, dep_comp)
#    }
#  }
#  
#  

## ----eval=FALSE---------------------------------------------------------------
#  depth_rank<-function(data,method){
#    dep <- data_depth(data,method)
#    rank(dep,ties.method = "first")
#  }
#  

## ----eval=FALSE---------------------------------------------------------------
#  Z.t<-function(data,method,t){
#    N<-nrow(data)
#    Nt<-N*t
#    n<-floor(Nt)
#    mean<-(N+1)/2
#    sig<-sqrt((N^2-1)/12)
#    r<-depth_rank(data,method)
#    normalized_rank<-(r-mean)/sig
#    trank<-normalized_rank[1:n]
#    sum(trank)/sqrt(N)
#  }
#  

## ----eval=FALSE---------------------------------------------------------------
#  Z_stat<-function(data,method){
#    N<-nrow(data)
#    mean<-(N+1)/2
#    sig<-sqrt((N^2-1)/12)
#    r<-depth_rank(data,method)
#    normalized_rank<-(r-mean)/sig
#    cumsum(normalized_rank)/sqrt(N)
#  }
#  

## ----eval=FALSE---------------------------------------------------------------
#  chan_poi<-function(data,method){
#    Z<-Z_stat(data,method)
#    M<-max(abs(Z))
#    which(Z==M)
#  }
#  

## ----eval=FALSE---------------------------------------------------------------
#  set.seed(258)
#  library(StatComp21086)
#  library(mvtnorm)
#  mu1<-rep(1,20)
#  sig1<-diag(10,20)
#  sig2<-diag(c(9,8,7,rep(10,14),7,8,9))
#  mu2<-seq(from=0.55,to=1.5,by=.05)
#  x<-rmvnorm(100,mu1,sig2)
#  y<-rmvnorm(100,mu2,sig2)
#  data<-rbind(x,y)
#  
#  z<-Z_stat(data,1)
#  plot(z,type="l",col='blue')
#  chan_poi(data,1,2)

