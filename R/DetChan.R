#' @title A function computes the depth of each sample point 
#' @name data_depth
#' @description This function calls the depth function in the depth package.
#' @param data The sample. 
#' @param method Choose the depth function  
#' @return A vector with dimension equal to the sample size
#' @examples
#' \dontrun{
#' x<-matrix(rnorm(300),nc=3)
#' data_depth(x,1)
#'   }
#' @import depth
#' @importFrom depth depth
#' @importFrom stats cov mahalanobis
#' @export


data_depth<-function(data,method){
  if(method==1){
    madist<-mahalanobis(data,colMeans(data),cov(data))
    madist
  }else{
    N<-nrow(data)
    nc<-ncol(data)
    dep<-numeric(N)
    meth=ifelse(nc<3,"Tukey","Oja")
    dep_comp<-function(y){
      depth::depth(y,data,method = meth)
    }
    apply(data, 1, dep_comp)
  }
}

#' @title A function computes the depth of each sample point
#' @name depth_rank
#' @description The function computes the rank of each sample point by depth.
#' @param data the sample
#' @param method Choose the depth function
#' @return A vector with dimension equal to the sample size
#' @examples
#' \dontrun{
#' x<-matrix(rnorm(300),nc=3)
#' rank_vector<-depth_rank(x)
#'   }
#' @import depth
#' @importFrom depth depth
#' @importFrom stats cov mahalanobis
#' @export

depth_rank<-function(data,method){
  dep <- data_depth(data,method)
  rank(dep,ties.method = "first")
}

#' @title A function calculates the value of the statistic Z at t
#' @name Z.t
#' @description Z is the rank-CUSUM statistic.
#' @param data the sample.
#' @param t a constant which is between 0 and 1, denotes the location of the data,for example, size of a sample is 100,t=.2 ,that is the 20th point of the sample
#' @param method Choose the depth function
#' @return A scalar.
#' @examples
#' \dontrun{
#' x<-matrix(rnorm(300),nc=6)
#' y<-matrix(runif(300),nc=6)
#' data<-rbind(x,y)
#' t=.2
#' Z.t(data,1,t)
#'   }
#' @import depth
#' @importFrom depth depth
#' @importFrom stats cov mahalanobis
#' @export

Z.t<-function(data,method,t){
  N<-nrow(data)
  Nt<-N*t
  n<-floor(Nt)
  mean<-(N+1)/2
  sig<-sqrt((N^2-1)/12)
  r<-depth_rank(data,method)
  normalized_rank<-(r-mean)/sig
  trank<-normalized_rank[1:n]
  sum(trank)/sqrt(N)
}


#' @title A function computes the value of Z for all sample points
#' @name Z_stat
#' @description Z is the rank-CUSUM statistic.
#' @param data the sample
#' @param method Choose the depth function
#' @examples
#' \dontrun{
#' x<-matrix(rnorm(300,0,1),nc=6)
#' y<-matrix(runif(300,1,2),nc=6)
#' data<-rbind(x,y)
#' z<-Z_stat(data,1)
#' plot(z,type="l",col="blue")
#' 
#' data1<-matrix(rnorm(30000),nc=30)
#' z1<-Z_stat(data1,1)
#' plot(z1,type="l",col="blue")
#'   }
#' @import depth
#' @importFrom depth depth
#' @importFrom stats cov mahalanobis
#' @export

Z_stat<-function(data,method){
  N<-nrow(data)
  mean<-(N+1)/2
  sig<-sqrt((N^2-1)/12)
  r<-depth_rank(data,method)
  normalized_rank<-(r-mean)/sig
  cumsum(normalized_rank)/sqrt(N)
}

#' @title A function estimate the location of change point
#' @name chan_poi
#' @description The estimation of the location is the point that makes z take the maximum value.
#' @param data the sample.
#' @param method Choose the depth function
#' @param loc If loc=1,tell us the absolute location, otherwise return the the relative location of the change point.
#' @examples
#' \dontrun{
#' library(mvtnorm)
#' set.seed(518)
#' mu1<-rep(1,20)
#' sig1<-diag(10,20)
#' sig2<-diag(1:20)
#' mu2<-seq(from=0.55,to=1.5,by=.05)
#' x<-rmvnorm(200,mu1,sig2)
#' y<-rmvnorm(100,mu2,sig2)
#' data<-rbind(x,y)
#' chan_poi(data,1,2)
#'   }
#' @import depth
#' @importFrom depth depth
#' @importFrom stats cov mahalanobis
#' @export

chan_poi<-function(data,method,loc){
  Z<-Z_stat(data,method)
  Z_abso<-abs(Z)
  M<-max(Z_abso)
  location<-which(Z_abso==M)
  theta<-location/nrow(data)
  if(loc==1){
    return(location)
  }else{
    return(theta)
  }
}
