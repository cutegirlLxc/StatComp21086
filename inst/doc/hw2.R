## ----eval=TRUE----------------------------------------------------------------

library(mvtnorm)

set.seed(333)
cov_matrix<-diag(c(1,1)) 
# generate bivariate normal sample (x,y), x ,y are independent.


z<-rmvnorm(100,rep(0,2),cov_matrix)
x<-z[,1]
y<-z[,2]
# Null hypothesis is that x is independent of y.


spear_cor<-cor.test(x,y,method="spearman")
rho_0<-spear_cor$estimate
# correlation coefficient use the method ,spearman.
R<-999 
# the number of replicate.

data<-c(x,y)

rho<-numeric(R)
for (r in 1:R) {
  index<-sample(1:200,size=100,replace = FALSE)
  x_1<-data[index]
  y_1<-data[-index]
  rho[r]<-cor(x_1,y_1,method="spearman")
}

# rhohat is correlation coefficient computed by permutation method. 
hist(rho,breaks = 50,col="pink")
points(rho_0,0,cex=1,pch=16)
p.value<-mean(c(rho_0,rho)>=rho_0)



## ----eval=TRUE----------------------------------------------------------------
set.seed(108)

b<-100
x<-rnorm(b,0,1.2)
y<-rnorm(b,0,1.5)
# x,y are independent.

z0<-data.frame(c(x,y))


library(RANN)
library(boot)
N<-c(b,b)
# sizes of x and y.

Tn<-function(data,index,sizes,k){
  n1<-sizes[1]
  n2<-sizes[2]
  n<-n1+n2
  z<-data[index,]
  NN <- nn2(data=z, k=k+1)
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 <= n1)
  i2 <- sum(block2 > n1)
  (i1 + i2) / (k * n)
}


boot.obj <- boot(data = z0, statistic = Tn, R = 9999,
sim = "permutation", sizes = N, k=4)
ts <- c(boot.obj$t0,boot.obj$t)
power.NN <-1- mean(ts>=ts[1])
cat("the power of NN is ",power.NN)

## ----eval=TRUE----------------------------------------------------------------
library(energy)
p_value.energy <- eqdist.etest(z0, sizes=N, R=9999)$p.value
power.energy<-1-p_value.energy
cat("the power of energy is ",power.energy)

## ----eval=TRUE----------------------------------------------------------------
library(Ball)
p_value.ball = bd.test(x = x, y = y, num.permutations=9999)$p.value
power.ball<-1-p_value.ball
cat("the power of ball is ",power.ball)

## ----eval=TRUE----------------------------------------------------------------
# set.seed(108)

#b<-100
#x<-rnorm(b,2,1)
#y<-rnorm(b,2.4,1.1)
#z1<-data.frame(c(x,y))
#N<-c(b,b)
#boot.obj <- boot(data = z1, statistic = Tn, R = 9999,
#sim = "permutation", sizes = N, k=4)
#ts <- c(boot.obj$t0,boot.obj$t)
#p_value.NN <- mean(ts>=ts[1])
#power.NN<-1-p_value.NN
#cat("the p-value of NN is ",power.NN)

## ----eval=TRUE----------------------------------------------------------------
#set.seed(255)
#library(energy)
#p_value.energy <- eqdist.etest(z1, sizes=N, R=9999)$p.value
#power.energy<-1-p_value.energy
#cat("the power of energy is ",power.energy)

## ----eval=TRUE----------------------------------------------------------------
#set.seed(255)
#library(Ball)
#p_value.ball = bd.test(x = x, y = y, num.permutations=9999)$p.value
#power.ball<-1-p_value.ball
#cat("the power of energy is ",power.ball)

## ----eval=TRUE----------------------------------------------------------------
#set.seed(48)
#b<-80
#N<-c(b,b)
#x<-rt(b,df=1)
#y<-numeric(b)
#for (i in 1:b) {
#  mu<-sample(c(1,2),size=1,prob = c(.6,.4))
#  y[i]<-rnorm(1,0,mu)
#}
# the distribution of y is a bimodel distribution.
#z2<-data.frame(c(x,y))

## ----eval=TRUE----------------------------------------------------------------
#boot.obj <- boot(data = z2, statistic = Tn, R = 9999,
#sim = "permutation", sizes = N, k=4)
#ts <- c(boot.obj$t0,boot.obj$t)
#p_value.NN <- mean(ts>=ts[1])
#power.NN<-1-p_value.NN
#cat("the power of NN is ",power.NN)

## ----eval=TRUE----------------------------------------------------------------
#library(energy)
#p_value.energy <- eqdist.etest(z2, sizes=N, R=9999)$p.value
#power.energy<-1-p_value.energy
#cat("the power of energy is ",power.energy)

## ----eval=TRUE----------------------------------------------------------------
#library(Ball)
#p_value.ball = bd.test(x = x, y = y, num.permutations=9999)$p.value
#power.ball<-1-p_value.ball
#cat("the power of energy is ",power.ball)

## ----eval=TRUE----------------------------------------------------------------
#set.seed(225)
#b1<-50
#b2<-500
#x<-rnorm(b1,0,1)
#y<-rnorm(b2,.5,1)

# the sizes of x and y are very different.
#z3<-data.frame(c(x,y))
#N<-c(b1,b2)

## ----eval=TRUE----------------------------------------------------------------
#set.seed(225)
#boot.obj <- boot(data = z3, statistic = Tn, R = 9999,
#sim = "permutation", sizes = N, k=4)
#ts <- c(boot.obj$t0,boot.obj$t)
#p_value.NN <- mean(ts>=ts[1])
#power.NN<-1-p_value.NN
#cat("the power of NN is ",power.NN)

## ----eval=TRUE----------------------------------------------------------------
#set.seed(225)
#library(energy)
#p_value.energy <- eqdist.etest(z3, sizes=N, R=9999)$p.value
#power.energy<-1-p_value.energy
#cat("the power of energy is ",power.energy)

## ----eval=TRUE----------------------------------------------------------------
#set.seed(225)
#library(Ball)
#p_value.ball = bd.test(x = x, y = y, num.permutations=9999)$p.value
#power.ball<-1-p_value.ball
#cat("the power of energy is ",power.ball)

## ----eval=TRUE----------------------------------------------------------------
set.seed(547)
# Choose normal distribution as a proposal distribution.
n<-1e4
x<-numeric(n)
x[1]<-rnorm(1,0,1) # sigma=1
k<-0
u<-runif(n)
for (l in 2:n) {
  xt<-x[l-1]
  y<-rnorm(1,xt)
  num<-dt(y,df=1)*dnorm(xt,mean=y)
  den<-dt(xt,df=1)*dnorm(y,mean=xt)
  
  alpha<-num/den
  if(u[l]<alpha)
    {x[l]<-y}
  else{
    x[l]<-xt
    k<-k+1
  }
  
}

index<-1001:n
x_dis<-x[index] 
# discarding the first 1000 of the chain.
plot(index,x_dis,type="l",col="red")

## ----eval=TRUE----------------------------------------------------------------
quan<-seq(0,1,.1)
decile<-qt(quan,df=1)
decile_simulation<-quantile(x_dis,quan)
cbind(decile,decile_simulation)

## ----eval=TRUE----------------------------------------------------------------
set.seed(254)
n<-1e4
chain_generating<-function(n,x1,sigma){
  x<-numeric(n)
  u<-runif(n)
  x[1]<-x1
  for (l in 2:n) {
  xt<-x[l-1]
  y<-rnorm(1,xt,sigma)
  num<-dt(y,df=1)*dnorm(xt,mean=y,sigma)
  den<-dt(xt,df=1)*dnorm(y,mean=xt,sigma)
  
  alpha<-num/den
  if(u[l]<alpha)
    {x[l]<-y}
  else{
    x[l]<-xt
  }
  
  }
  x # return the chain
}
# chain-generating is a function to generate a chain by m-h sampler.

n.chain<-4
# number of chains.
x0<-c(-4,-1,1,4)
n<-1e4
data<-matrix(0,nrow=n.chain,ncol = n)
data[1,]<-chain_generating(n,x0[1],1)
data[2,]<-chain_generating(n,x0[2],1)
data[3,]<-chain_generating(n,x0[3],1)
data[4,]<-chain_generating(n,x0[4],1)

## ----eval=TRUE----------------------------------------------------------------
psi<-t(apply(data, 1, cumsum))
for (i in 1:4) {
  psi[i,]<-psi[i,]/1:n
}



GR<-function(psi,n){
  psi<-as.matrix(psi)
  psirow.means<-rowMeans(psi)
  W<-mean(apply(psi, 1, "var"))
  v.hat<-(n-1)*W/n+var(psirow.means)
  R.hat<-v.hat/W
  R.hat
}
n<-1e4
rhat <- rep(0, n)
b<-1e3
#for (j in (b+1):n)
#        rhat[j] <- GR(psi[,1:j],n)
#    plot(x=(b+1):n,y=rhat[(b+1):n], type="l", xlab="index", ylab="R.hat",col="blue")
# we discard the first 1000 and plot since index=1001
#    abline(h=1.2, lty=2)

## ----eval=TRUE----------------------------------------------------------------
# we need more than 3000 times until rhat<1.2.
# rhat_dis<-rhat[b+1:n]
# min(which(rhat_dis<1.2))+b

## ----eval=TRUE----------------------------------------------------------------
#par(mfrow=c(2,2))
#for (i in 1:4) {
#  xi<-data[i,]
#  plot(xi,type = "l",col="blue",ylim = c(-15,15))
#}

## ----eval=TRUE----------------------------------------------------------------
set.seed(123)
n<-20
a<-1
b<-2
N<-1e4
data<-matrix(0,N,2)
data[1,]<-c(5,.5)
for (j in 2:N) {
  y<-data[j-1,2]
  data[j,1]<-rbinom(1,n,y)
  x<-data[j,1]
  data[j,2]<-rbeta(1,x+a,n-x+b)
}

b<-1e3+1 # discard the first 1000 of the chain.
chain<-data[b:N,]
plot(chain,xlab = "X",ylab = "Y")

## ----eval=TRUE----------------------------------------------------------------
chain_generating<-function(x0,y0,N,n,a,b){
  data<-matrix(0,N,2)
  data[1,]<-c(x0,y0)
  for (j in 2:N) {
  y<-data[j-1,2]
  data[j,1]<-rbinom(1,n,y)
  x<-data[j,1]
  data[j,2]<-rbeta(1,x+a,n-x+b)
  }
  data

}
x0<-c(0,20,50,100)
y0<-c(.98,.6,.1,.01)

## ----eval=TRUE----------------------------------------------------------------
library(stableGR)
set.seed(5432)
N<-1020
a<-20
b<-30
n<-100
rhat<-numeric(N)
burn<-20
#for (j in burn+1:N) {
#  chain1<-chain_generating(x0[1],y0[1],j,n,a,b)
#  chain2<-chain_generating(x0[2],y0[2],j,n,a,b)
#  chain3<-chain_generating(x0[3],y0[3],j,n,a,b)
#  chain4<-chain_generating(x0[4],y0[4],j,n,a,b)
#  data<-list(chain1,chain2,chain3,chain4)
#  rhat[j]<-stable.GR(data,multivariate = TRUE,mapping = "maxeigen")$mpsrf
  
#}

#x<-100:N
#y<-rhat[100:N]
# it can be seen that even when N is not large enough,rhat is still very small.
#plot(100:N,rhat[100:N])

## ----eval=TRUE----------------------------------------------------------------
# lambda computed by MLE.
library(stats4)
obs <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
mle.lam <- function(lambda=.3) {
  -(7*log(lambda) - lambda * sum(obs))
}
mle(mle.lam)@coef


## ----eval=TRUE----------------------------------------------------------------
iter<-1e2
tol <- .Machine$double.eps^0.5 # 

lam <- 0.2 # the original value of lambda
lam.op <- lam + 1

for (i in 1:iter){
  lam <- 10/( 3/lam + sum(obs) )
  if (abs(lam - lam.op)/lam.op < tol){break}
  else{lam.op <- lam}
  
}

lam
# the conclusion is the same as MLE

## ----eval=TRUE----------------------------------------------------------------
kth_term<-function(a,k,d){
  norm.a<-norm(as.vector(a),type = "2") 
  # norm of a vector
  e<-exp(lgamma((d+1)/2)+lgamma(k+(3/2))-lgamma(1+k+(d/2))-log(factorial(k))-k*log(2))
  coef<-(-1)^k
  (coef*e*norm.a^(2*k+2))/(2*k+1)*(2*k+2)
}

# for example, if we set a=(2,3),k=100,d=2.
kth_term(c(2,3),100,2)

## ----eval=TRUE----------------------------------------------------------------
k.sum<-function(a,K,d){
  a<-a
  k<-1:K
  d<-d
  sum(kth_term(a,k,d))
}


## ----eval=TRUE----------------------------------------------------------------
a<-c(1,2)

d<-2
convergence<-numeric(5)
for (j in 20:25) {
  convergence[j-20]<-k.sum(a,j,d)
}
convergence

## ----eval=TRUE----------------------------------------------------------------
# Firstly ,compute constants both sides, that is:
C1<-function(K){
  2*exp(lgamma(K/2)-lgamma((K-1)/2))/sqrt(pi*(K-1))
}
C2<-function(K){
  2*exp(lgamma((K+1)/2)-lgamma((K)/2))/sqrt(pi*K)
}
f.integ<-function(x,k){
  (1+x^2/k)^(-(k+1)/2)
}
# f.integ is the function which is integrated.

solve.a<-function(a,K){
  c.km1<-sqrt(a^2*(K-1)/(K-a^2))
  c.k<-sqrt(a^2*K/(K+1-a^2))
  inte.left<-integrate(f.integ,lower = 0,upper = c.km1,k<-K-1)$value
  inte.right<-integrate(f.integ,lower = 0,upper = c.k,k<-K)$value
  C1(K)*inte.left-C2(K)*inte.right
}
A.k<-numeric(25)
k<-c(4:25,100,500,1000)
for (j in 1:25) {
  if(k[j]<=7){
    up=sqrt(k[j]-.001)
  }else{
    up=2
  }
  A.k[j] <-unlist( uniroot(solve.a,lower=.0001,upper=up,K=k[j]))[1]}
# A.k is the root computed by uniroot.
# 11.4
k = c(4:25,100,500,1000)
S = function(a,k){
 ck = sqrt(a^2*k/(k+1-a^2))
 pt(ck,df=k,lower.tail=FALSE)
}

f = function(a,k){S(a,k)-S(a,k-1)}
#curve(f(x),xlim = c(0,sqrt(k)))
a <- seq(0, 4, by=0.01)
plot(a, f(a, k[23]), lty=1, col=1, type="l", xlim=c(0, 4), xlab="a", ylab="f(a|k)", main="f(a) with different k")
lines(a, f(a, k[24]), xlim = c(0, 4), lty=2, col=2)
lines(a, f(a, k[25]), xlim = c(0, 4), lty=3, col=3)
legend("topright", legend=c("k=100", "k=500", "k=1000"), col=1:3,lty=1:3)

## ----eval=TRUE----------------------------------------------------------------
solve = function(k){
  output = uniroot(function(a){S(a,k)-S(a,k-1)},lower=1,upper=2)
  output$root
}

root = matrix(0,2,sum(lengths(k)))

for (i in 1:sum(lengths(k))){
  root[2,i]=round(solve(k[i]),4)
}

root[1,] = k
rownames(root) = c('k','A(k)')

# compare with 11.4
Ak<-root[2,]
index<-c(4:25,100,500,1000)
df<-data.frame(index,Ak,A.k)
# Ak is computed in 11.4, df is the dataframe to compare Ak and A.k

df


## ----eval=TRUE----------------------------------------------------------------
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

reg.la <- lapply(formulas, function(x) {lm(formula = x, mtcars)})



reg.lp <- vector("list", 4)
for (i in seq_along(formulas)){
  reg.lp[[i]] <- lm(formulas[[i]], data = mtcars)
}


## ----eval=TRUE----------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})


regla.bs <- lapply(bootstraps, lm, formula = mpg ~ disp)

reglp.bs <- vector("list", 10)
for (i in seq_along(bootstraps)){
  reglp.bs[[i]] <- lm(mpg ~ disp,  bootstraps[[i]])
}




## ----eval=TRUE----------------------------------------------------------------
# exercise3：
rsq <- function(mod) summary(mod)$r.squared
sapply(reg.la, rsq)

sapply(reg.lp, rsq)

## ----eval=TRUE----------------------------------------------------------------
# exercise4
sapply(regla.bs, rsq)
sapply(reglp.bs,rsq)


## ----eval=TRUE----------------------------------------------------------------
set.seed(225)
x<-rnorm(100,0,1)
y<-rpois(100,10)
data<-data.frame(x,y) 

vapply(data, sd,numeric(1))



## ----eval=TRUE----------------------------------------------------------------
z<-"a"
data1<-data.frame(x,y,z)
vapply(data1[vapply(data1, is.numeric, logical(1))],
       sd, 
       numeric(1))
# The internal vapply finds the columns in data1 that are numeric, and the external vapply finds the sd for the numeric columns.


## ----eval=TRUE----------------------------------------------------------------
library(Rcpp)
#  original value of x is specified
cppFunction('NumericMatrix RcppGibbs(int N, int n,int a, int b ) {

    int i;
    double yc;
    NumericMatrix mat(N, 2);
    mat(0,0)=5;
    mat(0,1)=.5;

    double x, y;

    for (i=1; i<N; i++) {
        
        yc=mat(i-1,1) ; 
    x=R::rbinom(n,yc);
    y=R::rbeta(x+a,n-x+b);
        mat(i,0) = x;
        mat(i,1) = y;
        
    }

    return mat;            
}')

## ----eval=TRUE----------------------------------------------------------------
# sample using gibbs
Rgibbs<-function(N,n,a,b){
  data<-matrix(0,N,2)
  data[1,]<-c(5,.5)
  for (j in 2:N) {
  y<-data[j-1,2]
  data[j,1]<-rbinom(1,n,y)
  x<-data[j,1]
  data[j,2]<-rbeta(1,x+a,n-x+b)
  }
  data

}

## ----eval=TRUE----------------------------------------------------------------
set.seed(1578)
N<-2000
n<-10
a<-4
b<-6
Rmat<-Rgibbs(N,n,a,b)  # The random number matrix by r function.
Cmat<-RcppGibbs(N,n,a,b)# by Cpp function
par(mfrow=c(1,2))
plot(Cmat)
plot(Rmat) 

## ----eval=TRUE----------------------------------------------------------------
library(ggplot2)
gg_qq_empirical <- function(a, b, quantiles = seq(0, 1, 0.01))
{
  a_lab <- deparse(substitute(a))
  if(missing(b)) {
    b <- rnorm(length(a), mean(a), sd(a))
    b_lab <- "normal distribution"
  }
  else b_lab <- deparse(substitute(b))
  
  ggplot(mapping = aes(x = quantile(a, quantiles), 
                       y = quantile(b, quantiles))) + 
    geom_point() +
    geom_abline(aes(slope = 1, intercept = 0), linetype = 2) +
    labs(x = paste(deparse(substitute(a)), "quantiles"), 
         y = paste(deparse(substitute(b)), "quantiles"),
         title = paste("Empirical qq plot of", a_lab, "against", b_lab))
}
Rgib<-Rmat[,2]
Cgib<-Cmat[,2]

qq <- gg_qq_empirical(Rgib, Cgib)
qq + theme_light() + coord_equal()


## ----eval=TRUE----------------------------------------------------------------
Rgib1<-Rmat[,1]
Cgib1<-Cmat[,1]
qq1 <- gg_qq_empirical(Rgib1, Cgib1)
qq1 + theme_light() + coord_equal()

## ----eval=TRUE----------------------------------------------------------------
# library(microbenchmark)
#ts <- microbenchmark(Rgib=Rgibbs(N,n,a,b),Cppgib=RcppGibbs(N,n,a,b))

#summary(ts)[,c(1,3,5,6)]
# rcpp's performace is better

## ----eval=TRUE----------------------------------------------------------------
# Increase N Again compare the computational speed.
#N<-1e4
#ts <- microbenchmark(Rgib=Rgibbs(N,n,a,b),Cppgib=RcppGibbs(N,n,a,b))

#summary(ts)[,c(1,3,5,6)]

