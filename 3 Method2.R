#t link method
library(MASS) #for mvn
library(truncnorm)
set.seed(1919)
n <- 5000 #number of iterations
N <- nrow(X) #5457
A <- solve(t(X) %*% X)
beta_mc_2 <- matrix(NA,9,n)
Z_mc <- matrix(NA,N,n)
lambda_mc <- matrix(NA,N,n)
Z <- matrix(NA,N,1)

#df_prob <- matrix(NA,n,2)
#df_prob_sum <- 0
df <- 2
c <- (gamma(df/2)*(df/2)^(df/2))^(-1)
#initial values

beta <- myprobit$coefficients
lambda <- rgamma(N,df/2,2/df)
W <- matrix(0,N,N)

for (i in 1:n) {
  for (j in 1:N) {
    m <- t(X[j,]) %*% beta
    if(y[j] == 1){
      Z_mc[j,i] <- Z[j] <- rtruncnorm(1,a = 0, mean = m, sd = sqrt(1/lambda[j]))
    }else{
      Z_mc[j,i] <- Z[j] <- rtruncnorm(1,b = 0, mean = m, sd = sqrt(1/lambda[j]))
    }
  }
  diag(W) <- lambda
  K <- solve(t(X) %*% W %*% X)
  beta_hat <- K %*% t(X) %*% W %*% Z
  beta_mc_2[,i] <- beta <- mvrnorm(1,mu = beta_hat, Sigma = K)
  for (k in 1:N) {
    sig <- df + (Z[k]-t(X[k,]) %*% beta)^2
    lambda_mc[k,i] <- lambda[k] <- rgamma(1,(df+1)/2,2/sig)
  }
if(i%%100==0){
  print(i)
}
}

rm(Z_mc,Z,W,K)


pdf(paste0("beta_mc_2.pdf"),width=8, height=8)
par(mfrow = c(3,3),mar=c(2,3,2,3))
for (i in 1:9) {
  plot(beta_mc_2[i,1000:5000], type = "l", ylab = "", main= paste0("Beta ",i-1))
}
dev.off()

pdf(paste0("beta_mc_2_density.pdf"),width=8, height=8)
par(mfrow = c(3,3),mar=c(2,3,2,3))
for (i in 1:9) {
  plot(density(beta_mc_2[i,1000:5000]), ylab = "", main= paste0("Beta ",i-1))
}
dev.off()

beta_2 <- array(NA, dim = c(9,2))
for (i in 1:9) {
  beta_2[i,1] <- mean(beta_mc_2[i,1000:5000])
  beta_2[i,2] <- sd(beta_mc_2[i,1000:5000])
}

lambda_post_2 <- array(NA,dim = c(N,1))
for (i in 1:N) {
  lambda_post_2[i] <- mean(lambda_mc[i,1000:5000])
}

lambda_post_2 <- sqrt(lambda_post_2)

p_beta_2 <- array(NA, dim = c(N,2))
for (i in 1:N) {
  p_beta_2[i,1] <- pnorm(lambda_post_2[i] %*% t(X[i,]) %*% beta_2[,1])
}

summary(p_beta_2)

#classification based on predicted probability
p_beta_2[,2] <- 1
p_beta_2[which(p_beta_2[,1]<0.5),2] <- 0
class_errorbeta_2 <- length(which(p_beta_2[,2]!=y))/length(y)

summary(p_beta_2)
summary(p_beta_P)
plot(p_beta_P[,1],p_beta_2[,1], xlab = "p from Probit", ylab = "p from tLink")


