#method 1
library(truncnorm)
library(MASS)
crimedat <- crimedata[, c("constant","NBHD_NO", "AVG_HH_INCOME","LESS_THAN_HS_DIPLOMA_EDU","PCT_HISPANIC","PCT_WHITE", "PCT_BLACK","PCT_POVERTY","timenew")]
crimedata[,155] <- as.numeric(crimedata[,155])
myprobit <- glm(crime.cat ~ NBHD_NO + AVG_HH_INCOME  + LESS_THAN_HS_DIPLOMA_EDU + PCT_HISPANIC + PCT_WHITE + PCT_BLACK + PCT_POVERTY + timenew, family = binomial(link = "probit"), 
                data = crimedata)
summary(myprobit)

colnames(X) <- c("constant","NBHD_NO", "AVG_HH_INCOME","LESS_THAN_HS_DIPLOMA_EDU","PCT_HISPANIC","PCT_WHITE", "PCT_BLACK","PCT_POVERTY","timenew")
test.dat <- data.frame(X)
myprobit <- glm(crimedata[,"crime.cat"] ~ NBHD_NO + AVG_HH_INCOME  + LESS_THAN_HS_DIPLOMA_EDU + PCT_HISPANIC + PCT_WHITE + PCT_BLACK + PCT_POVERTY + timenew, family = binomial(link = "probit"), 
                data = test.dat)
summary(myprobit)


X <- unname(data.matrix(crimedat))
X <- scale(X)
X[,1] <- 1
y <- crimedata[,"crime.cat"]

#Gibbs sampling
set.seed(1919)
n <- 10000 #number of iterations
N <- nrow(X) #5395
A <- solve(t(X) %*% X)
beta_mc_P <- matrix(NA,9,n)
Z_mc <- matrix(NA,N,n)
Z <- matrix(NA,N,1)

#initial values
beta <- myprobit$coefficients

#Gibbs sampling
for (i in 1:n) {
  for (j in 1:N) {
    m <- t(X[j,]) %*% beta
    if(y[j] == 1){
      Z_mc[j,i] <- Z[j] <- rtruncnorm(1,a = 0, mean = m, sd = 1)
    }else{
      Z_mc[j,i] <- Z[j] <- rtruncnorm(1,b = 0, mean = m, sd = 1)
    }
  }
  B <- t(X) %*% Z
  mvn_mean <- A %*% B
  beta_mc_P[,i] <- beta <- mvrnorm(1, mvn_mean, A)
}

rm(Z,Z_mc,A,B)

pdf(paste0("beta_mc_P.pdf"),width=8, height=8)
par(mfrow = c(3,3),mar=c(2,3,2,3))
for (i in 1:9) {
  plot(beta_mc_P[i,5000:10000], type = "l", ylab = "", main= paste0("Beta ",i-1))
}
dev.off()

pdf(paste0("beta_mc_P_density.pdf"), width=8, height=8)
par(mfrow = c(3,3),mar=c(2,3,2,3))
for (i in 1:9) {
  plot(density(beta_mc_P[i,5000:10000]), ylab = "", main= paste0("Beta ",i-1))
}
dev.off()

beta_P <- array(NA, dim = c(9,2))
for (i in 1:9) {
  beta_P[i,1] <- mean(beta_mc_P[i,5000:10000])
  beta_P[i,2] <- sd(beta_mc_P[i,5000:10000])
}

#probability of crime for each observation in sample
p_beta_P <- array(NA, dim = c(N,2))
for (i in 1:N) {
  p_beta_P[i,1] <- pnorm(t(X[i,]) %*% beta_P[,1])
}

summary(p_beta_P)

#classification based on predicted probability
p_beta_P[,2] <- 1
p_beta_P[which(p_beta_P[,1]<0.5),2] <- 0
class_errorbeta_P <- length(which(p_beta_P[,2]!=y))/length(y)


