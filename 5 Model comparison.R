
nov.crime <- read.csv("G:/My Drive/Bayes/Final Project/Data/November crimes.csv", stringsAsFactors = TRUE)
nov.crime$FIRST_OCCURRENCE_DATE<-as.POSIXct(nov.crime$FIRST_OCCURRENCE_DATE,format="%m/%d/%Y %I:%M:%S %p")
nov.crime$FIRST_OCCURRENCE_DATE<-as.POSIXct(nov.crime$FIRST_OCCURRENCE_DATE,format="%m/%d/%Y %I:%M:%S %p")
nov.crime$month <- months.POSIXt(nov.crime$FIRST_OCCURRENCE_DATE)
nov.crime$year <- format(nov.crime$FIRST_OCCURRENCE_DATE, format = "%Y")
crime.type <- c("arson", "auto-theft", "burglary", 
                "larceny","public-disorder","robbery",
                "theft-from-motor-vehicle")
nov.crime <- subset(nov.crime, subset = OFFENSE_CATEGORY_ID %in% crime.type )

nov.agg.crime.month <- aggregate(nov.crime$INCIDENT_ID, by = list(nov.crime$NEIGHBORHOOD_ID), FUN = length)
colnames(nov.agg.crime.month) <- c("NBHD_NAME", 'crime')

nov.crimedata <- merge(nov.agg.crime.month, ACS, by = 'NBHD_NAME')
nov.crimedata$crime.rate <- nov.crimedata$crime/nov.crimedata$TTL_POPULATION_ALL

nov.crimedata$crime.cat <- 0
nov.crimedata[which(nov.crimedata$crime.rate>= 0.0064475),"crime.cat"] <- 1
summary(as.factor(nov.crimedata$crime.cat))


library(stringr)
nov.crimedata$timenew <- 72
nov.crimedata$constant <- 1
nov.crimedata$NBHD_NO <- as.numeric(nov.crimedata$NBHD_NAME)

nov.crimedat <- nov.crimedata[, c("constant","NBHD_NO", "AVG_HH_INCOME","LESS_THAN_HS_DIPLOMA_EDU","PCT_HISPANIC","PCT_WHITE", "PCT_BLACK","PCT_POVERTY","timenew")]
X.pred <- unname(data.matrix(nov.crimedat))
X.old <- unname(data.matrix(crimedat))
x.stat <- array(NA, dim = c(ncol(X.old),2))
for (i in 1:ncol(X.old)) {
  x.stat[i,1] <- mean(X.old[,i])
  x.stat[i,2] <- sd(X.old[,i])
}
X.pred <- scale(X.pred, center = x.stat[,1], scale = x.stat[,2] )
X.pred[,1] <- 1
y.pred <- nov.crimedata[,"crime.cat"]


p.pred <- array(NA, dim = c(length(y.pred),6))
for (i in 1:length(y.pred)) {
  p.pred[i,1] <- pnorm(t(X.pred[i,]) %*% beta_P[,1])
}

for (i in 1:length(y.pred)) {
  p.pred[i,2] <- pnorm(lambda_post_2[i] %*% t(X.pred[i,]) %*% beta_2[,1])
  p.pred[i,3] <- pnorm(lambda_post_4[i] %*% t(X.pred[i,]) %*% beta_4[,1])
  p.pred[i,4] <- pnorm(lambda_post_8[i] %*% t(X.pred[i,]) %*% beta_8[,1])
  p.pred[i,5] <- pnorm(lambda_post_16[i] %*% t(X.pred[i,]) %*% beta_16[,1])
  p.pred[i,6] <- pnorm(lambda_post_32[i] %*% t(X.pred[i,]) %*% beta_32[,1])
}

cat.pred <- array(1, dim = c(length(y.pred),6))
for (k in 1:6) {
  cat.pred[which(p.pred[,k]<0.5),k] <- 0
}

class.error <- array(NA, dim = c(1,6))
for (k in 1:6) {
  class.error[1,k] <- length(which(cat.pred[,k]!=y.pred))/length(y.pred)
}

### classification errors for glm
pred.insampl <- predict(myprobit,test.dat[-1] , type = "response")
sample.pred <- array(1,dim = length(y))
sample.pred[which(pred.insampl<0.5)] <- 0
class.error.glm.insample <- length(which(sample.pred!=y))/length(y)

colnames(X.pred) <- c("constant","NBHD_NO", "AVG_HH_INCOME","LESS_THAN_HS_DIPLOMA_EDU","PCT_HISPANIC","PCT_WHITE", "PCT_BLACK","PCT_POVERTY","timenew")
test.dat.pred <- data.frame(X.pred)
pred.out <- predict(myprobit,test.dat.pred[,-1] , type = "response")
sample.pred <- array(1,dim = length(y.pred))
sample.pred[which(pred.out<0.5)] <- 0
class.error.glm.outsample <- length(which(sample.pred!=y.pred))/length(y.pred)


### comparison plots
pdf(paste0("probability comparison.pdf"),width=8, height=8)
par(mfrow = c(3,2),mar=c(4,5,2,2))
plot(p_beta_P[,1],pred.insampl, ylab = "probabilties from GLM", xlab = "probabilities from Probit")
plot(p_beta_2[,1],pred.insampl, ylab = "probabilties from GLM", xlab = "probabilities from t-link df = 2")
plot(p_beta_4[,1],pred.insampl, ylab = "probabilties from GLM", xlab = "probabilities from t-link df = 4")
plot(p_beta_8[,1],pred.insampl, ylab = "probabilties from GLM", xlab = "probabilities from t-link df = 8")
plot(p_beta_16[,1],pred.insampl,ylab = "probabilties from GLM", xlab = "probabilities from t-link df = 16")
plot(p_beta_32[,1],pred.insampl,ylab = "probabilties from GLM", xlab = "probabilities from t-link df = 32")
dev.off()

pdf(paste0("probability density comparison.pdf"),width=8, height=8)
par(mfrow = c(3,2),mar=c(4,5,2,2))
diff.prob <- p_beta_P[,1] - pred.insampl
plot(density(diff.prob), xlab = "Diff of Probit vs GLM",main = "")
diff.prob <- p_beta_2[,1] - pred.insampl
plot(density(diff.prob),xlab = "Diff of tLink df = 2 vs GLM",main = "")
diff.prob <- p_beta_4[,1] - pred.insampl
plot(density(diff.prob),xlab = "Diff of tLink df = 4 vs GLM",main = "")
diff.prob <- p_beta_8[,1] - pred.insampl
plot(density(diff.prob),xlab = "Diff of tLink df = 8 vs GLM",main = "")
diff.prob <- p_beta_16[,1] - pred.insampl
plot(density(diff.prob),xlab = "Diff of tLink df = 16 vs GLM",main = "")
diff.prob <- p_beta_32[,1] - pred.insampl
plot(density(diff.prob),xlab = "Diff of tLink df = 32 vs GLM",main = "")
dev.off()



diff.prob <- p_beta_P[,1]-pred.insampl
plot(density(diff.prob))
