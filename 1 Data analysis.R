crime<-read.csv("G:/My Drive/Bayes/Final Project/Data/crime.csv", stringsAsFactors = TRUE)
#ACS<-read.csv("G:/My Drive/Bayes/Final Project/Data/american_community_survey_nbrhd_2013_2017.csv", stringsAsFactors = TRUE)
ACS<-read.csv("G:/My Drive/Bayes/Final Project/Data/american_community_survey_nbrhd_2015_2019.csv", stringsAsFactors = TRUE)


crime$FIRST_OCCURRENCE_DATE<-as.POSIXct(crime$FIRST_OCCURRENCE_DATE,format="%m/%d/%Y %I:%M:%S %p")
crime$FIRST_OCCURRENCE_DATE<-as.POSIXct(crime$FIRST_OCCURRENCE_DATE,format="%m/%d/%Y %I:%M:%S %p")
crime$month <- months.POSIXt(crime$FIRST_OCCURRENCE_DATE)
crime$year <- format(crime$FIRST_OCCURRENCE_DATE, format = "%Y")
min(crime$FIRST_OCCURRENCE_DATE, na.rm = TRUE)
max(crime$FIRST_OCCURRENCE_DATE, na.rm = TRUE)
ACS$AVG_HH_INCOME <- as.numeric(ACS$AVG_HH_INCOME)

#summary of different crime categories in the dataset
pdf(paste0("crimecat.pdf"),width=8, height=8)
par(mfrow = c(1,1),mar=c(13,4,1.5,1.5))
plot(crime$OFFENSE_CATEGORY_ID,las=2 )
dev.off()

#extract the crime categories we want
crime.type <- c("arson", "auto-theft", "burglary", 
                "larceny","public-disorder","robbery",
                 "theft-from-motor-vehicle")
crime.new <- subset(crime, subset = OFFENSE_CATEGORY_ID %in% crime.type )
rm(crime.type, crime)

#average crimes per month in each neighbourhood
agg.crime.month <- aggregate(crime.new$INCIDENT_ID, by = list(crime.new$NEIGHBORHOOD_ID, crime.new$year, crime.new$month), FUN = length)
colnames(agg.crime.month) <- c("NBHD_NAME", "year", "month", 'crime')
agg.crime.month.avg <- aggregate(agg.crime.month$crime, by = list(agg.crime.month$NBHD_NAME), FUN = mean)
colnames(agg.crime.month.avg) <- c("NBHD_NAME", 'crime.per.month')
plot(density(agg.crime.month.avg$crime.per.month), main = "")

rm(crime.new)

crimedata <- merge(agg.crime.month, ACS, by = 'NBHD_NAME')
crimedata$crime.rate <- crimedata$crime/crimedata$TTL_POPULATION_ALL
#crimedata$crime.rate <- crimedata$crime/crimedata$TTL_HOUSEHOLDS
#summary(crimedata$crime.rate)

#creating crime category
crimedata$crime.cat <- 0
crimedata[which(crimedata$crime.rate>= mean(crimedata$crime.rate)),"crime.cat"] <- 1
summary(as.factor(crimedata$crime.cat))

pdf(paste0("crimemonthratedensity.pdf"),width=8, height=8)
plot(density(crimedata$crime.rate), main = "")
abline(v=0.006087045,col = "red")
dev.off()


pdf(paste0("crimemonthdensiyt2in1.pdf"),width=8, height=4)
par(mfrow = c(1,2))
plot(density(agg.crime.month$crime), main = "Number of Crimes")
plot(density(crimedata$crime.rate), main = "Crime rate")
dev.off()

library(stringr)
crimedata$time <- str_c(crimedata$year,"-",crimedata$month,"-01")
crimedata$time <- as.Date(crimedata$time, "%Y-%B-%d")
min(crimedata$time)
crimedata$timenew <- round(difftime(crimedata$time, "2015-12-01", units = "days")/30)
crimedata <- crimedata[order(crimedata$timenew),]
#lets remove the last month november 2021 because it does not have a full year of data
length(which(crimedata$timenew==72)) #75 data points 5532-75=5457
crimedata <- crimedata[-which(crimedata$timenew==72),]
crimedata$constant <- 1
crimedata$NBHD_NO <- as.numeric(crimedata$NBHD_NAME)





