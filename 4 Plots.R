crimedata_avg <- merge(agg.crime.month.avg, ACS, by = 'NBHD_NAME')
crimedata_avg <- crimedata_avg[,1:4]
crimedata_avg$crime.rate <- crimedata_avg$crime.per.month/crimedata_avg$TTL_POPULATION_ALL

#plots
library(sf)
denver_neighborhoods<-st_read("G:/My Drive/Bayes/Final Project/Data/american_community_survey_nbrhd_2013_2017/american_community_survey_nbrhd_2013_2017.shp")
denver_neighborhoodstest<-st_read("G:/My Drive/Bayes/Final Project/Data/statistical_neighborhoods/statistical_neighborhoods.shp")
plot(denver_neighborhoods$geometry)  
denver_neighborhoods_new<-denver_neighborhoods[order(denver_neighborhoods$NBHD_NAME),]
denver_neighborhoods_new$NBHD_NAME2<-denver_neighborhoods$NBHD_NAME

crimeplot <- merge(crimedata_avg, denver_neighborhoods_new, by = 'NBHD_NAME2')
Burglary_summer<-data.frame(Summer_burglary_counts,denver_neighborhoods_new)
#making an sf file type
crimeplot_sf<-st_sf(crimeplot)
#plotting

pdf(paste0("neghibourhoodscrime.pdf"),width=8, height=4)
plot(crimeplot_sf['crime.per.month'],main = 'Average number of crimes in Denver neighbourhoods')
dev.off()
