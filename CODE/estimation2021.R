#####2021#####
Red_21 <- Redditi_IRPEF_2021[,c(1:8,11:16,31:50)]
#remove one faulty col
Red_21 <- Red_21[-7905,]
Red_21[9:34][is.na(Red_21[9:34])] <- 0
###AVGs###
empty_cols <- c('avglavdip','avgret','avgpiva','avgadreg','avgadcom', 'avg0less', 
                'avg10k','avg15k','avg26k','avg55k','avg75k','avg120k','avg120+k')
Red_21[,empty_cols]<-NA
a <-35
i <-10
while(i<36){
  Red_21[,a] <- Red_21[,i]/Red_21[,i-1]
  a<-a+1
  i<-i+2
}
###%###computing the incidence of the different categories over the total number of taxpayers###
Red_21[9:47][is.na(Red_21[9:47])] <- 0
empty_cols <- c('%lavdip','%ret','%piva','adreg-tbr','adcom-tbr', 
                '%0less', '%10k','%15k','%26k','%55k','%75k','%120k','%120+k')
Red_21[,empty_cols]<-NA
a <-48
i <-9
while(i<34){
  Red_21[,a] <- round(Red_21[,i]/Red_21[,8],3) #count of each category/contribuenti
  a<-a+1
  i<-i+2
}

Red_21 <- Red_21[,-c(51,52)] #remove incidence of regional and municipal tax

colnames(Red_21)[8:34]<-c("taxpayers","count_lavdip","lavdip","count_ret","ret","count_piva","piva","count_regio","regio","count_comu","comu","count_0less","0less","count_10k","10k","count_15k","15k",
                          "count_26k","26k","count_55k","55k","count_75k","75k",
                          "count_120k","120k","count_120+k","120+k")
#join PAs Bolzano & Trento
Red_21<- Red_21%>%
  mutate(Regione = ifelse(`Codice Istat Regione`== "04", "Trentino Alto Adige",Regione))

###Group by region and group by regXdegurba###
#####regionXdegurba#####
units_rd_21<- classified_units%>%
  select(c(LAU_NAME,`Codice Catastale del comune`,DGURBA, geometry))%>%
  left_join(Red_21,join_by(`Codice Catastale del comune`==`Codice catastale`))

Red_21_gbregdeg<-units_rd_21%>%
  group_by(Regione,DGURBA, `Anno di imposta`)%>%
  summarise_at(c("taxpayers","count_lavdip","lavdip","count_ret","ret","count_piva","piva","count_regio","regio","count_comu","comu","count_0less","0less","count_10k","10k","count_15k","15k",
                 "count_26k","26k","count_55k","55k","count_75k","75k",
                 "count_120k","120k","count_120+k","120+k"), sum)

by <- c("Regione","DGURBA","Anno di imposta")
library(data.table)
weighted_inc_21 <-setDT(units_rd_21)[,  Map(weighted.mean, mget(grep('%', names(units_rd_21), value=TRUE)), 
                                            mget(grep('count_', names(units_rd_21), value=TRUE))), by = by]  
weighted_inc_21 <- weighted_inc_21[,-c(15,16)]
#####compute aggregate values - region x degurba level######
regdeg_21 <- Red_21_gbregdeg%>%
  left_join(weighted_inc_21[,-3],join_by("Regione","DGURBA"))
regdeg_21[is.na(regdeg_21)] <- 0

empty_cols <- c('avglavdip','avgret','avgpiva','avgadreg','avgadcom', 'avg0less', 
                'avg10k','avg15k','avg26k','avg55k','avg75k','avg120k','avg120+k')

regdeg_21[,empty_cols]<-NA
a <- 42
i <- 6
while(i<31){
  regdeg_21[,a] <- regdeg_21[,i]/regdeg_21[,i-1]
  a<-a+1
  i<-i+2
}
###########ESTIMATING POVERTY#############
#step 1: create a dataset with the variables and the actual values 
# regdeg_red + povrel_byregion (filter by year) 
colnames(harmonize_region_code)<- c("Codice Istat Regione","ITTER107","names")
regdeg_21[is.na(regdeg_21)] <- 0

modelling_estimates21 <- regdeg_21%>%
  left_join(harmonize_region_code,join_by("Regione" == "names"))

modelling_estimates21 <- modelling_estimates21%>%
  left_join(povrelbyregion19_21, join_by("ITTER107","Anno di imposta"=="TIME"))

#Valle D'Aosta's households in relative poverty in 2021 is NA so is proxied by 2020's value
modelling_estimates21[which(modelling_estimates21$ITTER107=="ITC2"),59]=5.4