#####2020 - case one #####
Red_20 <- Redditi_IRPEF_2020[,c(1:8,11:16,31:50)]
#remove one faulty col
Red_20 <- Red_20[-7904,]
Red_20[9:34][is.na(Red_20[9:34])] <- 0
###AVGs###
empty_cols <- c('avglavdip','avgret','avgpiva','avgadreg','avgadcom', 'avg0less', 
                'avg10k','avg15k','avg26k','avg55k','avg75k','avg120k','avg120+k')
Red_20[,empty_cols]<-NA
a <-35
i <-10
while(i<36){
  Red_20[,a] <- Red_20[,i]/Red_20[,i-1]
  a<-a+1
  i<-i+2
}
###%###computing the incidence of the different categories over the total number of taxpayers###
Red_20[9:47][is.na(Red_20[9:47])] <- 0
empty_cols <- c('%lavdip','%ret','%piva','adreg-tbr','adcom-tbr', 
                '%0less', '%10k','%15k','%26k','%55k','%75k','%120k','%120+k')
Red_20[,empty_cols]<-NA
a <-48
i <-9
while(i<34){
  Red_20[,a] <- round(Red_20[,i]/Red_20[,8],3) #count of each category/contribuenti
  a<-a+1
  i<-i+2
}

Red_20 <- Red_20[,-c(51,52)] #remove incidence of regional and municipal tax

colnames(Red_20)[8:34]<-c("taxpayers","count_lavdip","lavdip","count_ret","ret","count_piva","piva","count_regio","regio","count_comu","comu","count_0less","0less","count_10k","10k","count_15k","15k",
                          "count_26k","26k","count_55k","55k","count_75k","75k",
                          "count_120k","120k","count_120+k","120+k")
#join PAs Bolzano & Trento
Red_20<- Red_20%>%
  mutate(Regione = ifelse(`Codice Istat Regione`== "04", "Trentino Alto Adige",Regione))

###Group by region and group by regXdegurba###
#####regionXdegurba#####
units_rd_20<- classified_units%>%
  dplyr::select(c(LAU_NAME,`Codice Catastale del comune`,DGURBA, geometry))%>%
  left_join(Red_20,join_by(`Codice Catastale del comune`==`Codice catastale`))

Red_20_gbregdeg<-units_rd_20%>%
  group_by(Regione,DGURBA, `Anno di imposta`)%>%
  summarise_at(c("taxpayers","count_lavdip","lavdip","count_ret","ret","count_piva","piva","count_regio","regio","count_comu","comu","count_0less","0less","count_10k","10k","count_15k","15k",
                    "count_26k","26k","count_55k","55k","count_75k","75k",
                    "count_120k","120k","count_120+k","120+k"), sum)

by <- c("Regione","DGURBA","Anno di imposta")
library(data.table)
weighted_inc_20 <-setDT(units_rd_20)[,  Map(weighted.mean, mget(grep('%', names(units_rd_20), value=TRUE)), 
                  mget(grep('count_', names(units_rd_20), value=TRUE))), by = by]  
weighted_inc_20 <- weighted_inc_20[,-c(15,16)]
#####compute aggregate values - region x degurba level######
regdeg_20 <- Red_20_gbregdeg%>%
  left_join(weighted_inc_20[,-3],join_by("Regione","DGURBA"))
regdeg_20[is.na(regdeg_20)] <- 0

empty_cols <- c('avglavdip','avgret','avgpiva','avgadreg','avgadcom', 'avg0less', 
                'avg10k','avg15k','avg26k','avg55k','avg75k','avg120k','avg120+k')

regdeg_20[,empty_cols]<-NA
a <- 42
i <- 6
while(i<31){
  regdeg_20[,a] <- regdeg_20[,i]/regdeg_20[,i-1]
  a<-a+1
  i<-i+2
}
###########ESTIMATING POVERTY#############
#step 1: create a dataset with the variables and the actual values 
# regdeg_red + povrel_byregion (filter by year) 
colnames(harmonize_region_code)<- c("Codice Istat Regione","ITTER107","names")
regdeg_20[is.na(regdeg_20)] <- 0

modelling_estimates20 <- regdeg_20%>%
  left_join(harmonize_region_code,join_by("Regione" == "names"))

modelling_estimates20 <- modelling_estimates20%>%
  left_join(povrelbyregion19_21, join_by("ITTER107","Anno di imposta"=="TIME"))