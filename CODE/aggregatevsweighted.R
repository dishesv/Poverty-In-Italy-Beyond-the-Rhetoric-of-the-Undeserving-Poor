###Group by region and group by regXdegurba###
####region####
Red_19_gbregion <- Red_19%>%
  group_by(Regione,`Codice Istat Regione`, `Anno di imposta`)%>%
  summarise_at(vars("taxpayers":32), sum)
#####compute aggregate values - region level######
###AVG###
empty_cols <- c('avglavdip','avgret','avgpiva','avgadreg','avgadcom', 'avg0less', 
                'avg10k','avg15k','avg26k','avg55k','avg75k','avg120k','avg120+k')
Red_19_gbregion[,empty_cols]<-NA
a <-31
i <-6
while(i<31){
  Red_19_gbregion[,a] <- Red_19_gbregion[,i]/Red_19_gbregion[,i-1]
  a<-a+1
  i<-i+2
}
###%###
empty_cols <- c('%lavdip','%ret','%piva','adreg-tbr','adcom-tbr', 
                '%0less', '%10k','%15k','%26k','%55k','%75k','%120k','%120+k')
Red_19_gbregion[,empty_cols]<-NA
a <-44
i <-9
while(i < 31){
  Red_19_gbregion[,a] <- round(Red_19_gbregion[,i]/Red_19_gbregion[,4],3) #count of each category/contribuenti
  a<-a+1
  i<-i+2
}

Red_19_gbregion <- Red_19_gbregion[,-c(47,48)] #remove incidence of regional and municipal tax

#weighted incidence and average  
weighted_incidences_19 <- as.data.frame(setDT(Red_19)[,  Map(weighted.mean, mget(grep('%', names(Red_19), 
                                                                                      value=TRUE)), mget(grep('count_', names(Red_19), value=TRUE))), by = `Codice Istat Regione`]  )
weighted_incidences_19 <- weighted_incidences_19[,-c(13:14)]
weighted_averages_19 <- as.data.frame(setDT(Red_19)[,  Map(weighted.mean, mget(grep('avg', names(Red_19), 
                                                                                    value=TRUE)), mget(grep('count_', names(Red_19), value=TRUE))), by = `Codice Istat Regione`]  )

region_red <- Red_19_gbregion%>%
  left_join(weighted_averages_19,join_by( `Codice Istat Regione`),suffix = c("",".w"))%>%
  left_join(weighted_incidences_19,join_by( `Codice Istat Regione`),suffix = c("",".w"))

region_red[is.na(region_red)] <- 0
check<- region_red[,c(1,31:78)]
colnames(check)[1]<-"#regione"
check<-check[,order(colnames(check))]
####Difference aggregate and weighted aggregate####
empty_cols <- c('avglavdip.z','avgret.z','avgpiva.z','avgadreg.z','avgadcom.z', 'avg0less.z', 
                'avg10k.z','avg15k.z','avg26k.z','avg55k.z','avg75k.z','avg120k.z','avg120+k.z',
                '%lavdip.z','%ret.z','%piva.z', 
                '%0less.z', '%10k.z','%15k.z','%26k.z','%55k.z','%75k.z','%120k.z','%120+k.z')
check[,empty_cols]<-NA
check<-check[,order(colnames(check))]
a <-4
i <-2
while(i < 72){
  check[,a] <- check[,i]-check[,i+1]
  a<-a+3
  i<-i+3
}
library(ggplot2)
check_visual<-check[,c(1,4,7,16,19,22,25,10,13,28,31,34)]%>%
  tidyr::pivot_longer(!"#regione", names_to="var")
colnames(check_visual)[1]<-"area"
check_visual<-check_visual%>%
  mutate(area =as.factor(area))
design <- "
ABCDE
FGHIJ
KLMNO
PQRST"
library(ggh4x)
check_visual%>%
  ggplot(aes(var, value,fill=var)) +
  geom_col() +
  geom_text(aes(label=round(value,2)),check_overlap = TRUE,size=2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(limits = c(-0.6, 0.6), breaks = seq(-0.6, 0.6, by = 0.2))+
  facet_manual(vars(area), design = design)+
  labs(title = "Difference between aggregate proportions and weighted aggregate proportions")
summary(check[,c(1,4,7,16,19,22,25,10,13,28,31,34)])


#####regionXdegurba#####
Red_19_gbregionanddegurba_units<- classified_units%>%
  select(c(LAU_NAME,`Codice Catastale del comune`,DGURBA, geometry))%>%
  left_join(Red_19,join_by(`Codice Catastale del comune`==`Codice catastale`))
#how many NA?????
Red_19_gbregdeg<-Red_19_gbregionanddegurba_units%>%
  group_by(Regione,DGURBA, `Anno di imposta`)%>%
  summarise_at(vars("taxpayers","count_lavdip","lavdip","count_ret","ret","count_piva","piva","count_regio","regio","count_comu","comu","count_0less","0less","count_10k","10k","count_15k","15k",
                    "count_26k","26k","count_55k","55k","count_75k","75k",
                    "count_120k","120k","count_120+k","120+k"), sum)

by <- c("Regione","DGURBA","Anno di imposta")
library(data.table)
weighted_incidences_19_d <-setDT(Red_19_gbregionanddegurba_units)[,  Map(weighted.mean, mget(grep('%', names(Red_19_gbregionanddegurba_units), 
                                                                                                  value=TRUE)), mget(grep('count_', names(Red_19_gbregionanddegurba_units), value=TRUE))), by = by]  
weighted_incidences_19_d <- weighted_incidences_19_d[,-c(15,16)]
weighted_averages_19_d<- setDT(Red_19_gbregionanddegurba_units)[,  Map(weighted.mean, mget(grep('avg', names(Red_19_gbregionanddegurba_units), 
                                                                                                value=TRUE)), mget(grep('count_', names(Red_19_gbregionanddegurba_units), value=TRUE))), by = by]  
#####compute aggregate values - region x degurba level######
###AVG###
empty_cols <- c('avglavdip','avgret','avgpiva','avgadreg','avgadcom', 'avg0less', 
                'avg10k','avg15k','avg26k','avg55k','avg75k','avg120k','avg120+k')
Red_19_gbregdeg[,empty_cols]<-NA
a <-31
i <-6
while(i<31){
  Red_19_gbregdeg[,a] <- Red_19_gbregdeg[,i]/Red_19_gbregdeg[,(i-1)]
  a<-a+1
  i<-i+2
}
###%###
empty_cols <- c('%lavdip','%ret','%piva','adreg-tbr','adcom-tbr', 
                '%0less', '%10k','%15k','%26k','%55k','%75k','%120k','%120+k')
Red_19_gbregdeg[,empty_cols]<-NA
a <-44
i <-9
while(i < 31){
  Red_19_gbregdeg[,a] <- round(Red_19_gbregdeg[,i]/Red_19_gbregdeg[,4],3) #count of each category/contribuenti
  a<-a+1
  i<-i+2
}
Red_19_gbregdeg <- Red_19_gbregdeg[,-c(47,48)] #remove incidence of regional and municipal tax
regdeg_red <- Red_19_gbregdeg%>%
  left_join(weighted_averages_19_d[,-3],join_by("Regione","DGURBA"),suffix = c("",".w"))%>%
  left_join(weighted_incidences_19_d[,-3],join_by("Regione","DGURBA"),suffix = c("",".w"))
regdeg_red[is.na(regdeg_red)] <- 0
checkd<- regdeg_red[,c(1,31:78)]
colnames(checkd)[1]<-"#regione"
checkd<-checkd[,order(colnames(checkd))]
###aggregated values-weighted###
empty_cols <- c('avglavdip.z','avgret.z','avgpiva.z','avgadreg.z','avgadcom.z', 'avg0less.z', 
                'avg10k.z','avg15k.z','avg26k.z','avg55k.z','avg75k.z','avg120k.z','avg120+k.z',
                '%lavdip.z','%ret.z','%piva.z', 
                '%0less.z', '%10k.z','%15k.z','%26k.z','%55k.z','%75k.z','%120k.z','%120+k.z')
checkd[,empty_cols]<-NA
checkd<-checkd[,order(colnames(checkd))]
a <-4
i <-2
while(i < 72){
  checkd[,a] <- checkd[,i]-checkd[,i+1]
  a<-a+3
  i<-i+3
}
checkd_visual<-checkd[,c(1,4,7,16,19,22,25,10,13,28,31,34)]%>%
  tidyr::pivot_longer(!"#regione", names_to="var")
colnames(checkd_visual)[1]<-"area"
checkd_visual<-checkd_visual%>%
  mutate(area =as.factor(area))
design <- "
ABCDE
FGHIJ
KLMNO
PQRST"
library(ggh4x)
checkd_visual%>%
  ggplot(aes(var, value,fill=var)) +
  geom_col() +
  geom_text(aes(label=round(value,2)),check_overlap = TRUE,size=2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(limits = c(-0.6, 0.6), breaks = seq(-0.6, 0.6, by = 0.2))+
  facet_manual(vars(area), design = design)+
  labs(title = "Difference between aggregate proportions and weighted aggregate proportions - DEGURBAxRegion")
summary(check[,c(1,4,7,16,19,22,25,10,13,28,31,34)])
#The main insight is that the differences between the
#incidence computed on the aggregate totals and weighted
#by the count of each category the group behaves consistently 
#across all areas (although, with different degrees), except for the 
#category 0-10k --> use .w for the proportions
