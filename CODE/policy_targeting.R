library(readr)
library(dplyr)
setwd("~/UNI/THESIS/Poverty-In-Italy-Beyond-the-Rhetoric-of-the-Undeserving-Poor")

recipients19_23 <- read_delim("Dataset/RDC/nuclei_e_importi_per_anno_e_regione.csv", 
                                                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",                                                                                                   grouping_mark = ""), trim_ws = TRUE)
recipients19_23 <- recipients19_23%>%
  group_by(Anno, regione)%>%
  summarise(
    households = sum(Numero_nuclei),
    amount = sum(Classe_importo),
    individuals = sum(Numero_persone_coinvolte)
  )%>%
  mutate(regione = case_when(regione=="Trentino-Alto-Adige"~"Trentino Alto Adige / Südtirol",
                              regione=="Valle d'Aosta/Vallee d'Aoste"~"Valle d'Aosta / Vallée d'Aoste",
                              TRUE~regione))

povrelbyregion19_21 <- read_csv("Dataset/Povertà/povrelbyregion19_21.csv", 
                                                     col_types = cols(`Tipo dato` = col_skip(), 
                                                                      `Flag Codes` = col_skip(),
                                                                      "Seleziona periodo"=col_skip(), 
                                                                      Flags = col_skip()))
povrelbyregion19_21 <- povrelbyregion19_21%>%
  filter(ITTER107 != "IT")

#Valle d'Aosta is NA in 2021

households_prov <- read_csv("Dataset/Povertà/householdsbyprovince19_21.csv", 
                       col_types = cols(TIPO_DATO15 = col_skip(), 
                                        `Tipo di indicatore demografico` = col_skip(), 
                                        SEXISTAT1 = col_skip(), Sesso = col_skip(), 
                                        `Seleziona periodo` = col_skip(), 
                                        `Flag Codes` = col_skip(), Flags = col_skip()))

households_reg <- households_prov%>%
  filter(nchar(ITTER107)==4 & ITTER107 !="ITDA")%>%
  group_by(ITTER107, TIME)%>%
  summarise(
    households_tot = sum(Value)
  )

households_PAs <- households_reg%>%
  filter(ITTER107 %in% c("ITD1","ITD2"))%>%
  group_by(TIME)%>%
  summarise(
    households_tot = sum(households_tot)
  )
households_PAs <- cbind(households_PAs, ITTER107=c("ITDA","ITDA","ITDA"))

households_reg<-rbind(households_reg,households_PAs)

povrelbyregion19_21 <- povrelbyregion19_21%>%
  left_join(households_reg)%>%
  mutate(rel_poor_hous = households_tot*Value)

by = join_by("Territorio"=="regione", "TIME"=="Anno")
targeting <- left_join(povrelbyregion19_21, recipients19_23,by )
targeting<-targeting%>%
  mutate(cov_rate = round(households/rel_poor_hous * 100,3))

#Valle D'Aosta's households in relative poverty in 2021 is NA so is proxied by 2020's value
targeting[which(targeting$ITTER107=="ITC2"& targeting$TIME=="2021"),11]=0.476

households_regional_coverage_rate_povrel <- targeting%>%
  select(Territorio, TIME, cov_rate)

#######Incidence-based Absolute Poverty Coverage Rate and Relative Poverty Coverage Rate
#we have an issue here: Istat releases regional data only on relative poverty. abs
#poverty must be retreived by the HBS. Only the edition of 2021 has the variable povassc.
HBS_2021 <- read_delim("Dataset/SAE/HBS_Microdati_2021.txt", 
                                 delim = "\t", escape_double = FALSE, 
                                 trim_ws = TRUE)
HBS21 <- HBS_2021[,c(1291,1293:1295)]
#group by accounting for the calibration coefficient provided by ISTAT
HBS21 <- HBS21%>%
  group_by(povassc, rgn, rip)%>%
  summarise(
    w_anno = sum(w_anno))
HBS21 <- HBS21%>%
  pivot_wider(names_from="povassc",values_from="w_anno")
colnames(HBS21)<-c("rgn","rip","notpov","pov")
HBS21 <- HBS21%>%
  mutate(
    tot = notpov+pov,
    pov = pov/tot,
    notpov = notpov/tot
  )
ITTER107 = c("ITC1", "ITC2", "ITC4", "ITDA",
"ITD3", "ITD4", "ITC3", "ITD5" ,"ITE1", "ITE2",
"ITE3" ,"ITE4" ,"ITF1" ,"ITF2", "ITF3",
"ITF4", "ITF5", "ITF6" ,"ITG1" ,"ITG2")
TIME = rep(2021,20)
HBS21 <- cbind(HBS21,ITTER107,TIME)
colnames(HBS21)<-c("rgn","rip","notpov","pov","tot_weights","ITTER107","TIME")

povass21 <- HBS21%>%
  left_join(povrelbyregion19_21%>%
              select(c(ITTER107, TIME, Territorio, households_tot)))
povass21 <- povass21%>%
  mutate(diffhouseholdstot = round((tot_weights-households_tot)/tot_weights,3))
#comment on the difference on households_tot and tot_weights 

targeting_pass <- left_join(povass21, recipients19_23,join_by("Territorio"=="regione", "TIME"=="Anno") )
targeting_pass<-targeting_pass%>%
  mutate(cov_rate = round(households/(pov*tot_weights),3)*100)

households_regional_coverage_rate_povass <- targeting_pass%>%
  select(Territorio, TIME, cov_rate)






#######VIZ######
households_regional_coverage_rate_povrel<- households_regional_coverage_rate_povrel%>%
  mutate(`Territorio`= case_when(
  `Territorio`=="Valle d'Aosta / Vallée d'Aoste"~"Valle d'Aosta/Vallée d'Aoste",
  `Territorio`=="Trentino Alto Adige / Südtirol"~"Trentino-Alto Adige/Südtirol",
  TRUE ~`Territorio`
))

viz_cr_region19 <- households_regional_coverage_rate_povrel%>%
  filter(TIME=="2019")%>%
  right_join(deg_viz, join_by("Territorio"=="Regione"))
#errorcrviz2019<- households_regional_coverage_rate_povrel%>%
#  filter(TIME=="2019")%>%
#  anti_join(deg_viz, join_by("Territorio"=="Regione"))
viz_cr_region20 <- households_regional_coverage_rate_povrel%>%
  filter(TIME=="2020")%>%
  right_join(deg_viz, join_by("Territorio"=="Regione"))
viz_cr_region21 <- households_regional_coverage_rate_povrel%>%
  filter(TIME=="2021")%>%
  right_join(deg_viz, join_by("Territorio"=="Regione"))
library(ggplot2)

viz_cr_region <- rbind(viz_cr_region19,viz_cr_region20,viz_cr_region21)
CR_relativepov <- viz_cr_region%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill=cov_rate,color=cov_rate)) + 
  ggtitle("RdC/PdC Coverage Rate on Household Relative Poverty - Region level in 2019") + 
  coord_sf() + 
  scale_fill_continuous(type = "viridis")+
  scale_color_continuous(type = "viridis")+ 
  facet_grid(. ~ TIME)
CR_relativepov

viz_cr_region21_pass <- households_regional_coverage_rate_povass%>%
  mutate(rgn = as.factor(rgn))%>%
  right_join(deg_viz, join_by("rgn"=="reg_code"))
CR_2021_pass <- viz_cr_region21_pass%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill=cov_rate,color=cov_rate)) + 
  ggtitle("RdC/PdC Coverage Rate on Household Absolute Poverty - Region level in 2021") + 
  coord_sf() + 
  scale_fill_continuous(type = "viridis", option = "magma")+
  scale_color_continuous(type = "viridis", option = "magma")
CR_2021_pass

