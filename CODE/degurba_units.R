######CREATION OF THE DEGURBA CLASSIFICATION FILE#######

install.packages("sf")
install.packages("dplyr")
install.packages("magrittr")
install.packages("cartography")

library(sf)
library(dplyr)
library(magrittr)
library(readr)
#library(cartography)

setwd("~/UNI/THESIS/Poverty-In-Italy-Beyond-the-Rhetoric-of-the-Undeserving-Poor")

shapefile <- st_read("Dataset/SAE/DGURBA-2020-01M-SH/DGURBA-2020-01M-SH.shp")
degurba_units <- shapefile %>%
  filter(CNTR_CODE == "IT") %>%
  mutate(DGURBA = as.factor(DGURBA))%>%
  as_tibble()

#degurba_units<-degurba_units%>%
#  mutate(LAU_NAME = case_when(LAU_NAME=="Bardello"~"Bardello con Malgesso e Bregano",
#                              LAU_NAME=="Moransengo"~"Moransengo-Tonengo",
#                              LAU_ID=="031022"~"Savogna d'Isonzo",
#                              TRUE~LAU_NAME))
degurba_units<-degurba_units%>%
  mutate(LAU_NAME = case_when(LAU_ID=="031022"~"Savogna d'Isonzo",
                              TRUE~LAU_NAME))

denominazioni_territoriali <- read_delim("Dataset/SAE/denominazioni_territoriali.tsv", 
                                         delim = "\t", escape_double = FALSE, 
                                         trim_ws = TRUE)
denominazioni_territoriali<- denominazioni_territoriali%>%
  select(c("Denominazione (Italiana e straniera)",
           "Codice Ripartizione Geografica",
           "Ripartizione geografica",
           "Codice Regione",
           "Denominazione Regione",
           "Codice Provincia (Storico)(1)",
           "Denominazione dell'Unità territoriale sovracomunale \r\n(valida a fini statistici)",
           "Denominazione in italiano","Denominazione altra lingua",
           "Tipologia di Unità territoriale sovracomunale",
           "Sigla automobilistica",
           "Codice Catastale del comune",
           "Codice NUTS3 2021"     ))

denominazioni_territoriali<-rbind(denominazioni_territoriali,
c("Moransengo",1,"Nord-ovest",1,"Piemonte",5,"Asti","Moransengo",NA,1,"AT","F709","ITC17"),
c("Tonengo",1,"Nord-ovest",1,"Piemonte",5,"Asti","Tonengo",NA,1,"AT","L204","ITC17"),
c("Bardello",1,"Nord-ovest",3,"Lombardia",12,"Varese","Bardello",NA,1,"VA","A645","ITC41"),
c("Malgesso",1,"Nord-ovest",3,"Lombardia",12,"Varese","Malgesso",NA,1,"VA","E856","ITC41"),
c("Bregano",1,"Nord-ovest",3,"Lombardia",12,"Varese","Bregano",NA,1,"VA","B131","ITC41"))

denominazioni_territoriali <- denominazioni_territoriali%>%
  mutate(`Denominazione (Italiana e straniera)`= case_when(
    `Denominazione (Italiana e straniera)`=="Casorzo Monferrato"~"Casorzo",
    `Denominazione (Italiana e straniera)`=="Doberdò del Lago-Doberdob"~"Doberdò del Lago/Doberdob",
    `Denominazione (Italiana e straniera)`=="Grana Monferrato"~"Grana",
    `Denominazione (Italiana e straniera)`=="Malé"~"Malè",
    `Denominazione (Italiana e straniera)`=="Montagna sulla Strada del Vino/Montan an der Weinstraße"~"Montagna/Montan",
    `Denominazione (Italiana e straniera)`=="Pont Canavese"~"Pont-Canavese",
    `Denominazione (Italiana e straniera)`=="Puegnago del Garda"~"Puegnago sul Garda",
    `Denominazione (Italiana e straniera)`=="Trentola Ducenta"~"Trentola-Ducenta",
    `Denominazione (Italiana e straniera)`=="San Giovanni di Fassa-Sèn Jan"~"Sèn Jan di Fassa-Sèn Jan",
    `Denominazione (Italiana e straniera)`=="Savogna d'Isonzo-Sovodnje ob So?i"~"Savogna d'Isonzo",
    `Denominazione (Italiana e straniera)`=="Sgonico-Zgonik"~"Sgonico",
    TRUE ~`Denominazione (Italiana e straniera)`
  ))

classified_units<-degurba_units%>%
  select(c(LAU_NAME,LAU_ID,DGURBA,geometry))%>%
  inner_join(denominazioni_territoriali,join_by("LAU_NAME"=="Denominazione (Italiana e straniera)"))
#There are two Samone (one in Piemonte (LAU_ID 001235) the other in TAAD(LAU_ID 022165)) but they have the same degurba (2)
rown_samone = c(which(classified_units$`Codice Regione` == 1&classified_units$LAU_ID=="022165"),
                which(classified_units$`Codice Regione`==4&classified_units$LAU_ID=="001235"))
classified_units<-classified_units%>%
  filter(!row_number() %in% rown_samone)

missingdegurba<-degurba_units%>%
  select(LAU_NAME)%>%
  anti_join((denominazioni_territoriali ),join_by("LAU_NAME"=="Denominazione (Italiana e straniera)"))

missingterdenom<-denominazioni_territoriali%>%
  select(c("Denominazione (Italiana e straniera)",`Denominazione in italiano`))%>%
  anti_join((degurba_units ),join_by("Denominazione (Italiana e straniera)"=="LAU_NAME"))

#Importing and matching the variables to the units
Redditi_IRPEF_2019 <- read_delim("Dataset/SAE/COV/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2019.csv", 
                                 delim = ";", escape_double = FALSE, col_types = cols(`Codice Istat Regione` = col_integer()), trim_ws = TRUE)
Redditi_IRPEF_2020 <- read_delim("Dataset/SAE/COV/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2020.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
Redditi_IRPEF_2021 <- read_delim("Dataset/SAE/COV/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2021.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
check2019a <- classified_units%>%
  select(c(LAU_NAME,`Codice Catastale del comune`))%>%
  anti_join(Redditi_IRPEF_2019,join_by(`Codice Catastale del comune`==`Codice catastale`))%>%
  select(c(LAU_NAME,`Codice Catastale del comune`))
colnames(check2019a)=c("name","codcat")
check2019b <- Redditi_IRPEF_2019%>%
  select(c(`Codice catastale`,`Denominazione Comune`))%>%
  anti_join(classified_units,join_by(`Codice catastale`==`Codice Catastale del comune`))%>%
  select(c(`Denominazione Comune`,`Codice catastale`))
colnames(check2019b)=c("name","codcat")
check2019<-rbind(check2019a,check2019b) #this were the missing matches, 4 rows only

check2020a <- classified_units%>%
  select(c(LAU_NAME,`Codice Catastale del comune`))%>%
  anti_join(Redditi_IRPEF_2020,join_by(`Codice Catastale del comune`==`Codice catastale`))%>%
  select(c(LAU_NAME,`Codice Catastale del comune`))
colnames(check2020a)=c("name","codcat")
check2020b <- Redditi_IRPEF_2020%>%
  select(c(`Codice catastale`,`Denominazione Comune`))%>%
  anti_join(classified_units,join_by(`Codice catastale`==`Codice Catastale del comune`))%>%
  select(c(`Denominazione Comune`,`Codice catastale`))
colnames(check2020b)=c("name","codcat")
check2020<-rbind(check2020a,check2020b) #this were the missing matches, 4 rows only

check2021a <- classified_units%>%
  select(c(LAU_NAME,`Codice Catastale del comune`))%>%
  anti_join(Redditi_IRPEF_2021,join_by(`Codice Catastale del comune`==`Codice catastale`))%>%
  select(c(LAU_NAME,`Codice Catastale del comune`))
colnames(check2021a)=c("name","codcat")
check2021b <- Redditi_IRPEF_2021%>%
  select(c(`Codice catastale`,`Denominazione Comune`))%>%
  anti_join(classified_units,join_by(`Codice catastale`==`Codice Catastale del comune`))%>%
  select(c(`Denominazione Comune`,`Codice catastale`))
colnames(check2021b)=c("name","codcat")
check2021<-rbind(check2021a,check2021b) #this were the missing matches, 5 rows only

Red_19 <- Redditi_IRPEF_2019[,c(1:8,13,14,35:50)]
colnames(Red_19)[9:26]<-c("count_retired","pensions","count_0","zero_or_less","count_10k","10k","count_10_15k","10_15k",
                          "count_15_26k","15_26k","count_26_55k","26_55k","count_55_75k","55_75k",
                          "count_75_120k","75_120k","count_120k+","120k+")
Red_19 <- Red_19%>%
  mutate(
    avg_pension = pensions/count_retired,
    avg0 = zero_or_less/count_0 ,
    avg10 = `10k`/count_10k,
    avg15 = `10_15k`/count_10_15k,
    avg26 = `15_26k`/count_15_26k,
    avg55 = `26_55k`/count_26_55k,
    avg75 = `55_75k`/count_55_75k,
    avg120 = `75_120k`/count_75_120k,
    avg_over_120 = `120k+`/`count_120k+`)
#more manipulation of this dataset will be done
harmonize_region_code <- cbind(
  1:20,
  ITTER107
)
harmonize_region_code <- as.data.frame(harmonize_region_code)%>%
  mutate(V1 = as.integer(V1))
colnames(harmonize_region_code)<- c("Codice Istat Regione","ITTER107")
Region_19 <- Red_19%>%
  full_join(harmonize_region_code)
Region_19 <- Region_19%>%
  left_join(povrelbyregion19_21%>%
              filter(TIME=="2019"))



########INSIGHTS######
# now on degurba, bur can be made also with the threefold comune type classification
#here i want to know % of the three categories for the 4 levels; national, area, region and province
colnames(classified_units)[6] <- "area"
colnames(classified_units)[7] <- "reg_code"
colnames(classified_units)[8] <- "Regione"
colnames(classified_units)[10] <- "Provincia"
colnames(classified_units)[14] <- "prov_code"


degurba_facts_prov <- classified_units%>%
  group_by(area,Regione,reg_code, Provincia, prov_code)%>%
  summarise(
    n = n(),
    D1 = sum(DGURBA==1),
    D2 = sum(DGURBA==2),
    D3 = sum(DGURBA==3)
  )
degurba_facts_prov<-degurba_facts_prov%>%
  mutate(D1_prc = round(D1/n,2),
         D2_prc = round(D2/n,2),
         D3_prc = round(D3/n,2),
         deg_prov = case_when(D1_prc>0.49 ~ "D1",
                              D2_prc>0.49 ~ "D2",
                              D3_prc>0.49 ~ "D3"))

degurba_facts_region <- degurba_facts_prov%>%
  group_by(area,Regione, reg_code)%>%
  summarise(
    n_com = sum(n),
    n_prov = n(),
    D1 = sum(D1),
    D2 = sum(D2),
    D3 = sum(D3)
  )
degurba_facts_region<-degurba_facts_region%>%
  mutate(D1_prc = round(D1/n_com,2),
         D2_prc = round(D2/n_com,2),
         D3_prc = round(D3/n_com,2),
         deg_reg = case_when(D1_prc>0.49 ~ "D1",
                              D2_prc>0.49 ~ "D2",
                              D3_prc>0.49 ~ "D3"))


degurba_facts_area <- degurba_facts_region%>%
  group_by(area)%>%
  summarise(
    n_com = sum(n_com),
    n_prov = sum(n_prov),
    n_reg = n(),
    D1 = sum(D1),
    D2 = sum(D2),
    D3 = sum(D3)
  )
degurba_facts_area<-degurba_facts_area%>%
  mutate(D1_prc = round(D1/n_com,2),
         D2_prc = round(D2/n_com,2),
         D3_prc = round(D3/n_com,2),
         deg_area = case_when(D1_prc>0.49 ~ "D1",
                              D2_prc>0.49 ~ "D2",
                              D3_prc>0.49 ~ "D3"))

degurba_facts <-degurba_facts_area%>%
  summarise(
    n_com = sum(n_com),
    n_prov = sum(n_prov),
    n_reg = sum(n_reg),
    D1 = sum(D1),
    D2 = sum(D2),
    D3 = sum(D3)
  )
degurba_facts<-degurba_facts%>%
  mutate(D1_prc = round(D1/n_com,2),
         D2_prc = round(D2/n_com,2),
         D3_prc = round(D3/n_com,2))

deg_match <- degurba_facts_prov%>%
  select(c(area, reg_code, prov_code, deg_prov))%>%
  inner_join(degurba_facts_region%>%
               select(deg_reg,reg_code))%>%
  inner_join(degurba_facts_area%>%
               select(area, deg_area))

deg_viz <- classified_units%>%
  select(c(3,4,7,8,9,10,11))%>%
  inner_join(deg_match)%>%
  mutate(DGURBA = as.factor(DGURBA),
         deg_prov = as.factor(deg_prov),
         deg_reg = as.factor(deg_reg),
         deg_area = as.factor(deg_area))
library(readr)
write_delim(classified_units[,c(1,3,4,8,10)],delim=";", "DataViz/mapping.csv")

#######Integration with HBS######### let's pray it will be possible
#AVQ_Microdati_2019 <- read_delim("Dataset/SAE/HBS/AVQ_Microdati_2019.txt", 
#                                 delim = "\t", escape_double = FALSE, 
#                                 trim_ws = TRUE)
#colnames(AVQ_Microdati_2019)
#povassc
#rgn


######VIZ######
library(ggplot2)

mun<-deg_viz%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill=DGURBA, color=DGURBA)) + 
  ggtitle("Italy by Degree of Urbanization - Municipality ") + 
  coord_sf()

prov<-deg_viz%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill=deg_prov, color=deg_prov)) + 
  ggtitle("Italy by Degree of Urbanization - Province") + 
  coord_sf()

reg<-deg_viz%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill=deg_reg, color=deg_reg)) + 
  ggtitle("Italy by Degree of Urbanization - Region") + 
  coord_sf()

area<-deg_viz%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill=deg_area, color=deg_area)) + 
  ggtitle("Italy by Degree of Urbanization - Area") + 
  coord_sf()
