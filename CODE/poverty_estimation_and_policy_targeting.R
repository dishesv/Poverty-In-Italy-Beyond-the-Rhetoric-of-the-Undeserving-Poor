setwd("~/UNI/THESIS/Poverty-In-Italy-Beyond-the-Rhetoric-of-the-Undeserving-Poor")
#####for easier joins#####
ITTER107 = c("ITC1", "ITC2", "ITC4", "ITDA",
             "ITD3", "ITD4", "ITC3", "ITD5" ,"ITE1", "ITE2",
             "ITE3" ,"ITE4" ,"ITF1" ,"ITF2", "ITF3",
             "ITF4", "ITF5", "ITF6" ,"ITG1" ,"ITG2")
harmonize_region_code <- cbind(
  1:20,
  ITTER107,
  c(  "Piemonte" ,"Valle d'Aosta"  ,"Lombardia", "Trentino Alto Adige" , "Veneto" ,              
      "Friuli Venezia Giulia" ,  "Liguria"  ,   "Emilia Romagna" ,
      "Toscana" , "Umbria"  , "Marche" , "Lazio" ,
      "Abruzzo"  , "Molise" ,  "Campania"  , "Puglia" ,         
      "Basilicata"  , "Calabria","Sicilia" ,  "Sardegna" )
)
harmonize_region_code <- as.data.frame(harmonize_region_code)%>%
  mutate(V1 = as.integer(V1),
         V3 = as.factor(V3))
colnames(harmonize_region_code)<-c("Codice Istat Regione","ITTER107","name")
smallareas <- data.frame(c("Abruzzo_1","Abruzzo_2","Abruzzo_3","Basilicata_1" ,"Basilicata_2",           
"Basilicata_3","Calabria_1","Calabria_2" ,"Calabria_3","Campania_1" ,            
"Campania_2","Campania_3" ,"Emilia Romagna_1","Emilia Romagna_2",       
"Emilia Romagna_3", "Friuli Venezia Giulia_1","Friuli Venezia Giulia_2",
"Friuli Venezia Giulia_3","Lazio_1","Lazio_2" ,"Lazio_3" ,               
"Liguria_1","Liguria_2","Liguria_3","Lombardia_1","Lombardia_2",            
"Lombardia_3","Marche_1","Marche_2","Marche_3","Molise_1","Molise_2",               
"Molise_3","Piemonte_1","Piemonte_2","Piemonte_3","Puglia_1","Puglia_2",               
"Puglia_3","Sardegna_1", "Sardegna_2","Sardegna_3","Sicilia_1","Sicilia_2",              
"Sicilia_3","Toscana_1","Toscana_2","Toscana_3","Trentino Alto Adige_1",  
"Trentino Alto Adige_2","Trentino Alto Adige_3","Umbria_1","Umbria_2","Umbria_3",               
"Valle d'Aosta_2","Valle d'Aosta_3","Veneto_1","Veneto_2","Veneto_3"), 
 c(13,13,13,17,17,17,18,18,18,15,15,15,8,8,8,6,6,6,
   12,12,12,7,7,7,3,3,3,11,11,11,14,14,14,1,1,1,
   16,16,16,20,20,20,19,19,19,9,9,9,4,4,4,
   10,10,10,2,2,5,5,5) )  
colnames(smallareas)<-c("smarea","ISTAT_code")
smallareas <- smallareas %>%
  mutate(ISTAT_code = as.integer(ISTAT_code))%>%
  left_join(harmonize_region_code, join_by(ISTAT_code=="Codice Istat Regione"))

#DEGURBA####
shapefile <- st_read("Dataset/SAE/DGURBA-2020-01M-SH/DGURBA-2020-01M-SH.shp")
degurba_units <- shapefile %>%
  filter(CNTR_CODE == "IT") %>%
  mutate(DGURBA = as.factor(DGURBA))%>%
  as_tibble()
library(dplyr)
degurba_units<-degurba_units%>%
  mutate(LAU_NAME = case_when(LAU_ID=="031022"~"Savogna d'Isonzo",
                              TRUE~LAU_NAME))

denominazioni_territoriali <- read_delim("Dataset/SAE/denominazioni_territoriali.tsv", 
                                         delim = "\t", escape_double = FALSE, 
                                         trim_ws = TRUE)
denominazioni_territoriali<- denominazioni_territoriali%>%
  dplyr::select(c("Denominazione (Italiana e straniera)",
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

library(sf)
library(dplyr)
classified_units<-degurba_units%>%
  dplyr::select(c(LAU_NAME,LAU_ID,DGURBA,geometry))%>%
  inner_join(denominazioni_territoriali,join_by("LAU_NAME"=="Denominazione (Italiana e straniera)"))
#There are two Samone (one in Piemonte (LAU_ID 001235) the other in TAAD(LAU_ID 022165)) but they have the same degurba (2)
rown_samone = c(which(classified_units$`Codice Regione` == 1&classified_units$LAU_ID=="022165"),
                which(classified_units$`Codice Regione`==4&classified_units$LAU_ID=="001235"))
classified_units<-classified_units%>%
  filter(!row_number() %in% rown_samone)

missingdegurba<-degurba_units%>%
  dplyr::select(LAU_NAME)%>%
  anti_join((denominazioni_territoriali ),join_by("LAU_NAME"=="Denominazione (Italiana e straniera)"))

missingterdenom<-denominazioni_territoriali%>%
  dplyr::select(c("Denominazione (Italiana e straniera)",`Denominazione in italiano`))%>%
  anti_join((degurba_units ),join_by("Denominazione (Italiana e straniera)"=="LAU_NAME"))
##Statistics on degurba ----
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
  dplyr::select(c(area, reg_code, prov_code, deg_prov))%>%
  inner_join(degurba_facts_region%>%
               dplyr::select(deg_reg,reg_code))%>%
  inner_join(degurba_facts_area%>%
               dplyr::select(area, deg_area))
## Prepping the structure to visualise it ----
deg_viz <- classified_units%>%
  dplyr::select(c(3,4,7,8,9,10,11))%>%
  inner_join(deg_match)%>%
  mutate(DGURBA = as.factor(DGURBA),
         deg_prov = as.factor(deg_prov),
         deg_reg = as.factor(deg_reg),
         deg_area = as.factor(deg_area))

#POLICY TARGETING - Regional available data ----
#1: with regional data
library(readr)
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
  mutate(rel_poor_hous = households_tot*(Value/100))

by = join_by("Territorio"=="regione", "TIME"=="Anno")
targeting <- left_join(povrelbyregion19_21, recipients19_23,by )
targeting<-targeting%>%
  mutate(cov_rate = households/rel_poor_hous * 100)

#Valle D'Aosta's households in relative poverty in 2021 is NA so is proxied by 2020's value 1560/3277.692
targeting[which(targeting$ITTER107=="ITC2"& targeting$TIME=="2021"),11]=47.59447

households_regional_coverage_rate_povrel <- targeting[,c(1,2,4,11)]
  
#######Incidence-based Absolute Poverty Coverage Rate and Relative Poverty Coverage Rate
#we have an issue here: Istat releases regional data only on relative poverty. abs
#poverty must be retreived by the HBS. Only the edition of 2021 has the variable povassc.
HBS_2021 <- read_delim("Dataset/SAE/HBS_21/HBS_Microdati_2021.txt", 
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

TIME = rep(2021,20)
HBS21 <- cbind(HBS21,ITTER107,TIME)
colnames(HBS21)<-c("rgn","rip","notpov","pov","tot_weights","ITTER107","TIME")

povass21 <- HBS21%>%
  left_join(povrelbyregion19_21%>%
              dplyr::select(c(ITTER107, TIME, Territorio, households_tot)))
povass21 <- povass21%>%
  mutate(diffhouseholdstot = round((tot_weights-households_tot)/tot_weights,3))
#comment on the difference on households_tot and tot_weights 

targeting_pass <- left_join(povass21, recipients19_23,join_by("Territorio"=="regione", "TIME"=="Anno") )
targeting_pass<-targeting_pass%>%
  mutate(cov_rate = round(households/(pov*tot_weights),3)*100)

households_regional_coverage_rate_povass <- targeting_pass%>%
  dplyr::select(Territorio, TIME, cov_rate)

#Prepping the structure for visualisation
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

#IRPEF data and Poverty estimation ----
### Importing and matching the variables to the degurba units ----
Redditi_IRPEF_2019 <- read_delim("Dataset/SAE/COV/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2019.csv", 
                                 delim = ";", escape_double = FALSE, col_types = cols(`Codice Istat Regione` = col_integer()), trim_ws = TRUE)
Redditi_IRPEF_2020 <- read_delim("Dataset/SAE/COV/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2020.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
Redditi_IRPEF_2021 <- read_delim("Dataset/SAE/COV/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2021.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
check2019a <- classified_units%>%
  dplyr::select(c(LAU_NAME,`Codice Catastale del comune`))%>%
  anti_join(Redditi_IRPEF_2019,join_by(`Codice Catastale del comune`==`Codice catastale`))%>%
  dplyr::select(c(LAU_NAME,`Codice Catastale del comune`))
colnames(check2019a)=c("name","codcat")
check2019b <- Redditi_IRPEF_2019%>%
  dplyr::select(c(`Codice catastale`,`Denominazione Comune`))%>%
  anti_join(classified_units,join_by(`Codice catastale`==`Codice Catastale del comune`))%>%
  dplyr::select(c(`Denominazione Comune`,`Codice catastale`))
colnames(check2019b)=c("name","codcat")
check2019<-rbind(check2019a,check2019b) #this were the missing matches, 9 places only

check2020a <- classified_units%>%
  dplyr::select(c(LAU_NAME,`Codice Catastale del comune`))%>%
  anti_join(Redditi_IRPEF_2020,join_by(`Codice Catastale del comune`==`Codice catastale`))%>%
  dplyr::select(c(LAU_NAME,`Codice Catastale del comune`))
colnames(check2020a)=c("name","codcat")
check2020b <- Redditi_IRPEF_2020%>%
  dplyr::select(c(`Codice catastale`,`Denominazione Comune`))%>%
  anti_join(classified_units,join_by(`Codice catastale`==`Codice Catastale del comune`))%>%
  dplyr::select(c(`Denominazione Comune`,`Codice catastale`))
colnames(check2020b)=c("name","codcat")
check2020<-rbind(check2020a,check2020b) #this were the missing matches, 4 rows only

check2021a <- classified_units%>%
  dplyr::select(c(LAU_NAME,`Codice Catastale del comune`))%>%
  anti_join(Redditi_IRPEF_2021,join_by(`Codice Catastale del comune`==`Codice catastale`))%>%
  dplyr::select(c(LAU_NAME,`Codice Catastale del comune`))
colnames(check2021a)=c("name","codcat")
check2021b <- Redditi_IRPEF_2021%>%
  dplyr::select(c(`Codice catastale`,`Denominazione Comune`))%>%
  anti_join(classified_units,join_by(`Codice catastale`==`Codice Catastale del comune`))%>%
  dplyr::select(c(`Denominazione Comune`,`Codice catastale`))
colnames(check2021b)=c("name","codcat")
check2021<-rbind(check2021a,check2021b) #this were the missing matches, 5 rows only
#see the the relative files:

##estimation2019.R
##estimation2020.R
##estimation2021.R

##see file models.R 

# SAE ----

if (!require('sae', quietly = TRUE)) { install.packages('sae') } 
library('sae')
#### 19----
FH19 <- direst19
FH19[is.na(FH19)] <- 0
FH19 <- FH19%>%
  mutate(CV = sd / yi)
summary(FH19$CV*100)

mod_FH <- mseFH(formula   = FH19$yi ~ as.factor(FH19$DGURBA), 
                vardir    = FH19$Var,
                method    = "REML",
                MAXITER   = 1000,
                PRECISION = 10^(-5),
                B         = 0)

FH_CV <- 100 * sqrt(mod_FH$mse) / mod_FH$est$eblup
res_FH19<-data.frame(Publ_Area    = FH19$SmallArea,
           Sample_Size  = FH19$ni,
           Direct          = round(FH19$yi, digits = 2), 
           CV       = round(FH19$CV * 100, digits = 2), 
           FH_EBLUP     = round(mod_FH$est$eblup, digits = 2), 
           FH_CV        = round(FH_CV, digits = 2),
           TIME = 2019,
           DGURBA = FH19$DGURBA)

summary(res_FH19)

CV_compare <- as.data.frame(cbind(cut(res_FH19$CV ,c(-0.01,16.5,33.3,100)),cut(res_FH19$FH_CV,c(-0.01,16.5,33.3,100))))
CV_compare<-CV_compare%>%
  summarise(
    'Direct<16.5' = sum(V1==1) ,
    'Direct16.51-33.3' = sum(V1==2),
    'Direct>33.31'= sum(V1==3),
    'FH<16.5'= sum(V2==1) ,
    'FH16.51-33.3'= sum(V2==2),
    'FH>33.31'= sum(V2==3),
  )
#### 20----
FH20 <- direst20
FH20[is.na(FH20)] <- 0
FH20 <- FH20%>%
  mutate(CV = sd / yi)
summary(FH20$CV*100)

mod_FH_20 <- mseFH(formula   = FH20$yi ~ as.factor(FH20$DGURBA), 
                vardir    = FH20$Var,
                method    = "REML",
                MAXITER   = 1000,
                PRECISION = 10^(-5),
                B         = 0)

FH_CV_20 <- 100 * sqrt(mod_FH_20$mse) / mod_FH_20$est$eblup
res_FH20<-data.frame(Publ_Area    = FH20$SmallArea,
                     Sample_Size  = FH20$ni,
                     Direct          = round(FH20$yi, digits = 2), 
                     CV       = round(FH20$CV * 100, digits = 2), 
                     FH_EBLUP     = round(mod_FH_20$est$eblup, digits = 2), 
                     FH_CV        = round(FH_CV_20, digits = 2),
                     TIME = 2020,
                     DGURBA = FH20$DGURBA)

summary(res_FH20)

CV_compare20 <- as.data.frame(cbind(cut(res_FH20$CV ,c(-0.01,16.5,33.3,100)),cut(res_FH20$FH_CV,c(-0.01,16.5,33.3,100))))
CV_compare20[is.na(CV_compare20)] <- 0
CV_compare20<-CV_compare20%>%
  summarise(
    'Direct<16.5' = sum(V1==1) ,
    'Direct16.51-33.3' = sum(V1==2),
    'Direct>33.31'= sum(V1==3),
    'FH<16.5'= sum(V2==1) ,
    'FH16.51-33.3'= sum(V2==2),
    'FH>33.31'= sum(V2==3),
  )

#### 21----
FH21 <- direst21
FH21[is.na(FH21)] <- 0
FH21 <- FH21%>%
  mutate(CV = sd / yi)
summary(FH21$CV*100)

mod_FH_21 <- mseFH(formula   = FH21$yi ~ as.factor(FH21$DGURBA), 
                   vardir    = FH21$Var,
                   method    = "REML",
                   MAXITER   = 1000,
                   PRECISION = 10^(-5),
                   B         = 0)

FH_CV_21 <- 100 * sqrt(mod_FH_21$mse) / mod_FH_21$est$eblup
res_FH21<-data.frame(Publ_Area    = FH21$SmallArea,
                     Sample_Size  = FH21$ni,
                     Direct          = round(FH21$yi, digits = 2), 
                     CV       = round(FH21$CV * 100, digits = 2), 
                     FH_EBLUP     = round(mod_FH_21$est$eblup, digits = 2), 
                     FH_CV        = round(FH_CV_21, digits = 2),
                     TIME = 2021,
                     DGURBA = FH21$DGURBA)

summary(res_FH21)

CV_compare21 <- as.data.frame(cbind(cut(res_FH21$CV ,c(-0.01,16.5,33.3,100)),cut(res_FH21$FH_CV,c(-0.01,16.5,33.3,100))))
CV_compare21[is.na(CV_compare21)] <- 0
CV_compare21<-CV_compare21%>%
  summarise(
    'Direct<16.5' = sum(V1==1) ,
    'Direct16.51-33.3' = sum(V1==2),
    'Direct>33.31'= sum(V1==3),
    'FH<16.5'= sum(V2==1) ,
    'FH16.51-33.3'= sum(V2==2),
    'FH>33.31'= sum(V2==3),
  )
##### preparing results for the visualization ----
result <- smallareas%>% left_join(res_FH19, join_by(smarea == Publ_Area))
result <- rbind(result,(smallareas%>% left_join(res_FH20, join_by(smarea == Publ_Area))))
result <- rbind(result,(smallareas%>% left_join(res_FH21, join_by(smarea == Publ_Area))))
result <- result[,-4]%>% left_join(povrelbyregion19_21[,c(1,4,5)])
units_result19 <- classified_units%>%
  left_join(result%>%
              filter(TIME=="2019")%>%
              mutate(ISTAT_code = as.character(ISTAT_code)),
            join_by("reg_code"=="ISTAT_code","DGURBA"=="DGURBA"))
units_result20 <- classified_units%>%
  left_join(result%>%
              filter(TIME=="2020")%>%
              mutate(ISTAT_code = as.character(ISTAT_code)),
            join_by("reg_code"=="ISTAT_code","DGURBA"=="DGURBA"))
units_result21 <- classified_units%>%
  left_join(result%>%
              filter(TIME=="2021")%>%
              mutate(ISTAT_code = as.character(ISTAT_code)),
            join_by("reg_code"=="ISTAT_code","DGURBA"=="DGURBA"))
# VIZ ----
library(ggplot2)
library(viridis)
dev.off()
## DEGURBA ----
palette <- c(d1 = "#37123C", d2 = "#71677C", d3 = "#945D5E")
mun<-deg_viz%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill=DGURBA, color=DGURBA)) + 
  ggtitle("Italy by Degree of Urbanization - Municipality ") + 
  scale_fill_manual(values=c("#37123C","#71677C","#DDA77B"))+
  scale_color_manual(values=c("#37123C","#71677C","#DDA77B"))+ 
  coord_sf()
mun
prov<-deg_viz%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill=deg_prov, color=deg_prov)) + 
  ggtitle("Italy by Degree of Urbanization - Province")  + 
  scale_fill_manual(values=c("#37123C","#71677C","#DDA77B"))+
  scale_color_manual(values=c("#37123C","#71677C","#DDA77B"))+ 
  coord_sf()
prov
reg<-deg_viz%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill=deg_reg, color=deg_reg)) + 
  ggtitle("Italy by Degree of Urbanization - Region")  + 
  scale_fill_manual(values=c("#71677C","#DDA77B"))+
  scale_color_manual(values=c("#71677C","#DDA77B"))+ 
  coord_sf()
reg
area<-deg_viz%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill=deg_area, color=deg_area)) + 
  ggtitle("Italy by Degree of Urbanization - Area")  + 
  scale_fill_manual(values=c("#DDA77B"))+
  scale_color_manual(values=c("#DDA77B"))+ 
  coord_sf()
area
## SAE ----
saeviz <- rbind(units_result19,units_result20,units_result21)
SAE_map<-saeviz%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill=FH_EBLUP,color=FH_EBLUP)) + 
  ggtitle("Small-Area-Estimated Relative Poverty - Region x DEGURBA") + 
  coord_sf() + 
  scale_fill_viridis_c(option = "plasma")+
  scale_color_viridis_c(option = "plasma")+ 
  facet_grid(. ~ TIME)
SAE_map

saeviz<-saeviz%>%
  mutate(FH_nas = ifelse(FH_CV>33.3,NA,FH_EBLUP))
SAE_map_accurate<-saeviz%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill=FH_nas,color=FH_nas)) + 
  ggtitle("Small-Area-Estimated Relative Poverty - Region x DEGURBA\nAccounting for CV>33.3") + 
  coord_sf() + 
  scale_fill_viridis_c(option = "plasma")+
  scale_color_viridis_c(option = "plasma")+ 
  facet_grid(. ~ TIME)
SAE_map_accurate

SAE_map_direct<-saeviz%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill=Direct,color=Direct)) + 
  ggtitle("Relative Poverty - Direct estimates") + 
  coord_sf() + 
  scale_fill_viridis_c(option = "plasma")+
  scale_color_viridis_c(option = "plasma")+ 
  facet_grid(. ~ TIME)
SAE_map_direct

library(GGally)
SAE_val<- result%>%
  dplyr::select(c("TIME", "DGURBA", "FH_EBLUP","smarea","ITTER107"))%>%
  pivot_wider(names_from = TIME, 
                     values_from = FH_EBLUP)%>%
  mutate(ITTER107 = as.factor(ITTER107))
SAE_val_deg <-ggparcoord(SAE_val, columns = 4:6, groupColumn = 1, order = "anyClass",
             scale="globalminmax",
             showPoints = TRUE, 
             title = "Trend of Relative Poverty - by DEGURBA",
             alphaLines = 0.3
  ) + 
  scale_color_manual(values=c("#37123C","#71677C","#DDA77B"))+
  xlab("")
SAE_val_deg
#add national average in a contrasting colour - set useful colours

SAE_val_reg<- ggparcoord(SAE_val, columns = 4:6, groupColumn = 3, order = "anyClass",
                         scale="globalminmax",
                         showPoints = TRUE, 
                         title = "Trend of Relative Poverty - by DEGURBA",
                         alphaLines = 0.3
) + 
  xlab("") 
SAE_val_reg
  
## TARGETING ----
viz_cr_region <- rbind(viz_cr_region19,viz_cr_region20,viz_cr_region21)
CR_relativepov <- viz_cr_region%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill=cov_rate,color=cov_rate)) + 
  ggtitle("RdC/PdC Coverage Rate on Household Relative Poverty - Region level") + 
  coord_sf() + 
  scale_fill_continuous(type = "viridis")+
  scale_color_continuous(type = "viridis")+ 
  facet_grid(. ~ TIME)
CR_relativepov

regional_povrel <-targeting[,c(1,2,4,5)]%>%
  mutate(Value=ifelse(is.na(Value),4.2,Value))%>%
  left_join(harmonize_region_code)%>%
  right_join(viz_cr_region, join_by(ITTER107,TIME))
regional_povrel_map<-regional_povrel%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill=Value,color=Value)) + 
  ggtitle("Relative Poverty - Regional Incidences") + 
  coord_sf() + 
  scale_fill_viridis_c(option = "plasma")+
  scale_color_viridis_c(option = "plasma")+ 
  facet_grid(. ~ TIME)
regional_povrel_map

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

