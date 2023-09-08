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

households <- read_csv("Dataset/Povertà/householdsbyprovince19_21.csv", 
                       col_types = cols(TIPO_DATO15 = col_skip(), 
                                        `Tipo di indicatore demografico` = col_skip(), 
                                        SEXISTAT1 = col_skip(), Sesso = col_skip(), 
                                        `Seleziona periodo` = col_skip(), 
                                        `Flag Codes` = col_skip(), Flags = col_skip()))

households_reg <- households%>%
  group_by(ITTER107, TIME)%>%
  summarise(
    households_tot = sum(Value)
  )

povrelbyregion19_21 <- povrelbyregion19_21%>%
  inner_join(households_reg)%>%
  mutate(rel_poor_hous = households_tot*Value)

by = join_by("Territorio"=="regione", "TIME"=="Anno")
targeting <- left_join(povrelbyregion19_21, recipients19_23,by )
targeting<-targeting%>%
  mutate(cov_rate = round(households/households_tot,3)*100)

households_regional_coverage_rate_povrel <- targeting%>%
  select(Territorio, TIME, cov_rate)


#######VIZ######
mapping <- read_delim("C:\\Users\\vitto\\Documents\\UNI\\THESIS\\Poverty-In-Italy-Beyond-the-Rhetoric-of-the-Undeserving-Poor\\DataViz\\mapping.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

viz_cr_region <- households_regional_coverage_rate_povrel%>%
  filter(TIME=="2019")%>%
  right_join(mapping, join_by("Territorio"=="Regione"))
library(ggplot2)
#not working
viz_cr_region%>%
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill=cov_rate, color=cov_rate)) + 
  ggtitle("RdC/PdC Coverage Rate on Household Relative Poverty - Region level") + 
  coord_sf()
