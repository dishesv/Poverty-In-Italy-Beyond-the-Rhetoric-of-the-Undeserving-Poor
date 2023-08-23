#DATAVIZ#
##packages in packages.R
setwd("~/UNI/THESIS/Poverty-In-Italy-Beyond-the-Rhetoric-of-the-Undeserving-Poor")
#####Data Loading#####
povabslines21 <- read_delim("Dataset/Povertà/povabslines21.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

sogliepovrel16_21 <- read_csv("Dataset/Povertà/sogliepovrel16_21.csv", 
                              col_types = cols(NUMEROSITA = col_skip(), 
                                               `Numero componenti della famiglia` = col_integer(), 
                                               `Seleziona periodo` = col_skip(), 
                                               `Flag Codes` = col_skip(), Flags = col_skip()))

povrellines21<-sogliepovrel16_21%>%filter(TIME=="2021")

pop2021 <- read_csv("Dataset/Povertà/population2021.csv", 
                           col_types = cols(TIPO_DATO15 = col_skip(), 
                                            SEXISTAT1 = col_skip(), 
                                            `Seleziona periodo` = col_skip(),
                                            TIME = col_integer()))
pop2021<- pop2021%>%
  filter( `Tipo di indicatore demografico`!="popolazione inizio periodo")%>%
  select(-c("Flags","Flag Codes"))%>%
  filter(TIME==2021)

p21<- pop2021%>%
  filter(Sesso=='totale')%>%
  select(-(Sesso))%>%
  pivot_wider(names_from = `Tipo di indicatore demografico`, values_from = Value)
  
rednetbyregion <- read_csv("Dataset/Povertà/redditonettobyregion.csv", 
                                 col_types = cols(T_D8 = col_factor(levels = c("REDD_MEDIO_FAM", 
                                                                               "REDD_MEDIANO_FAM")), `Presenza affitti imputati` = col_skip(), 
                                                  RDPR = col_skip(), `Fonte principale di reddito familiare` = col_skip(), 
                                                  `Seleziona periodo` = col_skip(), 
                                                  `Flag Codes` = col_skip(), Flags = col_skip()))

rnet21<-rednetbyregion%>%
  filter(TIME=="2021" & !(Territorio%in%c('Sud','Centro')) )%>%
  select(-c(PRAF,`Tipo dato`))%>%
  pivot_wider(names_from = T_D8, values_from = Value)

popbyred <- rnet21%>%
  inner_join(p21)

povbyhouseholdtype16_21 <- read_csv("Dataset/Povertà/povbyhouseholdtype16_21.csv")
povbyhouseholdtype16_21 <- povbyhouseholdtype16_21%>% mutate_if(is.character, as.factor)

households <- c('totale famiglie',
                'persona sola 35-64 anni','persona sola 65 anni o più',
                'famiglie di soli italiani','famiglie di soli stranieri',
                'almeno un anziano',
                'monogenitore', 
                '1 figlio minore','almeno un figlio minore')

eng_households <-c("1 person household 65+","Household with elderly", "Italians-only household",
                   "1 person household 35-64", "All households", "Household with 1 minor",
                   "Single- parent", "Household with minors", "Foreigners-only household")

pbht_ass<-povbyhouseholdtype16_21%>%
  filter(TIPO_DATO8=='INCID_POVASS_FAM')%>%
  select(c(TIME, `Tipologia familiare`,ITTER107,Value))%>% 
  filter(`Tipologia familiare` %in% households)

pbhtasschart <- povbyhouseholdtype16_21%>%
  filter(TIPO_DATO8=='INCID_POVASS_FAM')%>%
  select(c(TIME, `Tipologia familiare`,ITTER107,Value))%>% 
  filter(`Tipologia familiare` %in% households)%>% 
  mutate(ITTER107 = as.character(ITTER107))%>%
  mutate(ITTER107 = replace(ITTER107, ITTER107 == "ITCD", 'North'))%>%
  mutate(ITTER107 = replace(ITTER107, ITTER107 == "ITE", 'Centre'))%>%
  mutate(ITTER107 = replace(ITTER107, ITTER107 == "ITFG", 'South and Islands'))

pbht_ass<- pbht_ass%>% 
  mutate(ITTER107 = as.character(ITTER107))%>%
  mutate(ITTER107 = replace(ITTER107, ITTER107 == "ITCD", 'N'))%>%
  mutate(ITTER107 = replace(ITTER107, ITTER107 == "ITE", 'C'))%>%
  mutate(ITTER107 = replace(ITTER107, ITTER107 == "ITFG", 'S'))%>%
  pivot_wider(
  names_from = (`Tipologia familiare`),
  values_from = Value
)
pbht_ass1721 <- pbht_ass%>% filter(TIME %in% c('2021','2017'))%>%
  mutate(ITTER107 = as.factor(ITTER107))

pbht_rel<-povbyhouseholdtype16_21%>%
  filter(TIPO_DATO8=='INCID_POVREL_FAM')%>%
  select(c(TIME, `Tipologia familiare`,ITTER107,Value))%>% 
  filter(`Tipologia familiare` %in% households)
  
rdcpdc <- read_delim("Dataset/RDC/rdc_pdc_19_23.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                            grouping_mark = "."), trim_ws = TRUE)
rdc_19_23<-rdcpdc[,c(1:5)]

Romanos <- data.frame(views = c("None of these","Functionalistic","Social","Culturalistic","Fatalistic"),
                      "2007" = c(10,15,26,28,21),
                      "2009" = c(6,16,35,29,14),
                      "2010" = c(5,20,35,26,14))
Romanos <- Romanos %>% pivot_longer(cols=c(X2007, X2009, X2010))



#####VIZ####

#themecolours = c("#7D9B9F","#3A1625","#D09C32","#495751","#EDEEEB")
themecolours = c("#495751","#3A1625","#D09C32")

totbyarea <- povbyhouseholdtype16_21%>%
  filter(TIP_FAM=='HH'& ITTER107!='IT')
gtotbyarea = ggplot(totbyarea, aes(x=TIME, y=TIPO_DATO8, size = Value, color=Territorio)) +
  geom_text(
    vjust = 0, hjust=0,
    aes(label = Value, size=3),
    check_overlap = TRUE,
    parse = TRUE
  )+
  geom_jitter(width = 0.05, height = 0.05)
gtotbyarea

gtotbyarea_bar = ggplot(totbyarea, aes(x=TIME, y= Value, fill = factor(Territorio, levels=c("Nord", "Centro", "Mezzogiorno")))) +
  geom_bar(stat="identity",position=position_fill())+
  facet_wrap(~ TIPO_DATO8)+
  labs(fill='Area')
gtotbyarea_bar


gtot = ggplot(data=(povbyhouseholdtype16_21%>% filter(TIP_FAM=='HH'& ITTER107=='IT')), aes(x=TIME, y=Value, color=TIPO_DATO8))+
  geom_line() +
  geom_point()+
  scale_y_continuous(breaks=seq(0, 20, 1))+
  geom_text(
    vjust = 0, nudge_y = 0.15,
    aes(label = Value),
    parse = TRUE
  )
gtot

gfamnationality = ggplot(data=(povbyhouseholdtype16_21%>% filter((TIP_FAM=='WH_FR_HH'|TIP_FAM=='NAT_FR_HH'|TIP_FAM=='ALL_NNAT')& ITTER107=='IT')), 
  aes(x=order(TIME,), y=Value, group=TIP_FAM, color =TIP_FAM))+
  geom_line()+
  geom_text(
    vjust = 0, nudge_y = 0.15,
    aes(label = Value),
    parse = TRUE
  )+
  facet_grid(TIPO_DATO8 ~.)
gfamnationality


######ABSOLUTE POVERTY BY FAMILY TYPE - DIFFERENCIATED BY AREA (N-C-S) - Change 2021/2017####  
layer1 <- pbhtasschart%>%filter(ITTER107!='IT')%>%filter(TIME ==2021)
layer2 <- pbhtasschart%>%filter(ITTER107!='IT')%>%filter(TIME ==2017)
#imputation missing values ([C]p.sola 35-64 anni(2017)->2016; [C]p.sola 65+(2017)->2018; [C]monogenitore(2017)->2020)
layer2$Value[11]<-4.7
layer2$Value[12]<-4.0
layer2$Value[13]<-10.0
avglayer1 <- pbhtasschart%>%filter(ITTER107=='IT')%>%filter(TIME ==2021)%>%select(-ITTER107)
avglayer2 <- pbhtasschart%>%filter(ITTER107=='IT')%>%filter(TIME ==2017)%>%select(-ITTER107)

crt = ggplot()+
  theme_minimal_grid(font_size = 8)+
  geom_col(data= layer1,
           aes(x= reorder(str_wrap(`Tipologia familiare`, 7), Value), y=Value, fill=ITTER107),
           show.legend = FALSE,position=position_dodge(5), alpha=.8)+
  scale_fill_manual(values=themecolours)+
  facet_wrap(ITTER107~ .)+
  geom_col(data= layer2,
           aes(x= reorder(str_wrap(`Tipologia familiare`, 7), Value), y=Value),
           linetype="dashed",
           linewidth =0.75,
           color="black", 
           fill="transparent",
           position=position_dodge(5))+
  geom_point((data= avglayer1),
            mapping=aes(x= reorder(str_wrap(`Tipologia familiare`, 7), Value),y=Value),
            shape=21, fill="#7D9B9F",color="black", size=1.7,show.legend = FALSE)+
  geom_point((data= avglayer2),
            mapping=aes(x= reorder(str_wrap(`Tipologia familiare`, 7), Value), y=Value),
            shape=23, fill="#7D9B9F", color="white",size=0.9,show.legend = FALSE) + 
  scale_y_continuous(
    limits = c(-20, 45),
    expand = c(0, 0),
    breaks = c(0, 5,10,15,20,25,30,35,40,45))+
  scale_x_discrete(breaks = waiver(),labels=str_wrap(eng_households, 7))+
  coord_polar()
crt  
  
crt<-crt+ annotate(
  x = 0.25, 
  y = 5, 
  label = "   5", 
  geom = "text", 
  color = "gray12",
  size = 2,
  check_overlap = TRUE
) +
  annotate(
    x = 0.25, 
    y = 10, 
    label = "   10", 
    geom = "text", 
    color = "gray12",
    size = 2 ,
    check_overlap = TRUE
  ) +
  annotate(
    x = 0.25, 
    y =15, 
    label = "   15", 
    geom = "text", 
    color = "gray12",
    size = 2 ,
    check_overlap = TRUE
  ) +
  annotate(
    x = 0.25, 
    y =20, 
    label = "   20", 
    geom = "text", 
    color = "gray12",
    size = 2,
    check_overlap = TRUE
  ) +
  annotate(
    x = 0.25, 
    y =25, 
    label = "   25", 
    geom = "text", 
    color = "gray12",
    size = 2,
    check_overlap = TRUE 
  ) +
  annotate(
    x = 0.25, 
    y =30, 
    label = "   30", 
    geom = "text", 
    color = "gray12",
    size = 2,
    check_overlap = TRUE) +
  annotate(
    x = 0.25, 
    y =35, 
    label = "   35", 
    geom = "text", 
    color = "gray12",
    size = 2 ,
    check_overlap = TRUE
  ) +
  annotate(
    x = 0.25, 
    y =40, 
    label = "   40", 
    geom = "text", 
    color = "gray12",
    size = 2,
    check_overlap = TRUE
  )+
  labs(
   title = "\nAbsolute Poverty Incidence by Family Type and Area - 2021")+
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank())
crt

######Poverty Lines######
hs=c("1componente18-59",                  
     "1comp.60-74",                       
     "1comp.75+" ,                        
      "1comp.11-17_1comp.18-59" ,          
      "1comp.4-10_1comp.18-59",            
      "1comp.11-17_2comp.18-59",           
      "1comp.4-10_2comp.18-59",            
      "1comp.0-3_2comp.18-59" ,            
      "1comp.4-10_1comp.11-17_2comp.18-59",
      "1comp.0-3_1comp.4-10_2comp.18-59"  ,           
      "1comp.4-10_2comp.11-17_2comp.18-59",
      "2comp.11-17_3comp.18-59"           )

povlines21<-povrellines21%>%filter(LINEE_POVERTA=='ALL' & is.na(`Numero componenti della famiglia`)=='FALSE' )%>%select(c("Numero componenti della famiglia","Value"))
povlines21<-cbind(povlines21,povlines21$Value,povlines21$Value,povlines21$Value,povlines21$Value,povlines21$Value,povlines21$Value,povlines21$Value,povlines21$Value)
colnames(povlines21)<-colnames(povabslines21[1:10])
povlines21<-rbind(povlines21,
                  povabslines21%>%filter(household_size %in% hs)%>%select(-c("familysize","minor_bin")))

gpovlines<-GGally::ggparcoord(povlines21,
           columns = 2:10,
           showPoints = TRUE, 
           groupColumn = 1,
           title = "Absolute and relative poverty lines",
           alphaLines = 0.3,
           scale="globalminmax"
) 


######RdC/PdC recipients#####
grdcpdc <- rdc_19_23 %>% filter(Area!="Italia")%>%
  ggplot(aes(x=TIME, y=nuclei, fill=factor(Area,levels=c("Nord", "Centro","Sud e Isole"))))+
  geom_col()+
  geom_text(aes(label=avg),
             position=position_stack(
               0.5
             ),
            check_overlap = T)
  #scale_y_discrete(breaks=waiver(),n.breaks = 10)
grdcpdc

######Romano's analysis######
gromanos <- Romanos %>%
  ggplot(aes(x=name,y=value, fill=reorder(views,value)))+
  geom_col()
gromanos

######Population and Income#####
gpopinc <- popbyred%>% 
  ggplot() +
  geom_segment( aes(x=REDD_MEDIANO_FAM, xend=REDD_MEDIO_FAM, y=reorder(Territorio,`numero di famiglie al 31 dicembre`), yend=reorder(Territorio,`numero di famiglie al 31 dicembre`)), color="grey") +
  geom_point( aes(x=REDD_MEDIANO_FAM, y=reorder(Territorio,`numero di famiglie al 31 dicembre`)), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=REDD_MEDIO_FAM, y=reorder(Territorio,`numero di famiglie al 31 dicembre`)), color=rgb(0.7,0.2,0.1,0.5), size=3 ) 
gpopinc
popoverview <-popbyred%>%filter(Territorio!="Italia")%>%
  ggplot()+
  geom_point(aes(x=`numero di famiglie al 31 dicembre`,y=reorder(Territorio,`numero di famiglie al 31 dicembre`)))
popoverview
