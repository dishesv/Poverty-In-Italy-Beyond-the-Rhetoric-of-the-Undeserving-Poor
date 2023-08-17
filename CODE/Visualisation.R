#DATAVIZ#
##packages in packages.R
setwd("~/UNI/THESIS/Poverty-In-Italy-Beyond-the-Rhetoric-of-the-Undeserving-Poor")
#####Data Loading#####
povabslines21 <- read_delim("Dataset/Povertà/povabslines21.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

sogliepovrel16_21 <- read_csv("Dataset/Povertà/sogliepovrel16_21.csv", 
                              col_types = cols(`Numero componenti della famiglia` = col_number()))
povbyhouseholdtype16_21 <- read_csv("Dataset/Povertà/povbyhouseholdtype16_21.csv")
povbyhouseholdtype16_21 <- povbyhouseholdtype16_21%>% mutate_if(is.character, as.factor)

households <- c('totale famiglie',
                'persona sola 35-64 anni','persona sola 65 anni o più',
                'famiglie di soli italiani','famiglie di soli stranieri',
                'almeno un anziano',
                'monogenitore', 
                '1 figlio minore','almeno un figlio minore')

pbht_ass<-povbyhouseholdtype16_21%>%
  filter(TIPO_DATO8=='INCID_POVASS_FAM')%>%
  select(c(TIME, `Tipologia familiare`,ITTER107,Value))%>% 
  filter(`Tipologia familiare` %in% households)

pbhtasschart <- povbyhouseholdtype16_21%>%
  filter(TIPO_DATO8=='INCID_POVASS_FAM')%>%
  select(c(TIME, `Tipologia familiare`,ITTER107,Value))%>% 
  filter(`Tipologia familiare` %in% households)%>% 
  mutate(ITTER107 = as.character(ITTER107))%>%
  mutate(ITTER107 = replace(ITTER107, ITTER107 == "ITCD", 'N'))%>%
  mutate(ITTER107 = replace(ITTER107, ITTER107 == "ITE", 'C'))%>%
  mutate(ITTER107 = replace(ITTER107, ITTER107 == "ITFG", 'S'))


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
  


#####VIZ####
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
  aes(x=TIME, y=Value, group=TIP_FAM, color =TIP_FAM))+
  geom_line()+
  geom_text(
    vjust = 0, nudge_y = 0.15,
    aes(label = Value),
    parse = TRUE
  )+
  facet_grid(TIPO_DATO8 ~.)
gfamnationality


#####A FIRST COMPLEX CHART#####

plt <- ggplot(pbhtasschart) +
  # Make custom panel grid
 geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:3) * 1000),
    color = "lightgrey"
  ) + 

  geom_bar(
    aes(
      x = `Tipologia familiare`, 
      y = Value
      #fill=
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) 
  
  # Add dots to represent the mean gain
  geom_point(
    aes(
      x = reorder(str_wrap(region, 5),sum_length),
      y = mean_gain
    ),
    size = 3,
    color = "gray12"
  ) +
  
  # Lollipop shaft for mean gain per region
  geom_segment(
    aes(
      x = reorder(str_wrap(region, 5), sum_length),
      y = 0,
      xend = reorder(str_wrap(region, 5), sum_length),
      yend = 3000
    ),
    linetype = "dashed"
  ) + 
  
  # Make it circular!
  coord_polar()

plt

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
           show.legend = FALSE,position=position_dodge(5))+
  facet_wrap(ITTER107~ .)+
  geom_col(data= layer2,
           aes(x= reorder(str_wrap(`Tipologia familiare`, 7), Value), y=Value),
           linetype="dashed",
           size=0.75,
           color="#375F53", 
           fill="transparent",
           position=position_dodge(5))+
  geom_point((data= avglayer1),
            mapping=aes(x= reorder(str_wrap(`Tipologia familiare`, 7), Value),y=Value,size = 0.1),
            color="#000000",show.legend = FALSE)+
  geom_point((data= avglayer2),
            mapping=aes(x= reorder(str_wrap(`Tipologia familiare`, 7), Value), y=Value,
            size = 0.1,alpha=0.35),show.legend = FALSE) + 
  scale_y_continuous(
    limits = c(-20, 45),
    expand = c(0, 0),
    breaks = c(0, 5,10,15,20,25,30,35,40,45))+
  scale_x_discrete(breaks = waiver())+
  coord_polar()
crt  
  
crt+ annotate(
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
  )
