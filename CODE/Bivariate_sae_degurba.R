#########################################################################

rm(list=ls())

setwd("C:/Users/Giovanni/Dropbox/Dati Tesi Tonutti/Dati")

#install.packages("car")
#install.packages("lme4")
#install.packages("xlxs")
#install.packages("maptools")
##install.packages("spdep")

library(sampling)   # survey sampling
library(survey)
library(sae)
#library(car)
library(lme4)
library(maptools)
library(spdep)
library(dplyr)
library(expm)
library(stats)
hbs <- read.csv("spese2017_rid_edit.csv")
degurba<-read.csv("degurba_comuni.csv")
#hbs_2015 <- read.csv("spese2015.csv")
#hbs_2016 <- read.csv("spese2016_rid_edit.csv")
hbs$prov_1000<-(hbs$Cod_provincia_d)*1000
hbs$codice_comune<-hbs$prov_1000+hbs$Cod_comune_d
table(hbs$codice_comune)
hbs<-merge(hbs, degurba, by.x="codice_comune", by.y="codicecomune")
hbs$regione_degurba<-hbs$Codice.Regione*100+hbs$Grado.di.urbanizzazione


table(hbs$regione_degurba)
#59 degurba-regions area

##The covariates too have 110 provinces 
##however, for variables of occupation and education info was on the 107 provinces - so i have imputed the same values across all 4 of the sardinia ones 
#load("new_cov.Rda")# <- read.csv("Covariate_province_110.csv")
tabulate(hbs$povassc)
#i order the data frame by province
hbs <- hbs[order(hbs$regione_degurba,hbs$Cod_comune_d),]
cov<-read.csv("covariates_degurba_reg.csv")
cov <- cov[order(cov$degurba_regione),]
#creating a label variable region#degurba
cov$reg_abv<-substr(cov$regione,1,3)
cov$REGDEG<-paste(cov$reg_abv,cov$degurba, sep="_")
##somma dei pesi di famiglie in poverta':1.75m
sum(hbs$w_anno[hbs$povassc==1])

##ricalibro i pesi delle provincie 
ni<-table(hbs$regione_degurba)
#colnames(new_cov)<-sub("freq_pop", "prop_pop", colnames(new_cov))
#cov_107<-new_cov[!(new_cov$Codice.territorio==2 | new_cov$Codice.territorio==103 | new_cov$Codice.territorio==105),]
#hbs_104<-hbs[!(hbs$Cod_provincia_d==104 | hbs$Cod_provincia_d==106 | hbs$Cod_provincia_d==107),]
#cov_104<-cov_107[!(cov_107$Codice_prov==104 | cov_107$Codice_prov==106 | cov_107$Codice_prov==107),]

#length(cov_107)

#there are 107 provinces out of 110
Ni.err<-as.numeric(tapply(hbs$w_anno,hbs$regione_degurba,sum))
hbs$coeff.new<-hbs$w_anno*rep(cov$numerodifamiglie/Ni.err,ni) #SERVE IL NUMERO DI FAMIGLIE PER PROVINCIA PER CORREGGERE I PESI
Ni.new<-as.numeric(tapply(hbs$coeff.new,hbs$regione_degurba,sum))

##I generate the direct estimates
hcr.dir_deg<-direct(y=(hbs$povassc),dom=hbs$regione_degurba,sweight=hbs$coeff.new,domsize=data.frame(unique(hbs$regione_degurba),Ni.new))
cv.dir.cat<- cut(hcr.dir_deg$CV, c(0,16.5,33.3,100)) #labels("<16.5%" "16.51% - 33.30%" ">33.31%"))
table (cv.dir.cat, useNA = "always")

write.csv(hcr.dir_deg, "directs_degurba.csv")

hcr.dir_deg$REGDEG<-cov$REGDEG

hcr.dir_deg$Var<-hcr.dir_deg$SD^2

variables<-as.vector(c( "degurba_regione","redditofabbricatofrequenza", "redditofabbricato", "redditodapensionefrequenza", "redditolavorodipendente", "redditolavoroautonomo", "redditoautonomoamount", "redditoimprendorfreq", "redditoimprendammord", "redditoimprendsempfreq", "redditoimprendammsemp", "numerocontribuenti", "redditoimponibile", "redditoimponibilefrequenza", "reddito0a1000", "reddito10000a15000", "reddito120000", "reddito15000a26000", "reddito26000a55000", "reddito55000a75000","reddito75000a120000"))  
XXX=cov[,c(variables)]
names(XXX)[1]<-"Domain"
combined_data_FH_abspov=combine_data(pop_data=XXX, pop_domains = "Domain",
                                smp_data = hcr.dir_deg, smp_domains="Domain")
#####################################################################
#################################BI VARIATE FH##########################
#####################################################################

arope_analysis<-load("AROPE_analysis.Rdata")
write.csv(targeting_df_arope,"targeting_df_arope.csv")

#install.packages('msae')
library(msae)
## Load dataset
#combined_data_FH_arcsin<-combined_data_FH_arcsin[order(combined_data_FH_arcsin$Domain),]
bivariate_fh_data<-merge(combined_data_FH_abspov,combined_data_FH, by="Domain")

#redditolavorodipendente+numerocontribuenti+redditoimponibile+reddito0a1000+reddito15000a26000+reddito26000a55000
#numerocontribuenti+redditofabbricato+redditoimprendsempfreq+redditoimponibilefrequenza+reddito15000a26000

variables_1<-as.vector(c("Domain", "Direct.x", "Direct.y", "SD.x", "SD.y", "Var.x", "Var.y", "redditolavorodipendente.x", "numerocontribuenti.x", "redditoimponibile.x","reddito0a1000.x","reddito15000a26000.x","reddito26000a55000.x", "redditofabbricato.x","redditoimprendsempfreq.x","redditoimponibilefrequenza.x"))  
bivariate_fh_data<-bivariate_fh_data[,c(variables_1)]
bivariate_fh_data$Var.xy<-rep(0,59)
# Compute EBLUP and MSE of Y1 Y2 and Y3 based on Model 1
# using auxiliary variables X1 and X2 for each dependent variable
## Using parameter 'data'
Fo <- list(f1=Direct.y~numerocontribuenti.x+redditofabbricato.x+redditoimprendsempfreq.x+redditoimponibilefrequenza.x+reddito15000a26000.x,
           f2=Direct.x~+redditofabbricato.x+reddito15000a26000.x)
vardir <- c("Var.y", "Var.x", "Var.xy")
m1 <- eblupMFH1(Fo, vardir, data=bivariate_fh_data)
## Without parameter 'data'
#Fo <- list(f1=datasae1$Y1~datasae1$X1+datasae1$X2,f2=datasae1$Y2~datasae1$X1+datasae1$X2)
#vardir <- datasae1[,c("v1","v2","v12")]
#m1 <- eblupMFH1(Fo, vardir)
m1$eblup # see the EBLUP estimators
m1$MSE # see MSE of EBLUP estimators

bivariate_fh_data$CV_FH_arope<-(sqrt(m1$MSE$Direct.y)/m1$eblup$Direct.y)*100
bivariate_fh_data$CV_FH_abspov<-(sqrt(m1$MSE$Direct.x)/m1$eblup$Direct.x)*100

cv.biv_fh_arop.cat<- cut(bivariate_fh_data$CV_FH_arope, c(0,16.5,33.3,100,1000))
table (cv.biv_fh_arop.cat, useNA = "always")

cv.biv_fh_abs.cat<- cut(bivariate_fh_data$CV_FH_abspov, c(0,16.5,33.3,100,1000))
table (cv.biv_fh_abs.cat, useNA = "always")
table (cv.fh_arcsin.cat, useNA = "always")
table (cv.fh.cat, useNA = "always")

###Targeting 
hcr.dir_deg$HCR_BIV<-m1$eblup$Direct.x
hcr.dir_deg$HCR_BIV_CV<-bivariate_fh_data$CV_FH_abspov
cor(hcr.dir_deg$Direct,hcr.dir_deg$HCR_BIV)
cor(hcr.dir_deg$Direct,hcr.dir_deg$HCR_FH)
cor(hcr.dir_deg$Direct,hcr.dir_deg$HCR_FH_arcsin)
cor(combined_data_FH$Direct,m1$eblup$Direct.y)
