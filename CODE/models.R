library(tidymodels)
#2019
modelling_ <- modelling_estimates19
#install.packages("corrplot")
library(corrplot)
M<-cor(modelling_[,c(31:54,59)]) 
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(modelling_[,c(31:54,59)])
corrplot(M,type="upper", tl.col="black", tl.srt=45,p.mat = p.mat, sig.level = 0.5)
relevant <- cbind(var=rownames(M),coeff=M[,25],p=p.mat[,25])
relevant <- as.data.frame(relevant)%>%
  mutate(coeff = as.numeric(coeff),
         p = as.numeric(p))%>%
  filter(p<0.05)
#relvars <- cbind(relvars,relevant)
direst <- cbind(modelling_[,relevant$var],modelling_estimates19[,1:30])
direst["SmallArea"]<-paste(direst$Regione,direst$DGURBA,sep="_")
rcp <- recipe(Value ~ avglavdip+ avgret +  avgpiva +
                avg15k   + avg26k   + avg55k   +
                avg75k   + `avg120k`  + `avg120+k` +
                `%lavdip` + `%10k`  + `%26k`   +
                `%55k`    + `%75k` +  `%120k`  +
                `%120+k`, data = modelling_)#maybe needs updating
rcp %>% prep() %>% juice() %>% glimpse()
treer <- decision_tree(      
  mode = "regression",
  cost_complexity = 0.01,
  tree_depth = 15,
  min_n = 2
) %>% set_engine('rpart')
set.seed(42)
wrkfl_fit_treer <- workflow() %>%
  add_model(treer) %>%
  add_recipe(rcp) %>%
  fit(modelling_)
wrkfl_fit_treer %>%
  extract_fit_engine() %>%
  rpart.plot::rpart.plot(type=2,tweak = 1.35,roundint = FALSE)
wrkfl_fit_treer %>% 
  extract_fit_parsnip() %>% 
  vip::vip(10)
yi <-predict(wrkfl_fit_treer, 
             new_data = modelling_) 
modelling_ <- modelling_ %>% 
  bind_cols(predict(wrkfl_fit_treer, new_data = modelling_)) 
cor.test(modelling_$.pred, modelling_$Value) #corr=0.9898

library(sf)
direst19<-units_rd_19%>%
  bind_cols(predict(wrkfl_fit_treer, new_data = units_rd_19))

direst19<-direst19%>%    
  group_by(Regione,DGURBA,)%>%
  summarise(yi = weighted.mean(`.pred`,taxpayers),
            ni = n(),
            sd = sd( `.pred`, na.rm = TRUE),
            Var = sd^2)%>%
  mutate("SmallArea"= paste(Regione,DGURBA,sep="_"))%>%
  left_join(modelling_estimates19)

#2020
modelling_<-modelling_estimates20
M<-cor(modelling_[,c(31:54,59)]) 
p.mat <- cor.mtest(modelling_[,c(31:54,59)])
corrplot(M,type="upper", tl.col="black", tl.srt=45,p.mat = p.mat, sig.level = 0.5)
relevant <- cbind(var=rownames(M),coeff=M[,25],p=p.mat[,25])
relevant <- as.data.frame(relevant)%>%
  mutate(coeff = as.numeric(coeff),
         p = as.numeric(p))%>%
  filter(p<0.05)
relvars <- cbind(relvars,relevant)
direst <- cbind(modelling_[,relevant$var],modelling_estimates20[,1:30])
direst["SmallArea"]<-paste(direst$Regione,direst$DGURBA,sep="_")

rcp <- recipe(Value ~ avglavdip+ avgret +  avgpiva + avg0less + 
                avg15k   + avg26k   + avg55k   +
                avg75k   + `avg120k`  + `avg120+k` +
                `%lavdip` + `%10k`  + `%26k`   +
                `%55k`    + `%75k` +  `%120k`  +
                `%120+k`, data = modelling_)
rcp %>% prep() %>% juice() %>% glimpse()
treer <- decision_tree(      
  mode = "regression",
  cost_complexity = 0.01,
  tree_depth = 15,
  min_n = 2
) %>% set_engine('rpart')
set.seed(42)
wrkfl_fit_treer <- workflow() %>%
  add_model(treer) %>%
  add_recipe(rcp) %>%
  fit(modelling_)
wrkfl_fit_treer %>%
  extract_fit_engine() %>%
  rpart.plot::rpart.plot(type=2,tweak = 1.35,roundint = FALSE)
wrkfl_fit_treer %>% 
  extract_fit_parsnip() %>% 
  vip::vip(10)
yi <-predict(wrkfl_fit_treer, 
             new_data = modelling_) 
modelling_ <- modelling_ %>% 
  bind_cols(predict(wrkfl_fit_treer, new_data = modelling_)) 
cor.test(modelling_$.pred, modelling_$Value) #corr=0.983366


direst20<-units_rd_20%>%
  bind_cols(predict(wrkfl_fit_treer, new_data = units_rd_20))
direst20<-direst20%>%    
  group_by(Regione,DGURBA)%>%
  summarise(yi = weighted.mean(`.pred`,taxpayers),
            ni = n(),
            sd = sd( `.pred`, na.rm = TRUE),
            Var = sd^2)%>%
  mutate("SmallArea"= paste(Regione,DGURBA,sep="_"))%>%
  left_join(modelling_estimates20)


#2021

modelling_ <- modelling_estimates21
M<-cor(modelling_[,c(31:54,59)]) 
p.mat <- cor.mtest(modelling_[,c(31:54,59)])
corrplot(M,type="upper", tl.col="black", tl.srt=45,p.mat = p.mat, sig.level = 0.5)
relevant <- cbind(var=rownames(M),coeff=M[,25],p=p.mat[,25])
relevant <- as.data.frame(relevant)%>%
  mutate(coeff = as.numeric(coeff),
         p = as.numeric(p))%>%
  filter(p<0.05)
relvars <- cbind(relvars,relevant)
direst <- cbind(modelling_[,relevant$var],modelling_estimates21[,1:30])
direst["SmallArea"]<-paste(direst$Regione,direst$DGURBA,sep="_")

rcp <- recipe(Value ~ avglavdip+ avgret +  avgpiva + 
                avg15k   + avg26k   + avg55k   +
                avg75k   + `avg120k`  + `avg120+k` +
                `%lavdip` + `%10k`  + `%26k`   +
                `%55k`    + `%75k` +  `%120k`  +
                `%120+k`, data = modelling_)
rcp %>% prep() %>% juice() %>% glimpse()
treer <- decision_tree(      
  mode = "regression",
  cost_complexity = 0.01,
  tree_depth = 15,
  min_n = 2
) %>% set_engine('rpart')
set.seed(42)
wrkfl_fit_treer <- workflow() %>%
  add_model(treer) %>%
  add_recipe(rcp) %>%
  fit(modelling_)
wrkfl_fit_treer %>%
  extract_fit_engine() %>%
  rpart.plot::rpart.plot(type=2,tweak = 1.35,roundint = FALSE)
wrkfl_fit_treer %>% 
  extract_fit_parsnip() %>% 
  vip::vip(10)
yi <-predict(wrkfl_fit_treer, 
             new_data = modelling_) 
modelling_ <- modelling_ %>% 
  bind_cols(predict(wrkfl_fit_treer, new_data = modelling_)) 
cor.test(modelling_$.pred, modelling_$Value) #corr=0.9739351 

direst21<-units_rd_21%>%
  bind_cols(predict(wrkfl_fit_treer, new_data = units_rd_21))
direst21<-direst21%>%    
  group_by(Regione,DGURBA)%>%
  summarise(yi = weighted.mean(`.pred`,taxpayers),
            ni = n(),
            sd = sd( `.pred`, na.rm = TRUE),
            Var = sd^2)%>%
  mutate("SmallArea"= paste(Regione,DGURBA,sep="_"))%>%
  left_join(modelling_estimates21)

cor.test(direst19$yi, direst19$Value) #corr=0.9172
cor.test(direst20$yi, direst20$Value) #corr=0.8225
cor.test(direst21$yi, direst21$Value) #corr=0.9137 
