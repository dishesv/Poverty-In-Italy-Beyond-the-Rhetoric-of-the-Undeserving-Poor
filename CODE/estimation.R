#####ESTIMATING POVERTY######
#creation of 59 estimates to be taken as direct estimates in the SAE
modelling_<-modelling_estimates[,-c(44:67,82)]
modelling<-modelling_estimates20[,-c(2,3,42:45,47,48)]
#install.packages("corrplot")
library(corrplot)
#M<-cor(modelling_[,c(31:54,58)])
M<-cor(modelling:[,c()])
#the main interest is actually the correlation with Value
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
# matrix of the p-value of the correlation
p.mat <- cor.mtest(modelling_[,c(31:54,58)])
corrplot(M,type="upper", tl.col="black", tl.srt=45,p.mat = p.mat, sig.level = 0.5)
relevant = cbind(var=rownames(M),coeff=M[,25],p=p.mat[,25])
relevant <- as.data.frame(relevant)%>%
  mutate(coeff = as.numeric(coeff),
         p = as.numeric(p))%>%
  filter(p<0.05)

direst <- cbind(modelling_[,relevant$var],modelling_estimates[,1:30])
direst["SmallArea"]<-paste(direst$Regione,direst$DGURBA,sep="_")

library(rpart)

fit <- rpart(Value ~ avglavdip+ avgret +  avgpiva +
               avg15k   + avg26k   + avg55k   +
               avg75k   + `avg120k`  + `avg120+k` +
               `%lavdip.w` + `%10k.w`  + `%26k.w`   +
               `%55k.w`    + `%75k.w` +  `%120k.w`  +
               `%120+k.w`,
             method = "anova", data = modelling_estimates)
print(fit)
importance <- data.frame(imp = fit$variable.importance)
importance <- importance %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp)%>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
library(ggplot2)
ggplot(importance) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()

modelling_$yi <- predict(fit, modelling_, method = "anova")

#####I was trying to make it fancy#####
rcp <- recipe(Value ~ avglavdip+ avgret +  avgpiva +
                 avg15k   + avg26k   + avg55k   +
                 avg75k   + `avg120k`  + `avg120+k` +
                 `%lavdip.w` + `%10k.w`  + `%26k.w`   +
                 `%55k.w`    + `%75k.w` +  `%120k.w`  +
                 `%120+k.w`, data = modelling_)
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



#check quality of estimates#
cor.test(modelling_$.pred, modelling_$Value) #corr=0.9898
library(sf)
colnames(Red_19_gbregionanddegurba_units)[c(51,55:61)]<-c('%lavdip.w','%10k.w','%15k.w','%26k.w','%55k.w','%75k.w','%120k.w','%120+k.w')
direst<-Red_19_gbregionanddegurba_units%>%
  bind_cols(predict(wrkfl_fit_treer, new_data = Red_19_gbregionanddegurba_units))
  

direst<-direst%>%    
  group_by(Regione,DGURBA, `Anno di imposta`)%>%
  summarise(yi = weighted.mean(`.pred`,taxpayers),
            ni = n(),
            sd = sd( `.pred`, na.rm = TRUE),
            Var = sd^2)%>%
  mutate("SmallArea"= paste(Regione,DGURBA,sep="_"))%>%
  left_join(modelling_estimates)

cor.test(direst$yi, direst$Value) #corr=0.9172
