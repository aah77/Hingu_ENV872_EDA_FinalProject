#Import the tidyverse library 
library(tidyverse, quietly = TRUE)
library(lubridate)

#install.packages('sf')
library(sf)
#install.packages('leaflet')
library(leaflet)
#install.packages('mapview')
library(mapview)

#Disable on-the-fly projections
sf::sf_use_s2(FALSE)


getwd()
setwd("/Users/amanhingu/Documents/Hingu_ENV872_EDA_FinalProject")

chi_h_ind.sf <-st_read('./Raw Data/healthIndicators_Raw/HealthIndicators.shp')
mapview(chi_h_ind.sf)
ggplot(data=chi_h_ind.sf)+geom_sf()
head(chi_h_ind.sf, n=10)
dim(chi_h_ind.df)
summary(chi_h_ind.sf)
chi_h_ind.df <- as.data.frame(chi_h_ind.sf)


#make a heat/density map of low birth weight prevalence across chicago community areas

ggplot()+geom_sf(data=chi_h_ind.sf, aes(fill= LowBi_ight))+ scale_fill_continuous(name="Low Birth Weight Prevalence")+
  ggtitle("Low Birth Weight Prevalence in Chicago Community Areas", subtitle = "Aman Hingu")

#make heat/spatial maps of other independent variables. 

ggplot()+geom_sf(data=chi_h_ind.sf, aes(fill= Unemp_ment))+ scale_fill_continuous(name="Unemployment Rate")+
  ggtitle("Unemployment Rates in Chicago Community Areas", subtitle = "Aman Hingu")

ggplot()+geom_sf(data=chi_h_ind.sf, aes(fill= Below_evel))+ scale_fill_continuous(name="Proportion Under Poverty Level")+
  ggtitle("Portion under Poverty Level in Chicago Community Areas", subtitle = "Aman Hingu")

ggplot()+geom_sf(data=chi_h_ind.sf, aes(fill= NoHig_loma))+ scale_fill_continuous(name="Educational Attainment")+
  ggtitle("Educational Attainment in Chicago Community Areas", subtitle = "Aman Hingu")


#use corrplot and AIC to further justify running LM's

library(corrplot)
chi_select_1 <-chi_na.omit %>%
  select(LowBi_ight, Unemp_ment, Below_evel, NoHig_loma, comm_area)

chi_select_2 <-chi_na.omit %>%
  select(LowBi_ight, Unemp_ment, Below_evel, NoHig_loma)

write.csv(chi_na.omit, file = "./Processed Data Folder/chi_na.omit.csv", row.names=FALSE)
write.csv(chi_select_2, file = "./Processed Data Folder/chi_select_2.csv", row.names=FALSE)

chi_cor <-cor(chi_select_2)
corrplot(chi_cor)

chi_AIC <-lm(data=chi_select_2, LowBi_ight ~ Unemp_ment+Below_evel+NoHig_loma)
summary(chi_AIC)
step(chi_AIC)


#scatter plots of each independent variable

lm1_plot <-ggplot(chi_select_1, aes(x=Unemp_ment, y=LowBi_ight))+geom_point()+
  labs(title = "Low Birth Weight Prevalence vs. Unemployment", x ='Unemployment', 
       y ='Low Birth Weight Prevalence') +geom_smooth(method = "lm", color ="blue")+ah__theme
print(lm1_plot)

lm2_plot <-ggplot(chi_select_1, aes(x=Unemp_ment, y=Below_evel))+geom_point()+
  labs(title = "Low Birth Weight Prevalence vs. Portion under Poverty Level", x ='Portion under Poverty Level', 
       y ='Low Birth Weight Prevalence') +geom_smooth(method = "lm", color ="blue")+ah__theme
print(lm1_plot)

lm3_plot <-ggplot(chi_select_1, aes(x=Unemp_ment, y=NoHig_loma))+geom_point()+
  labs(title = "Low Birth Weight Prevalence vs. Portion under Poverty Level", x ='Educational Attainment', 
       y ='Low Birth Weight Prevalence') +geom_smooth(method = "lm", color ="blue")+ah__theme
print(lm1_plot)


#run multivariable linear regression models
 #first model includes unemployment and portion under poverty level based off recommendations from the corrplot

chi_select_lm5 <-lm(data=chi_select_1, LowBi_ight ~ Unemp_ment + Below_evel)
summary(chi_select_lm5)
par(mfrow = c(2,2), mar=c(4,4,4,4))
plot(chi_select_lm4)
par(mfrow = c(1,1))


 #second model includes all three variables in the interest of increasing model fit while maintaining significance

chi_select_lm4 <-lm(data=chi_select_1, LowBi_ight ~ Unemp_ment + Below_evel + NoHig_loma)
summary(chi_select_lm4)
par(mfrow = c(2,2), mar=c(4,4,4,4))
plot(chi_select_lm4)
par(mfrow = c(1,1))






