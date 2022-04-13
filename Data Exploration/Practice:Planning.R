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
chi_h_ind.sf <-st_read('./healthIndicators/HealthIndicators.shp')
mapview(chi_h_ind.sf)
ggplot(data=chi_h_ind.sf)+geom_sf()
head(chi_h_ind.sf, n=10)
chi_h_ind.df <- as.data.frame(chi_h_ind.sf)
#explore

chi_na.omit <-chi_h_ind.df %>%
  na.omit()
chi_AIC <-lm(data=chi_na.omit, LowBi_ight ~ Unemp_ment+Below_evel+NoHig_loma)
summary(chi_AIC)
step(chi_AIC)

library(corrplot)
chi_select_1 <-chi_na.omit %>%
  select(LowBi_ight, Unemp_ment, Below_evel, NoHig_loma, comm_area)

chi_select_2 <-chi_na.omit %>%
  select(LowBi_ight, Unemp_ment, Below_evel, NoHig_loma)


chi_cor <-cor(chi_select_2)
corrplot(chi_cor)
c


chi_plot <-ggplot(chi_na.omit, aes(x=Below_evel, y=LowBi_ight, color = comm_area))+geom_point()
print(chi_plot)


#make a heat/density map of low birth weight prevalence across chicago community areas 

ggplot()+geom_sf(data=chi_h_ind.sf, aes(fill= LowBi_ight))+ scale_fill_continuous(name="Low Birth Weight Prevalence")+
  ggtitle("Low Birth Weight Prevalence in Chicago Community Areas", subtitle = "Aman Hingu")



summary(chi_h_ind.df)
#no na's in dataset

#make single linear regressions with each of the variables

chi_select_lm1 <-lm(data = chi_select_1, LowBi_ight ~ Unemp_ment)
chi_select_lm1
summary(chi_select_lm1)

par(mfrow = c(2,2), mar=c(4,4,4,4))
plot(chi_select_lm1)
par(mfrow = c(1,1))

lm1_plot <-ggplot(chi_select_1, aes(x=Unemp_ment, y=LowBi_ight))+geom_point()
print(lm1_plot)

chi_select_lm2 <-lm(data=chi_select_1, LowBi_ight ~ Below_evel)
chi_select_lm2
summary(chi_select_lm2)

par(mfrow = c(2,2), mar=c(4,4,4,4))
plot(chi_select_lm2)
par(mfrow = c(1,1))

lm1_plot <-ggplot(chi_select_2, aes(x=Below_evel, y=LowBi_ight))+geom_point()
print(lm1_plot)

chi_select_lm3 <-lm(data=chi_select_1, LowBi_ight ~ NoHig_loma)
chi_select_lm3
summary(chi_select_lm3)

par(mfrow = c(2,2), mar=c(4,4,4,4))
plot(chi_select_lm3)
par(mfrow = c(1,1))

lm1_plot <-ggplot(chi_select_3, aes(x=NoHig_loma, y=LowBi_ight))+geom_point()
print(lm1_plot)

#do a multiple linear regression

chi_select_lm4 <-lm(data=chi_select_1, LowBi_ight ~ Unemp_ment + Below_evel + NoHig_loma)
summary(chi_select_lm4)


par(mfrow = c(2,2), mar=c(4,4,4,4))
plot(chi_select_lm4)
par(mfrow = c(1,1))

chi_select_lm5 <-lm(data=chi_select_1, LowBi_ight ~ Unemp_ment + Below_evel)
summary(chi_select_lm5)
par(mfrow = c(2,2), mar=c(4,4,4,4))
plot(chi_select_lm4)
par(mfrow = c(1,1))

