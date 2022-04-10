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
chi_select <-chi_na.omit %>%
  select(LowBi_ight, Unemp_ment, Below_evel, NoHig_loma)

chi_cor <-cor(chi_select)
corrplot(chi_cor)


chi_plot <-ggplot(chi_na.omit, aes(x=Below_evel, y=LowBi_ight, color = comm_area))+geom_point()
print(chi_plot)
