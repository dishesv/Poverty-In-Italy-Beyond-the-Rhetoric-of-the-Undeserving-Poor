#CREATION OF THE DEGURBA CLASSIFICATION FILE
install.packages("sf")
install.packages("dplyr")
install.packages("magrittr")

library(sf)
library(dplyr)
library(magrittr)
shapefile <- st_read("~/UNI/THESIS/Dataset/DGURBA-2020-01M-SH/DGURBA-2020-01M-SH.shp")
degurba_units <- shapefile %>%
  filter(CNTR_CODE == "IT") %>%
  as_tibble()
