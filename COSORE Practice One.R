# 06/05/2020
# COSORE practice day 1&2


library(dplyr)
library(tidyr)
library(ggplot2)

#install.packages("devtools")
#install.packages("remotes")
#devtools::install_github("bpbond/cosore")

library(devtools)
library(remotes)
library(cosore)
#why didn't "library(cosore)" work initially? my console returned 
#`Type citation("cosore") for the main COSORE database reference.`
#why did I have to go into packages on the left and click the box next 
#to cosore to get it to work? 

#list- a generic vector containing other objects
#dataframes-
#  *a list of vectors used for storing vectors of equal length
#  *used for storing datatables

## EXPLORING THE COSORE DATABASE
db_info <- csr_database()
tibble::glimpse(db_info)
print(db_info,n=100)

## EXPLORING A SINGLE DATASET
#picking a single dataset, getting some info about it, and plotting it

varner <- csr_dataset("d20190415_VARNER")
tibble::glimpse(varner$description)
names(varner)
glimpse(varner$description)
glimpse(varner$contributors)
varner$contributors 
#the description table gives basic info about the dataset:
#-where it was measured
#-the time zone the timestamps are in
#-the instruments used
#-citation information

#questions about what cosore fields contain, you can use 
#csr_metadata() to return a datatable with information about this

#EXPLORING THE DATASET ITSELF
sr<- varner$data
metadata <- csr_metadata()
View(metadata)
#this stores the varner data in the sr list or dataframe, (I am not sure)
##?? see above^
nrow(sr)
#the nrow function tells us how many rows sr has  
#>the data has 34,642 observations
summary(sr)  
#the summary function returns the min,1quartile, med, 3quartile, and max
#of each variable column
#> the data extends from May 20th 2003 to Dec 12th 2006
#> soil respiration ranged from 0.01 to 31.03
##ME?? what's the unit of soil respiration used?
#>soil temperature ranged from -7.49 degrees to 7 degrees
#>air temperature ranged from -17.08 degrees to 47 degrees
##ME? what is the unit? 
#>its pretty certain to be degrees celcius, but how can you confirm this? 


#VISUALIZING THE DATA
theme_set(theme_minimal())
##ME?? what does this do?
##ME?? what are ggplot themes? other ggplot themes?

ggplot(sr, aes(CSR_TIMESTAMP_BEGIN, CSR_FLUX_CO2, color = CSR_PORT)) +
  geom_point(size = 0.5, alpha = 0.25) +
  coord_cartesian(ylim = c(0, 20))

tibble::glimpse(varner$ports)

sr

db_info

db_info$CSR_DATASET[1]

anj<-csr_dataset("d20190409_ANJILELI")  

names(anj)
glimpse(anj)
anj$contributors
glimpse(anj$ports)

anj$data
tail(anj$data)

ggplot(anj$data, aes(CSR_SM5, CSR_FLUX_CO2, color=CSR_T5))+ 
  geom_point(alpha=0.2, size=1) + xlim(0.1,0.4)

dbf_datasets <- subset(db_info, CSR_IGBP == "Deciduous broadleaf forest")$CSR_DATASET
tdf <- csr_table("description", dbf_datasets)

library(sp)
library(leaflet)
map <- data.frame(lon = tdf$CSR_LONGITUDE, lat = tdf$CSR_LATITUDE)
coordinates(map) <- ~lon + lat
leaflet(map) %>% 
  addMarkers() %>% 
  addTiles()

tdf_dat <- csr_table("data", dbf_datasets, quiet = TRUE)
summary(tdf_dat)
ggplot(tdf_dat, aes(CSR_TIMESTAMP_BEGIN, CSR_FLUX_CO2, color = CSR_DATASET)) + 
  geom_point(size = 0.5, alpha = 0.25) + 
  scale_color_discrete(guide = FALSE) +
  coord_cartesian(ylim = c(0, 20))

ggplot(tdf_dat, aes(CSR_T5), CSR_FLUX_CO2))+
  geom_point()+facet_wrap()

