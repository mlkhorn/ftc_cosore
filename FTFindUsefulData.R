# 06/19/2020-06/26/2020
# Mercedes Horn with help from Stephanie Pennington and Ben Bond-Lamberty
# Script used to pull out useful datasets

# Packages  ##############
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(devtools)
library(remotes)
library(cosore)



# STEP ONE   #############

# Objective ####
# To create a list named MDAY_storagelist that contains dataframes 
# listing the number of days of data each COSORE dataset with CO2 flux data
# contains for each month in the calendar year. Eventually these dataframes will 
# be processed to determine if the datasets they summarize have 12 months of 
# data with at least 15 days of data each month 

# Set up variables ####
# full_database contains a summary of the whole COSORE database 
full_database <- csr_database()
# dataset_names contains a list of every dataset name in COSORE
dataset_names <- full_database$CSR_DATASET  
# MDAY_storagelist will eventually contain dataframes 
# listing the number of days of data each COSORE dataset with CO2 efflux data
# contains for each month in the calendar year
MDAY_storagelist <- list()
# CO2dataset_names will reference a vector of all the datasets that had 
# data in their data table in COSORE and had CO2 flux data that was not null
CO2dataset_names<-c()

# ~For loop~ ####
# this for loop is used to populate the MDAY_storagelist with the requisite
# dataframes and CO2dataset_names with the requisite dataset names
for(x in dataset_names) 
  {
  dat <- csr_dataset(x)
  if(is.null(dat$data)) next
  if(is.null(dat$data$CSR_FLUX_CO2)) next
  Rs <- dat$data 
  Rs %>%
    group_by(month(CSR_TIMESTAMP_BEGIN)) %>% 
    summarise(n = length(unique(day(CSR_TIMESTAMP_BEGIN)))) ->
    MDAY_storage
  MDAY_storagelist[[x]] <- MDAY_storage
  CO2dataset_names[x] <- dat$description$CSR_DATASET
  print(x)
}



# STEP TWO        #############   

# Objective ####
# To create a vector of dataset names that contains the name of each
# COSORE dataset that contains 12 months of data with at least 15 days of 
# data per month

# Set up variables ####
# allyeardata_names is a vector that will store the names of all the datasets
# that contain at least 12 months of data with at least 15 days of data per month 
allyeardata_names <- c()
# allyear_data is a list that will eventually store all the "data" data from the 
# all the datasets in the allyeardata_names vector 
allyear_data <- list()


# ~For loop~ ####
ayI=1
for(ds in CO2dataset_names) 
{
  #print the dataset name that is currently looping
  print(ds)
  #store the month day dataframe of one dataset in the Try dataframe 
  Try <- MDAY_storagelist[[ds]]
  #determine the length of the month vector in the Try dataframe
  numMonths <- length(Try$`month(CSR_TIMESTAMP_BEGIN)`)
  #if this length is not 12, then this dataset does not contain a full year of
  #data and should therefore not be included in allyeardata_names, so if 
  #this condition is true, the for loop quits
  if( numMonths != 12) next
  gooddataMonths <- 0
  #scan through the the Try dataframe to count the number of months that have at
  #least 15 days of data
  for(m in 1:12)
   {
    monthdays<-Try$n
    if(monthdays[m]<= 15) next
    gooddataMonths = gooddataMonths + 1
   }
  #if this number of months is not twelve then the dataset is thrown out
  if ((gooddataMonths) != 12) next
  #if the dataset does have 12 months of data with at least 15 days of data in 
  #each month, then it is added to a vector that lists the names of all the 
  #datasets in COSORE that have the requisite data 
  allyeardata_names[ayI]<-ds
  #after this is done i is increased by one so the next dataset name for a dataset 
  #with requisite data does not write over anything 
  ayI=ayI+1
  
}
#examine the results
print(allyeardata_names)



# STEP THREE        #############   
# Objective ####
# To store all the "data" from the allyeardata_names datasets in a list called
# allyear_data

# Set up variables ####
# allyear_data is a list that will eventually store all the "data" data from the 
# all the datasets in the allyeardata_names vector 
allyear_data <- list()

# ~For loop~ ####
for (dname in allyeardata_names)
{
  print(dname)
  inchworm<-csr_dataset(dname)
  inch<-inchworm$data
  allyear_data[[dname]]<-inch
}
#examine the results
head(allyear_data)



# STEP FOUR  ############

# Objective ####
# To determine how many datasets in allyeardata_names have 5 cm soil temperature
# and air temperature data

#Length of allyeardata_names
runs<-as.integer(length(allyeardata_names))


# Air Temp ########

# Which data sets contain air temperature data and which do not?

airay_names<-c()
#airay_names datasets will contain air temperature data
noairay_names<-c()
#noairay_names datasets will not contain air temperature data
datset<-list()
datsetcol<-c()
airI<-1
noairI<-1

for(i in 1:runs) {
  message("I'm on dataset ", allyeardata_names[i]," now")
  datset<-allyear_data[[i]]
  datsetcol<-colnames(datset)
  if(!("CSR_TAIR" %in% datsetcol)){
    noairay_names[noairI]<-allyeardata_names[i]
    noairI<-noairI+1
  }
  if("CSR_TAIR" %in% datsetcol){
    airay_names[airI]<-allyeardata_names[i]
    airI<-airI+1
  }
}

# Air Temp Conclusions #####

# 6 datasets contain air temperature data
airay_names
# 25 datasets do not contain air temperature data
noairay_names


# Soil Temp ####

# Which data sets contain T5 soil temperature data and which do not?

t5ay_names<-c()
#t5_names datasets will contain t5 soil temperature data
not5ay_names<-c()
#not5ay_names datasets will not contain t5 soil temperature data
datset<-list()
datsetcol<-c()
t5I<-1
not5I<-1

for(i in 1:runs) {
  message("I'm on dataset ", allyeardata_names[i]," now")
  datset<-allyear_data[[i]]
  datsetcol<-colnames(datset)
  if(!("CSR_T5" %in% datsetcol)){
    not5ay_names[not5I]<-allyeardata_names[i]
    not5I<-not5I+1
  }
  if("CSR_T5" %in% datsetcol){
    t5ay_names[t5I]<-allyeardata_names[i]
    t5I<-t5I+1
  }
}

# Soil Temp Conclusions ####

#22 datasets contain t5 soil temperature data
t5ay_names
#9 datasets do not contain t5 soil temperature data
not5ay_names


# Air & Soil Temp ####

#Are any datasets missing both air temperature and t5 soil temperature?

bothtempay_names<-c()
#will contain names of datasets with air and soil 5 cm temperature
tempay_names<-c()
#will contain names of datasets that have either air or soil 5 cm temperature
notempay_names<-c()
#will contain names of the datasets that neither air or soil 5 cm temperature
datset<-list()
datsetcol<-c()
tempI<-1
notempI<-1
bothtempI<-1

for(i in 1:runs){
    message("I'm on dataset ", allyeardata_names[i]," now")
    datset<-allyear_data[[i]]
    datsetcol<-colnames(datset)
    
    if((!("CSR_T5" %in% datsetcol)) & (!("CSR_TAIR" %in% datsetcol))){
      notempay_names[notempI]<-allyeardata_names[i]
      notempI<- notempI+1
    }
    else if(("CSR_T5" %in% datsetcol) & ("CSR_TAIR" %in% datsetcol)){
      bothtempay_names[bothtempI]<-allyeardata_names[i]
      bothtempI<- bothtempI+1
    }
    else if(("CSR_T5" %in% datsetcol)|("CSR_TAIR" %in% datsetcol)){
      tempay_names[tempI] <-allyeardata_names[i]
      tempI<-tempI+1
    } 
}

# Air & Soil Temp Conclusions ####

# 6 datasets are missing both air and soil t5 temperature data
notempay_names
# 22 datasets have at least air or t5 temperature data
tempay_names
# 0 datasets have both air and t5 soil temperature data
bothtemp_names



# STEP 5 ###########

# Objective ####
# To determine which datasets in allyeardata_names contain RH data.

RHdata_names<-c()
#will contain the names of datasets with Rh data
RHI<-1

for(i in 1:runs){
  message("I'm on dataset ", allyeardata_names[i]," now")
  datset<-csr_table(table = "ports",datasets = allyeardata_names[i])  
  if("Rh" %in% datset$CSR_MSMT_VAR) 
  {
    RHdata_names[RHI]<- allyeardata_names[i]
    RHI<-RHI+1  
    }
}

# Rh Conclusions ####

# 4 datasets contain Rh data
RHdata_names



# STEP 6 ########

# Objective ####
# To determine which datasets in allyeardata_names contain "winter" data.

winterdata_names<-c()  
# will hold the names of the all year datasets with T5 soil temp data that have
# a minimum T5 less than or equal to 0 oC
winterI<-1

for(i in 1:runs){
  message("I'm on dataset ", allyeardata_names[i]," now")
  datset<-allyear_data[[i]]  
  datsetcol<-colnames(datset)
  
  if(!("CSR_T5" %in% datsetcol)) next
  
  if(min(datset$CSR_T5, na.rm = TRUE) <= 0) {
    winterdata_names[winterI] <- allyeardata_names[i]
    winterI<-winterI+1
  }
  
}

# Conclusions ####
# 8 datasets in allyeardata_names have data that match our definition of winter
winterdata_names
print(winterdata_names)






