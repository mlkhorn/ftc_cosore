# 06/19/2020-06/23/2020
# COSORE function to pull out datasets




# *Meeting with Steph 06/19/2020* ####

#way to create a blank character vector with length 88
#storage_vector<-character(88)

# find_ft_datasets <- function(T5_min, T5_max) {
# What is the goal of the function? 
## to loop through datasets and pull out only the ones with the desired 
## criteria

# Arguments
# T5_min: A numeric value for the minimum T5
# T5_max: A numeric value for the maximum T5

# Create empty list called `ft_datasets`
# Use `list_datasets()` as your looping variable  
# Loop through `CSR_DATASET` names
# Does dataset contain Rs values within `T5_min` and `T5_max`?
# If TRUE, add to list

# Return a vector of dataset names that cross the FT barrier

#}







# *Meeting w/ Ben & Steph 06/22-23/2020* ####

# STEP ONE   #############
# Objective ####
# To create a list named MDAY_stroragelist that contains dataframes 
# listing the number of days of data each COSORE dataset with CO2 efflux data
# contains for each month in the calendar year. Eventually these dataframes will 
# be processed to determine if the datasets they represent contain the requisite
# data for conducting a Winter Free Thaw Analysis, making them useful for 
# for Mercedes's SULI 2020 project. 

# Set up variables ####
# full_database contains a summary of the whole COSORE database 
full_database <- csr_database()
# dataset_names contains a list of every dataset name in COSORE
dataset_names <- full_database$CSR_DATASET  
# MDAY_storagelist will eventually contain dataframes 
# listing the number of days of data each COSORE dataset with CO2 efflux data
# contains for each month in the calendar year
MDAY_storagelist <- list()
# # MDAY_datasetwdata will reference a a vector of all the datasets that had 
# data in their data table in COSORE and had CO2 efflux data that was not null
MDAY_datasetwdata<-c()

# ~Mercedes' For loop~ ####
# this for loop is used to populate the MDAY_storagelist with the requisite
# dataframes and MDAY_datasetwdata with the requisite dataset names
# should I use message() instead of print?
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
  MDAY_datasetwdata[x] <- dat$description$CSR_DATASET
  print(x)
}

# Pseudo Loop 1 ####
# there was data in this dataset, this pseudo loop was used for debugging
# dat <- csr_dataset("d20200423_SANCHEZ-CANETE")
# Rs <- dat$data 
# Rs %>%
#  group_by(month(CSR_TIMESTAMP_BEGIN)) %>% 
#  summarise(n = length(unique(day(CSR_TIMESTAMP_BEGIN)))) ->
#  mday_storage1

# Pseudo Loop 2 ####
# no data in this dataset, this pseudo loop was used for debugging
# dat <- csr_dataset("d20190526_MINIONS_BNZ")
# Rs <- dat$data 
# Rs %>%
#  group_by(month(CSR_TIMESTAMP_BEGIN)) %>% 
#  summarise(n = length(unique(day(CSR_TIMESTAMP_BEGIN)))) ->
#  mday_storage2
                                   




# STEP TWO        #############   
# Objective ####
# When you have all datasets summarized in this way, you can ask, 
# how many have all 12 months with days >=15?

# Set up variables ####
# full_database contains a summary of the whole COSORE database generated using 
# the csr_database() function
head(full_database) 
# dataset_names contains a vector with every dataset name in COSORE
head(dataset_names)
# MDAY_storagelist now contains dataframes 
# listing the number of days of data each COSORE dataset with CO2 efflux data
# contains for each month in the calendar year
head(MDAY_storagelist) 
# MDAY_datasetwdata is now references a a vector of all the datasets that had 
# data in there data table in COSORE and had CO2 efflux data that was not null
head(MDAY_datasetwdata)
# winterdataset_names is a vector that will store the names of all the datasets
# that contain at least 12 months of data with at least 15 days of data per month 
winterdataset_names <- c()
# winter_data is a list that will eventually store all the "data" data from the 
# all the datasets in the winterdataset_names vector 
winter_data <- list()


# ~Mercedes' For loop~ ####
i=1
for(ds in MDAY_datasetwdata) 
{
  #print the dataset name that is currently looping
  print(ds)
  #storing the month day dataframe of one dataset in the Try dataframe 
  Try <- MDAY_storagelist[[ds]]
  #determining the length of the month vector in the Try dataframe
  numMonths <- length(Try$`month(CSR_TIMESTAMP_BEGIN)`)
  #if this length is not 12, then this dataset does not contain a full year of
  #data and should therefore not be included in winterdataset_names, so if this
  #is true, the for loop quits
  if( numMonths != 12) next
  #before we check the Try dataframe we know of zero months in it that have at
  #least 15 days of data
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
  #datasets in COSORE that have the requisite data for the proposed contribution 
  #of winter Rs to annual Rs analysis
  winterdataset_names[i]<-ds
  #after this is done i is increased by one so the next dataset name for a dataset 
  #with requisite data does not write over anything 
  i=i+1
  
}
#examine the results
print(winterdataset_names)

# Pseudo Loop 1 ####
#ds <- "d20190409_ANJILELI"
#Try <- MDAY_storagelist[[ds]]
#numMonths <- length(Try$`month(CSR_TIMESTAMP_BEGIN)`)
#(numMonths != 12)
#gooddataMonths <- 0
#for(m in 1:12)
#{
#  monthdays<-Try$n
#  if(monthdays[m]<= 15) next
#  gooddataMonths = gooddataMonths + 1
#}
#winterdataset_names[[i]]<-ds
#i=i+1



# STEP THREE        #############   
# Objective ####
# Let's store all the "data" from datasets we are interested in a new list
# lets call this list winter_data, because I am tired and I will think of a more 
# descriptive name later

# Set up variables ####

# full_database contains a summary of the whole COSORE database generated using 
# the csr_database() function
head(full_database) 

# dataset_names contains a vector with every dataset name in COSORE
head(dataset_names) 

# MDAY_storagelist is a list now containing dataframes 
# listing the number of days of data each COSORE dataset with CO2 flux data
# contains for each month in the calendar year
head(MDAY_storagelist) 

# MDAY_datasetwdata is now references a a vector of all the datasets that had 
# data in there data table in COSORE and had CO2 efflux data that was not null
head(MDAY_datasetwdata)

# winterdataset_names is a vector that now stores the names of all the datasets
# that contain at least 12 months of data with at least 15 days of data per month 
head(winterdataset_names)

# winter_data is a list that will eventually store all the "data" data from the 
# all the datasets in the winterdataset_names vector 
winter_data <- list()

# ~Mercedes' For loop~ ####
j=1
for (dname in winterdataset_names)
{
  print(dname)
  inchworm<-csr_dataset(dname)
  inch<-inchworm$data
  winter_data[[dname]]<-inch
  j=j+1
}
#examine the results
print(j)
head(winter_data)

# Pseudo Loop 1 ####
#print(dname)
#inchworm<-csr_dataset(dname)
#inch<-inchworm$data
#winter_data[[dname]]<-inch
#j=j+1

# ~Ben's For loop~ does step 2&3 ####
for(ds in MDAY_datasetwdata) 
{
  message("Loading ", ds)
  Dat <- csr_dataset(ds)$data
  Dat$month <- month(Dat$CSR_TIMESTAMP_BEGIN)
  Dat %>%
    group_by(month) %>% 
    summarise(unique_days = length(unique(day(CSR_TIMESTAMP_BEGIN)))) ->
    winter_data[[ds]]
}
Overall_result <- dplyr::bind_rows(winter_data, .id = "dataset")

# Pseudo Loop 1 ####  

Dat <- csr_dataset("d20190409_ANJILELI")$data
Dat <- filter(Dat, !is.na(Dat$CSR_FLUX_CO2))
Dat$month <- month(Dat$CSR_TIMESTAMP_BEGIN)
Dat %>%
  group_by(month) %>% 
  summarise(unique_days = length(unique(day(CSR_TIMESTAMP_BEGIN)))) ->
  winter_data[[ds]]


# *Meeting w/ Steph 06/24/2020* ####
#which of these datasets is cold? 
#which of these datasets have Rh data and what is its coverage? 





