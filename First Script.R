# 06/16/2020
# COSORE further exploration




library(devtools)
library(remotes)
library(cosore)

#stores metadata of cosore in metadb
metadb<-csr_database()

#stores names from metadata in metanames
metanames<-metadb$CSR_DATASET




## EXPLORING FIRST DATASET #######

#extracts full zhangMaple dataset from cosore
zmaple<-csr_dataset("d20190424_ZHANG_maple")


## Description #####

#description of zhangMaple dataset
zmaple$description
#>the description table of each dataset gives you information about
#>the dataset, where it was measured, latitude, longitude, elevation
#>what instrument was used to measure it, what time zone was used, 
#>the format of the timestamp, citation information

#glimpse of the description table of the zmaple dataset
glimpse(zmaple$description)


## Contributors #####

#contributors of zhangMaple dataset
#>the contributors list holds information about who helped collect
#>the data, their first names, last names, emails, the orcid id of 
#>the publication, and the role of each person


## Ports #####

#ports of the zhangMaple dataset
#>specifics of methodology of how the data was measured
#>what is being measured, Rs, Rh, Reco-ecosystem respiration, or NEE net ecosystem exchange
#>if there was a treatment on the soil under this port 
#>area and volume of the msmt chamber, depth of collar insertion
#>whether plants were removed, the chamber was opaque, or chamber had a mixing fan
#>comma separated list of vegetation 
#>lat, long, elev, of each port


## Columns #####

#columns of the zhangMaple dataset
#>database
#>dataset
#>computation
#>notes


## Ancillary #####

#ancillary of the zhangMaple dataset
#>MAT 
#>MAP
#>Statistic ##??what is this? 
#>generally this includes data about the site
#>NEP, NEE, soil type, soil pH, total soil nitrogen
#>sand silt lay ratio etc

## Data #####

#data of the zhangMaple dataset
#>holds actual flux msmts 
#>and accompanying time stamped data
#>port number
#>when the timestamp begins
#>when the timestamp ends
#>R2 of the CO2 ##?? what is this?
#>soil moisture at 30 cm ##??double check this
#>soil temperature at 5 cm  

## Diagnostics #####

# diagnostics of the zhangMaple dataset
#>records
#>recordsRemovedNa
#>recordsRemovedErr
#>recordsRemovedTimestamp
#>exampleBadTimeStamps
#>timestampBegin
#>timestampEnd
#>gases, which gases are measured



## EXPLORING FIRST W/ CODE #######
names(zmaple)
zmRs<-zmaple$data
glimpse(zmRs)
summary(zmRs$CSR_T5)
#the minimum temperature5 is -1.787, so this 
#dataset is probably one we would want, 
#but what about air temperature, is that somewhere? 

zmRs %>%
ggplot(aes(x = CSR_TIMESTAMP_BEGIN, y = CSR_FLUX_CO2)) +
         geom_point(color="blue")
zmRs %>%
  ggplot(aes(x = CSR_TIMESTAMP_BEGIN, y = CSR_T5)) +
  geom_point(color="blue")



## EXPLORING SECOND DATASET #######
metanames

## EXPLORING SECOND W/ CODE #######
anj<-csr_dataset("d20190409_ANJILELI")
anjrs<-anj$data
names(anjrs)

anjrs %>%
  ggplot(aes(x = CSR_TIMESTAMP_BEGIN, y = CSR_T5)) +
  geom_point(color="blue")

anjrs %>%
  ggplot(aes(x = CSR_TIMESTAMP_BEGIN, y = CSR_TAIR)) +
  geom_point(color="blue")

anj<-csr_dataset("d20190409_ANJILELI")
anjrs<-anj$data
names(anjrs)



## EXPLORING THIRD DATASET #######
metanames

## EXPLORING THIRD W/ CODE #######
ama<-csr_dataset("d20200328_UEYAMA_FAIRBANKS")
amars<-anj$data
names(amars)

amars %>%
  ggplot(aes(x = CSR_TIMESTAMP_BEGIN, y = CSR_T5)) +
  geom_point(color="blue")

amars %>%
  ggplot(aes(x = CSR_TIMESTAMP_BEGIN, y = CSR_TAIR_AMB)) +
  geom_point(color="blue")











