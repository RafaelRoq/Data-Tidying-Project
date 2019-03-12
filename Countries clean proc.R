#setwd("C:/Users/josue/Documents/Shiny-regions/Regions")

data <- read.csv("countries of the world.csv")

##First clean the data
##Tidy the names of the columns
colnames(data)<- c("Country","Region","Population","Area","PopDens",
                   "Coastline", "NetMigration",
                   "InfantMortality","GDP","Literacy","PhonesPer1000",
                   "Arable","Crops","Other","Climate",
                   "Birthrate","Deathrate","Agriculture","Industry","Service")
lapply(data,class)



##Population,Area and GDP are integers, the others are factor
##We want to mantain Country, Region and Climate as factors, the others should be numeric


data$PopDens <- as.numeric(gsub(",",".",data$PopDens))
data$Coastline <- as.numeric(gsub(",",".",data$Coastline))
data$NetMigration <- as.numeric(gsub(",",".",data$NetMigration))
data$InfantMortality <- as.numeric(gsub(",",".",data$InfantMortality))
data$Literacy <- as.numeric(gsub(",",".",data$Literacy))
data$PhonesPer1000 <- as.numeric(gsub(",",".",data$PhonesPer1000))
data$Arable <- as.numeric(gsub(",",".",data$Arable))
data$Crops <- as.numeric(gsub(",",".",data$Crops))
data$Other <- as.numeric(gsub(",",".",data$Other))
data$Birthrate <- as.numeric(gsub(",",".",data$Birthrate))
data$Deathrate <- as.numeric(gsub(",",".",data$Deathrate))
data$Agriculture <- as.numeric(gsub(",",".",data$Agriculture))
data$Industry <- as.numeric(gsub(",",".",data$Industry))
data$Service <- as.numeric(gsub(",",".",data$Service))

lapply(data,class)
##Now the class of the columns are correct
##Let's clean the NA's
data$InfantMortality[is.na(data$InfantMortality)]<- median(data$InfantMortality, na.rm = TRUE)
data$GDP[is.na(data$GDP)]<- median(data$GDP, na.rm = TRUE)
data$NetMigration[is.na(data$NetMigration)]<- median(data$NetMigration, na.rm = TRUE)
data$Birthrate[is.na(data$Birthrate)]<- median(data$Birthrate, na.rm = TRUE)
data$Deathrate[is.na(data$Deathrate)]<- median(data$Deathrate, na.rm = TRUE)
data$Literacy[is.na(data$Literacy)]<- median(data$Literacy, na.rm = TRUE)
data$PhonesPer1000[is.na(data$PhonesPer1000)]<- median(data$PhonesPer1000, na.rm = TRUE)
data$Agriculture[is.na(data$Agriculture)]<- median(data$Agriculture, na.rm = TRUE)
data$Industry[is.na(data$Industry)]<- median(data$Industry, na.rm = TRUE)
data$Service[is.na(data$Service)]<- median(data$Service, na.rm = TRUE)
data$Arable[is.na(data$Arable)]<- median(data$Arable, na.rm = TRUE)
data$Crops[is.na(data$Crops)]<- median(data$Crops, na.rm = TRUE)
data$Other[is.na(data$Other)]<- median(data$Other, na.rm = TRUE)

data[data$Climate=="1,5",]$Climate <- "2"
data[data$Climate=="2,5",]$Climate <- "3"

##Change names of countries to match the database (maps)
data$Country <- as.character(data$Country)
data[data$Country=="Congo, Repub. of the ",]$Country <- "Republic of Congo"

dataForMap<-data[!(data$Country %in% c("Antigua & Barbuda ","Gaza Strip ","Gibraltar ",
                         "British Virgin Is. ","Gaza Strip ",
                         "Gibraltar ","Guadeloupe ","Martinique ","Mayotte ",
                         "Micronesia, Fed. St. ","Netherlands Antilles ",
                         "N. Mariana Islands ","Reunion ","St Pierre & Miquelon ",
                         "Trinidad & Tobago ","Turks & Caicos Is ","Bosnia & Herzegovina ")),]





data$Country <- as.factor(data$Country)

library(rworldmap)

myWorldMap <- joinCountryData2Map(dataForMap, nameJoinColumn = "Country", joinCode="NAME",verbose = TRUE)


mapCountryData(myWorldMap, nameColumnToPlot = "Population")

levels(data$Country)=gsub(" $","",levels(data$Country)) 
levels(data$Region)=c("ASIA (EX. NEAR EAST)","BALTICS","C.W. OF IND. STATES",
                      "EASTERN EUROPE","LATIN AMER. & CARIB","NEAR EAST","NORTHERN AFRICA",
                      "NORTHERN AMERICA","OCEANIA","SUB-SAHARAN AFRICA","WESTERN EUROPE")
