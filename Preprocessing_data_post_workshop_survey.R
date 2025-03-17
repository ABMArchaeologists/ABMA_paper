### This script was used for preprocessing the data before analyses.
### The underlying data is not share due to privacy issues in the original data.
### The output data is used in the analyses scripts

library(dplyr)
library(lubridate)

post_surveys <- read.csv("data/post_workshop_surveys.csv")

post_surveys$Workshop[date(post_surveys$Start.Date)=="2023-04-03"]  <- "202304_CAA"
post_surveys$Workshop[date(post_surveys$Start.Date)=="2023-08-31"]  <- "202308_EAA"
post_surveys$Workshop[date(post_surveys$Start.Date)=="2023-10-12"]  <- "202310_CAA-DE-NLFl"
post_surveys$Workshop[date(post_surveys$Start.Date)=="2023-10-13"]  <- "202310_CAA-DE-NLFl"
post_surveys$Workshop[date(post_surveys$Start.Date)=="2023-11-16"]  <- "202311_Reuvensdagen"
post_surveys$Workshop[date(post_surveys$Start.Date)=="2023-11-24"]  <- "202311_CAA_UK"
post_surveys$Workshop[date(post_surveys$Start.Date)=="2023-12-12"]  <- "202312_Leiden"
post_surveys$Workshop[date(post_surveys$Start.Date)=="2023-12-13"]  <- "202312_Leiden"
post_surveys$Workshop[date(post_surveys$Start.Date)>="2024-01-22"]  <- "202401_Aarhus"


#correct typos in the survey
post_surveys$How.would.you.rate.the.teaching.material.[post_surveys$How.would.you.rate.the.teaching.material.=="Sufficent"] <- "Sufficient"
post_surveys$How.would.you.rate.the.teaching.material.[post_surveys$How.would.you.rate.the.teaching.material.==""] <- "Not shared"
post_surveys$How.would.you.rate.the.workshop.in.general.[post_surveys$How.would.you.rate.the.workshop.in.general.=="Sufficent"] <- "Sufficient"
post_surveys$How.would.you.rate.the.trainers.teachers.[post_surveys$How.would.you.rate.the.trainers.teachers.=="Sufficent"] <- "Sufficient"
post_surveys$How.would.you.rate.the.trainers.teachers.[post_surveys$How.would.you.rate.the.trainers.teachers.==""] <- "Not shared"
post_surveys$Do.you.think.that.you.will.apply.ABM.to.your.own.research..If.yes..can.you.elaborate....Selected.Choice[post_surveys$Do.you.think.that.you.will.apply.ABM.to.your.own.research..If.yes..can.you.elaborate....Selected.Choice==""] <- NA

drop_privacy <- c("IP.Address","Recipient.Last.Name","Recipient.First.Name","Recipient.Email","External.Data.Reference", "Location.Latitude", "Location.Longitude","User.Language")
post_surveys <- post_surveys[,!(names(post_surveys) %in% drop_privacy)]

write.csv(post_surveys, "data/post_workshop_surveys.csv")
saveRDS(post_surveys, "data/post_workshop_surveys.rds")
