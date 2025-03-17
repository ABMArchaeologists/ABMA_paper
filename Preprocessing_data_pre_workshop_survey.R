### This script was used for preprocessing the data before analyses.
### The underlying data is not share due to privacy issues in the original data.
### The output data is used in the analyses scripts

library(dplyr)
library(stringr)

# importing data.
CAA_202304 <- read.csv("data/CAA_202304.csv")
CAA_202304$Workshop <- "202304_CAA"
EAA_202308 <- read.csv("data/EAA_202308.csv")
EAA_202308$Workshop <- "202308_EAA"
CAADENLFl_202310 <- read.csv("data/CAADENLFl_202310.csv")
CAADENLFl_202310$Workshop <- "202310_CAA-DE-NLFl"
Reuvensdagen_202311 <- read.csv("data/Reuvensdagen_202311.csv")
Reuvensdagen_202311$Workshop <- "202311_Reuvensdagen"
CAAUK_202311 <- read.csv("data/CAAUK_202311.csv")
CAAUK_202311$Workshop <- "202311_CAA-UK"
Leiden_202312 <- read.csv("data/Leiden_202312.csv")
Leiden_202312$Workshop <- "202312_Leiden"
Aarhus_202401 <- read.csv("data/Aarhus_online_202401.csv")
Aarhus_202401$Workshop <- "202401_Aarhus"

pre_surveys <-rbind(CAA_202304, EAA_202308, CAADENLFl_202310, Reuvensdagen_202311, CAAUK_202311, Leiden_202312, Aarhus_202401)

# correct different spellings of nationalities
replace_nationality <- c("USA" = "American", "United States of America" = "American", "American " = "American",
                         "UK (Scotland)" = "British", "UK" = "British", "British / Danish" = "British",
                         "British/Canadian" = "British", "British/Canadian " = "British", "Brazil" = "Brazilian",
                         "Spain" = "Spanish", "Polish " = "Polish", "nl" = "Dutch",
                         "NL" = "Dutch", "Nl" = "Dutch", "Netherlands" = "Dutch",
                         "Nederlands" = "Dutch", "Dutch " = "Dutch", "Ditch" = "Dutch",
                         "Dutch and Israeli" = "Dutch", "israel" = "Israeli", "italian" = "Italian",
                         "Italy" = "Italian", "Hungary" = "Hungarian", "Hungarian " = "Hungarian",
                         "Hungary " = "Hungarian", "greek" = "Greek", "Germany" = "German",
                         "german" = "German","China" = "Chinese", "Belgium" = "Belgian",
                         "Belgium " = "Belgian", "Belg" = "Belgian", "czech" = "Czech",
                         "cz" = "Czech", "Argentinian/Venezuelan/Italian" = "Argentinian", "Austrian" = "Austrian",
                         "New Zealand" = "New Zealander", "INDIAN" = "Indian", "CZ" = "Czech",
                         "Belgianian" = "Belgian", "Albanian " = "Albanian", "British(Scotland)" = "British",
                         "Brazilianian" = "Brazilian", "Dutchand Israeli" = "Dutch", "Macedonian " = "Macedonian",
                         "New Zealanderer" = "New Zealander", "British " = "British", "Egyptian " = "Egyptian",
                         "English" = "British", "British(Scotland)" = "British")
pre_surveys$What.is.you.nationality....Nationality...Text <- str_replace_all(pre_surveys$What.is.you.nationality....Nationality...Text, replace_nationality)
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text==""] <- "_not shared_"

# age data correction
pre_surveys$What.is.your.age.[pre_surveys$What.is.your.age.==""] <- "Not shared"
pre_surveys$What.is.your.age.[pre_surveys$What.is.your.age.=="Prefer not to share this"] <- "Not shared"
pre_surveys$What.is.your.age.[pre_surveys$What.is.your.age.=="Over 71 years"] <- "71 years or older"

# gender fill blanks
pre_surveys$What.is.your.gender....Selected.Choice[pre_surveys$What.is.your.gender....Selected.Choice==""] <- "Not shared"

pre_surveys$Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice <- gsub("Other, please describe:", "Other", pre_surveys$Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice)
pre_surveys$Have.you.ever.applied.Agent.Based.Modelling.[pre_surveys$Have.you.ever.applied.Agent.Based.Modelling.==""] <- NA
pre_surveys$Which.computer.skills.do.you.have....Selected.Choice <- gsub("Other, please describe:", "Other", pre_surveys$Which.computer.skills.do.you.have....Selected.Choice)
pre_surveys$Which.of.the.option.below.applies.to.you.[pre_surveys$Which.of.the.option.below.applies.to.you.==""] <- "Not Shared"
pre_surveys$Do.you.know.what.Agent.Based.Modeling.is.[pre_surveys$Do.you.know.what.Agent.Based.Modeling.is.==""] <- "Not shared"

write.csv(pre_surveys, "data/pre_workshop_surveys.csv")
saveRDS(pre_surveys, "data/pre_workshop_surveys.rds")
