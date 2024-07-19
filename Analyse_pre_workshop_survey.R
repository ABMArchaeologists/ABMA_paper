library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)

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
pre_surveys <- readRDS("data/pre_workshop_surveys.rds")

# do you know ABM

ggplot(pre_surveys, aes(x = 1, fill = `Do.you.know.what.Agent.Based.Modeling.is.`)) +
  geom_bar() + coord_polar(theta = "y", start=0) + theme_void() +
  scale_fill_discrete(name = paste0("I know what ABM is (n=", sum(!is.na(pre_surveys$Do.you.know.what.Agent.Based.Modeling.is.)), ")"))
ggsave("export/preworkshop/ABM_known_pie.png")

pre_surveys %>%
  filter(`Do.you.know.what.Agent.Based.Modeling.is.`!="Not shared") %>%
  ggplot(aes(x = `Do.you.know.what.Agent.Based.Modeling.is.`)) +
    geom_bar(na.rm = TRUE) + xlab("I know what ABM is") + facet_grid(~Workshop)
ggsave("export/preworkshop/ABM_known_facet.png", width = 12)

# Theory on ABM

pre_surveys %>%
  select(`How.do.you.judge.the.available.theory.on.Agent.Based.Modelling.`) %>%
  filter(`How.do.you.judge.the.available.theory.on.Agent.Based.Modelling.`!="") %>%
  mutate(`How.do.you.judge.the.available.theory.on.Agent.Based.Modelling.` =
           fct_relevel(`How.do.you.judge.the.available.theory.on.Agent.Based.Modelling.`,
                       "No opinion", "Limited", "Sufficient", "More than sufficient", "Very good")) %>%
  ggplot(aes(x = `How.do.you.judge.the.available.theory.on.Agent.Based.Modelling.`)) +
  geom_bar() + xlab("Quality of theory on ABM") + coord_flip() #+ scale_y_continuous(breaks=c(0,2,4,6,8,10))
ggsave("export/preworkshop/ABM_theory_quality.png")

pre_surveys %>%
  select(`How.do.you.judge.the.available.theory.on.Agent.Based.Modelling.`, Workshop) %>%
  filter(`How.do.you.judge.the.available.theory.on.Agent.Based.Modelling.`!="") %>%
  mutate(`How.do.you.judge.the.available.theory.on.Agent.Based.Modelling.` =
           fct_relevel(`How.do.you.judge.the.available.theory.on.Agent.Based.Modelling.`,
                       "No opinion", "Limited", "Sufficient", "More than sufficient", "Very good")) %>%
  ggplot(aes(x = `How.do.you.judge.the.available.theory.on.Agent.Based.Modelling.`)) +
  geom_bar() + xlab("Quality of theory on ABM") + coord_flip() +
  facet_grid(~Workshop) #+ scale_y_continuous(breaks=c(0,2,4,6,8,10))
ggsave("export/preworkshop/ABM_theory_quality_facet.png", width = 12)

# ABM software



pre_surveys %>%
  select(`Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice`) %>%
  filter(`Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice`!="") %>%
  separate_rows(Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice, sep = ',\\s*') %>%
  group_by(`Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice`) %>%
  ggplot(aes(x=`Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice`)) + geom_bar() +
  xlab("ABM Software known") + coord_flip()
ggsave("export/preworkshop/ABM_software_known.png")

pre_surveys %>%
  select(`Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice`, Workshop) %>%
  filter(`Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice`!="") %>%
  separate_rows(Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice, sep = ',\\s*') %>%
  group_by(`Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice`) %>%
  ggplot(aes(x=`Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice`)) + geom_bar() +
  xlab("ABM Software known") + coord_flip() + facet_grid(~Workshop)
ggsave("export/preworkshop/ABM_software_knownfacet.png", width = 12)

# ABM Applied



ggplot(pre_surveys[!is.na(pre_surveys$Have.you.ever.applied.Agent.Based.Modelling.),], aes(x = 1, fill = `Have.you.ever.applied.Agent.Based.Modelling.`)) +
  geom_bar(na.rm = TRUE) + coord_polar(theta = "y", start=0) + theme_void() +
  scale_fill_discrete(name = paste0("Applied ABM before (n=", sum(!is.na(pre_surveys$Have.you.ever.applied.Agent.Based.Modelling.)), ")"))
ggsave("export/preworkshop/ABM_applied_before_pie.png")


pre_surveys %>%
  select(`Have.you.ever.applied.Agent.Based.Modelling.`, Workshop) %>%
  filter(!is.na(`Have.you.ever.applied.Agent.Based.Modelling.`)) %>%
  ggplot(aes(x = `Have.you.ever.applied.Agent.Based.Modelling.`)) +
    geom_bar(na.rm = TRUE) + xlab("Applied ABM before")  +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_grid(~Workshop)
ggsave("export/preworkshop/ABM_applied_before_facet.png", width = 12)

# Computer skills

pre_surveys %>%
  select(`Which.computer.skills.do.you.have....Selected.Choice`) %>%
  separate_rows(`Which.computer.skills.do.you.have....Selected.Choice`, sep = ',\\s*') %>%
  filter(`Which.computer.skills.do.you.have....Selected.Choice`!="") %>%
  group_by(`Which.computer.skills.do.you.have....Selected.Choice`) %>%
  ggplot(aes(x=`Which.computer.skills.do.you.have....Selected.Choice`)) + geom_bar() +
    xlab("Computer skills") + coord_flip()
ggsave("export/preworkshop/computer_skills.png")

pre_surveys %>%
  select(`Which.computer.skills.do.you.have....Selected.Choice`, Workshop) %>%
  separate_rows(`Which.computer.skills.do.you.have....Selected.Choice`, sep = ',\\s*') %>%
  filter(`Which.computer.skills.do.you.have....Selected.Choice`!="") %>%
  group_by(`Which.computer.skills.do.you.have....Selected.Choice`) %>%
  ggplot(aes(x=`Which.computer.skills.do.you.have....Selected.Choice`)) + geom_bar() +
  xlab("Computer skills") + coord_flip() + facet_grid(~Workshop)
ggsave("export/preworkshop/computer_skills_facet.png", width = 12)



# Gender

ggplot(pre_surveys, aes(x = 1, fill = `What.is.your.gender....Selected.Choice`)) +
  geom_bar() + xlab("Gender") + coord_polar(theta = "y", start=0) + theme_void() +
  scale_fill_discrete(name = paste0("Gender (n=", sum(!is.na(pre_surveys$What.is.your.gender....Selected.Choice)), ")"))
ggsave("export/preworkshop/gender_pie.png")

ggplot(pre_surveys, aes(x = `What.is.your.gender....Selected.Choice`)) +
  geom_bar() + xlab("Gender")  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_grid(~Workshop)
ggsave("export/preworkshop/gender_bar_facet.png", width = 12)


# age
ggplot(pre_surveys, aes(x = `What.is.your.age.`)) +
  geom_bar() + xlab("Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("export/preworkshop/age_classes.png")

ggplot(pre_surveys, aes(x = `What.is.your.age.`)) +
  geom_bar() + xlab("Age") + facet_grid(~Workshop) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("export/preworkshop/age_classes_facet.png", width = 12)

# age and gender
ggplot(pre_surveys, aes(x = `What.is.your.age.`, fill = `What.is.your.gender....Selected.Choice`)) +
  geom_bar() + labs(x= "Age", y="Count", fill = "Gender") + facet_grid(~Workshop) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("export/preworkshop/age_classes_gender_facet.png", width = 12)


#nationality

ggplot(pre_surveys, aes(x = `What.is.you.nationality....Nationality...Text`)) +
  geom_bar() +  coord_flip() + xlab("Nationality") + scale_x_discrete(limits=rev)
ggsave("export/preworkshop/nationality.png")

ggplot(pre_surveys, aes(x = `What.is.you.nationality....Nationality...Text`)) +
  geom_bar() +  coord_flip() + xlab("Nationality") + scale_y_continuous(breaks=c(0,5,10)) +
  facet_grid(~Workshop)
ggsave("export/preworkshop/nationality_facet.png", width = 12)

pre_surveys %>%
  count(`What.is.you.nationality....Nationality...Text`) %>%
  mutate(`What.is.you.nationality....Nationality...Text` = fct_reorder(`What.is.you.nationality....Nationality...Text`,n)) %>%
  ggplot(aes(x = `What.is.you.nationality....Nationality...Text`, y= n)) +
  geom_col() + coord_flip() + xlab("Nationality")
ggsave("export/preworkshop/nationality_ordered_n.png")

# student

ggplot(pre_surveys, aes(x = 1, fill = `Which.of.the.option.below.applies.to.you.`)) +
  geom_bar() + coord_polar(theta = "y", start=0) + theme_void() +
  scale_fill_discrete(name = paste0("Student or working (n=", sum(!is.na(pre_surveys$Which.of.the.option.below.applies.to.you.)), ")"))
ggsave("export/preworkshop/student_working_pie.png")


ggplot(pre_surveys, aes(x = `Which.of.the.option.below.applies.to.you.`)) +
  geom_bar() +  facet_grid(~Workshop) + xlab("Student or working") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("export/preworkshop/student_working_bar_facet.png", width = 12)

