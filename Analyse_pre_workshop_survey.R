library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

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

write.csv(pre_surveys, "data/pre_workshop_surveys.csv")

# do you know ABM

ggplot(pre_surveys, aes(x = 1, fill = `Do.you.know.what.Agent.Based.Modeling.is.`)) +
  geom_bar() + coord_polar(theta = "y", start=0) + theme_void() +
  scale_fill_discrete(name = paste0("I know what ABM is (n=", sum(!is.na(pre_surveys$Do.you.know.what.Agent.Based.Modeling.is.)), ")"))
ggsave("export/preworkshop/ABM_known_pie.png")

ggplot(pre_surveys, aes(x = `Do.you.know.what.Agent.Based.Modeling.is.`)) +
  geom_bar() + xlab("I know what ABM is") + facet_grid(~Workshop)
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

pre_surveys$Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice <- gsub("Other, please describe:", "Other", pre_surveys$Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice)

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

pre_surveys$Have.you.ever.applied.Agent.Based.Modelling.[pre_surveys$Have.you.ever.applied.Agent.Based.Modelling.==""] <- NA

ggplot(pre_surveys[!is.na(pre_surveys$Have.you.ever.applied.Agent.Based.Modelling.),], aes(x = 1, fill = `Have.you.ever.applied.Agent.Based.Modelling.`)) +
  geom_bar(na.rm = TRUE) + coord_polar(theta = "y", start=0) + theme_void() +
  scale_fill_discrete(name = paste0("Applied ABM before (n=", sum(!is.na(pre_surveys$Have.you.ever.applied.Agent.Based.Modelling.)), ")"))
ggsave("export/preworkshop/gender_pie.png")

ggplot(pre_surveys[!is.na(pre_surveys$Have.you.ever.applied.Agent.Based.Modelling.),], aes(x = `Have.you.ever.applied.Agent.Based.Modelling.`)) +
  geom_bar() + xlab("Applied ABM before")  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_grid(~Workshop)
ggsave("export/preworkshop/ABM_applied_before_facet.png", width = 12)

# Computer skills

pre_surveys$Which.computer.skills.do.you.have....Selected.Choice <- gsub("Other, please describe:", "Other", pre_surveys$Which.computer.skills.do.you.have....Selected.Choice)

pre_surveys %>%
  select(`Which.computer.skills.do.you.have....Selected.Choice`) %>%
  separate_rows(`Which.computer.skills.do.you.have....Selected.Choice`, sep = ',\\s*') %>%
  group_by(`Which.computer.skills.do.you.have....Selected.Choice`) %>%
  ggplot(aes(x=`Which.computer.skills.do.you.have....Selected.Choice`)) + geom_bar() +
  xlab("Computer skills") + coord_flip()

ggsave("export/preworkshop/computer_skills.png")

pre_surveys %>%
  select(`Which.computer.skills.do.you.have....Selected.Choice`, Workshop) %>%
  separate_rows(`Which.computer.skills.do.you.have....Selected.Choice`, sep = ',\\s*') %>%
  group_by(`Which.computer.skills.do.you.have....Selected.Choice`) %>%
  ggplot(aes(x=`Which.computer.skills.do.you.have....Selected.Choice`)) + geom_bar() +
  xlab("Computer skills") + coord_flip() + facet_grid(~Workshop)
ggsave("export/preworkshop/computer_skills_facet.png", width = 12)


# Gender
pre_surveys$What.is.your.gender....Selected.Choice[pre_surveys$What.is.your.gender....Selected.Choice==""] <- "Not shared"

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
pre_surveys$What.is.your.age.[pre_surveys$What.is.your.age.==""] <- "Not shared"
pre_surveys$What.is.your.age.[pre_surveys$What.is.your.age.=="Prefer not to share this"] <- "Not shared"
pre_surveys$What.is.your.age.[pre_surveys$What.is.your.age.=="Over 71 years"] <- "71 years or older"

ggplot(pre_surveys, aes(x = `What.is.your.age.`)) +
  geom_bar() + xlab("Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("export/preworkshop/age_classes.png")

ggplot(pre_surveys, aes(x = `What.is.your.age.`)) +
  geom_bar() + xlab("Age") + facet_grid(~Workshop) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("export/preworkshop/age_classes_facet.png", width = 12)


#nationality

pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="USA"] <- "American"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="United States of America"] <- "American"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="American "] <- "American"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="UK (Scotland)"] <- "British"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="UK"] <- "British"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="British / Danish"] <- "British"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="British/Canadian"] <- "British"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="British/Canadian "] <- "British"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Brazil"] <- "Brazilian"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Spain"] <- "Spanish"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Polish "] <- "Polish"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="nl"] <- "Dutch"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="NL"] <- "Dutch"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Nl"] <- "Dutch"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Netherlands"] <- "Dutch"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Dutch "] <- "Dutch"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Ditch"] <- "Dutch"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Dutch and Israeli"] <- "Dutch"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="israel"] <- "Israeli"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="italian"] <- "Italian"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Italy"] <- "Italian"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Hungary"] <- "Hungarian"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Hungarian "] <- "Hungarian"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Hungary "] <- "Hungarian"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="greek"] <- "Greek"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Germany"] <- "German"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="german"] <- "German"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="China"] <- "Chinese"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Belgium"] <- "Belgian"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Belgium "] <- "Belgian"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Belg"] <- "Belgian"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="czech"] <- "Czech"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="cz"] <- "Czech"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Argentinian/Venezuelan/Italian"] <- "Argentinian"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="Austrian"] <- "Austrian"
pre_surveys$What.is.you.nationality....Nationality...Text[pre_surveys$What.is.you.nationality....Nationality...Text=="New Zealand"] <- "New Zealander"

ggplot(pre_surveys, aes(x = `What.is.you.nationality....Nationality...Text`)) +
  geom_bar() +  coord_flip() + xlab("Nationality")
ggsave("export/preworkshop/nationality.png")

ggplot(pre_surveys, aes(x = `What.is.you.nationality....Nationality...Text`)) +
  geom_bar() +  coord_flip() + xlab("Nationality") + scale_y_continuous(breaks=c(0,5,10)) +
  facet_grid(~Workshop)
ggsave("export/preworkshop/nationality_facet.png", width = 12)


# student

pre_surveys$Which.of.the.option.below.applies.to.you.[pre_surveys$Which.of.the.option.below.applies.to.you.==""] <- "Not Shared"
ggplot(pre_surveys, aes(x = 1, fill = `Which.of.the.option.below.applies.to.you.`)) +
  geom_bar() + coord_polar(theta = "y", start=0) + theme_void() +
  scale_fill_discrete(name = paste0("Student or working (n=", sum(!is.na(pre_surveys$Which.of.the.option.below.applies.to.you.)), ")"))
ggsave("export/preworkshop/student_working_pie.png")


ggplot(pre_surveys, aes(x = `Which.of.the.option.below.applies.to.you.`)) +
  geom_bar() +  facet_grid(~Workshop) + xlab("Student or working") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("export/preworkshop/student_working_bar_facet.png", width = 12)

