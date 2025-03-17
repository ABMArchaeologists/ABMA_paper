library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)

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

pre_surveys %>%
  filter(`Do.you.know.what.Agent.Based.Modeling.is.`!="Not shared") %>%
  select(Do.you.know.what.Agent.Based.Modeling.is., Workshop) %>%
  group_by(`Do.you.know.what.Agent.Based.Modeling.is.`, Workshop) %>%
  mutate(count_response = n()) %>%
  distinct() %>%
  ggplot(aes(fill = `Do.you.know.what.Agent.Based.Modeling.is.`, x=Workshop, y = count_response)) +
  geom_bar(na.rm = TRUE, position="fill", stat="identity") +
  geom_text(aes(label=count_response), position = position_fill(vjust = 0.5)) +
  xlab("") +  scale_fill_discrete(name = "I know what ABM is") + ylab("Percentage") +  coord_flip()
ggsave("export/preworkshop/ABM_known_percent_stacked.png", width = 12)


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
  mutate(count_software = n()) %>%
  ggplot(aes(x=reorder(`Which.software.do.you.know.for.Agent.Based.Modeling....Selected.Choice`, count_software))) + geom_bar() +
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
  mutate(count_skills = n()) %>%
  ggplot(aes(x=reorder(`Which.computer.skills.do.you.have....Selected.Choice`, count_skills))) + geom_bar() +
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

pre_surveys %>%
  group_by(`What.is.you.nationality....Nationality...Text`) %>%
  mutate(count_nationality = n()) %>%
  ggplot(aes(x=reorder(`What.is.you.nationality....Nationality...Text`, count_nationality))) + geom_bar() +  coord_flip() + xlab("Nationality")
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

