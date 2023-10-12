library(tidyverse)

CAA_202310 <- read.csv("data/CAADENLFl_202310.csv")

# do you know ABM
ggplot(CAA_202310, aes(x = `Do.you.know.what.Agent.Based.Modeling.is.`)) +
  geom_bar() + xlab("I know what ABM is") # + coord_polar(theta = "y", start=0) #+ theme_void()
ggsave("export/know_ABM.png")

ggplot(CAA_202310, aes(x = 1, fill = `Do.you.know.what.Agent.Based.Modeling.is.`)) +
  geom_bar() + coord_polar(theta = "y", start=0) + theme_void() +
  scale_fill_discrete(name = "I know what ABM is")
ggsave("export/know_ABM_pie.png")

# COmputer skills

CAA_202310$Which.computer.skills.do.you.have....Selected.Choice <- gsub("Other, please describe:", "Other", CAA_202310$Which.computer.skills.do.you.have....Selected.Choice)

CAA_202310 %>%
  select(`Which.computer.skills.do.you.have....Selected.Choice`) %>%
  separate_rows(`Which.computer.skills.do.you.have....Selected.Choice`, sep = ',\\s*') %>%
  group_by(`Which.computer.skills.do.you.have....Selected.Choice`) %>%
  ggplot(aes(x=`Which.computer.skills.do.you.have....Selected.Choice`)) + geom_bar() +
  xlab("Computer skills") + coord_flip()

ggsave("export/computer_skills.png")


# Gender
CAA_202310$What.is.your.gender....Selected.Choice[CAA_202310$What.is.your.gender....Selected.Choice==""] <- "Not shared"
ggplot(CAA_202310, aes(x = 1, fill = `What.is.your.gender....Selected.Choice`)) +
  geom_bar() + xlab("Gender") + coord_polar(theta = "y", start=0) + theme_void() +
  scale_fill_discrete(name = "Gender")
ggsave("export/gender.png")

# age
ggplot(CAA_202310, aes(x = `What.is.your.age.`)) +
  geom_bar() + xlab("Age")
ggsave("export/age_classes.png")

#nationality
ggplot(CAA_202310, aes(x = `What.is.you.nationality....Nationality...Text`)) +
  geom_bar() +  coord_flip() + xlab("Nationality")
ggsave("export/nationality.png")

# student

ggplot(CAA_202310, aes(x = 1, fill = `Which.of.the.option.below.applies.to.you.`)) +
  geom_bar() + coord_polar(theta = "y", start=0) + theme_void() +
  scale_fill_discrete(name = "Student or working")
ggsave("export/student_working_pie.png")

