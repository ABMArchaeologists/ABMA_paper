library(tidyverse)

post_surveys <- read.csv("data/ABM_post_workshop_survey.csv")

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
post_surveys$How.would.you.rate.the.workshop.in.general.[post_surveys$How.would.you.rate.the.workshop.in.general.=="Sufficent"] <- "Sufficient"
post_surveys$How.would.you.rate.the.trainers.teachers.[post_surveys$How.would.you.rate.the.trainers.teachers.=="Sufficent"] <- "Sufficient"

write.csv(post_surveys, "data/post_workshop_surveys.csv")

factor_levels <- c("Excellent", "Good", "Sufficient", "Insufficient")

# rating workshop in general
post_surveys %>%
  mutate(How.would.you.rate.the.workshop.in.general. = fct_relevel(How.would.you.rate.the.workshop.in.general., factor_levels)) %>%
  ggplot(aes(x=How.would.you.rate.the.workshop.in.general.)) + geom_bar() +
  xlab("Rating of the workshop")
ggsave("export/postworkshop/rating_workshop.png")

post_surveys %>%
  mutate(How.would.you.rate.the.workshop.in.general. = fct_relevel(How.would.you.rate.the.workshop.in.general., factor_levels)) %>%
  ggplot(aes(x=How.would.you.rate.the.workshop.in.general.)) + geom_bar() +
  xlab("Rating of the workshop") + facet_grid(~Workshop) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("export/postworkshop/rating_workshop_facet.png", width = 10)

# rating teaching material
post_surveys %>%
  mutate(How.would.you.rate.the.teaching.material. = fct_relevel(How.would.you.rate.the.teaching.material., factor_levels)) %>%
  ggplot(aes(x=How.would.you.rate.the.teaching.material.)) + geom_bar() +
  xlab("Rating of the teaching material")
ggsave("export/postworkshop/rating_teaching_material.png")

post_surveys %>%
  mutate(How.would.you.rate.the.teaching.material. = fct_relevel(How.would.you.rate.the.teaching.material., factor_levels)) %>%
  ggplot(aes(x=How.would.you.rate.the.teaching.material.)) + geom_bar() +
  xlab("Rating of the teaching material") + facet_grid(~Workshop) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("export/postworkshop/rating_teaching_material_facet.png", width = 10)

# rating teachers
post_surveys %>%
  mutate(How.would.you.rate.the.trainers.teachers. = fct_relevel(How.would.you.rate.the.trainers.teachers., factor_levels)) %>%
  ggplot(aes(x=How.would.you.rate.the.trainers.teachers.)) + geom_bar() +
  xlab("Rating of the teacher")
ggsave("export/postworkshop/rating_teaching_material.png")

post_surveys %>%
  mutate(How.would.you.rate.the.trainers.teachers. = fct_relevel(How.would.you.rate.the.trainers.teachers., factor_levels)) %>%
  ggplot(aes(x=How.would.you.rate.the.trainers.teachers.)) + geom_bar() +
  xlab("Rating of the teacher") + facet_grid(~Workshop) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("export/postworkshop/rating_workshop_facet.png", width = 10)

# Apply ABM in the future?

post_surveys$Do.you.think.that.you.will.apply.ABM.to.your.own.research..If.yes..can.you.elaborate....Selected.Choice[post_surveys$Do.you.think.that.you.will.apply.ABM.to.your.own.research..If.yes..can.you.elaborate....Selected.Choice==""] <- NA

post_surveys %>%
  filter(!is.na(Do.you.think.that.you.will.apply.ABM.to.your.own.research..If.yes..can.you.elaborate....Selected.Choice)) %>%
  ggplot(aes(x = 1, fill = `Do.you.think.that.you.will.apply.ABM.to.your.own.research..If.yes..can.you.elaborate....Selected.Choice`)) +
  geom_bar(na.rm = TRUE) + coord_polar(theta = "y", start=0) + theme_void() +
  scale_fill_discrete(name = paste0("Will apply ABM in the future (n=", sum(!is.na(post_surveys$Do.you.think.that.you.will.apply.ABM.to.your.own.research..If.yes..can.you.elaborate....Selected.Choice)), ")"),
                      labels = c("No", "Yes"))
ggsave("export/postworkshop/ABM_future_apply.png")


post_surveys %>%
  filter(!is.na(Do.you.think.that.you.will.apply.ABM.to.your.own.research..If.yes..can.you.elaborate....Selected.Choice)) %>%
  ggplot(aes(x = 1, fill = `Do.you.think.that.you.will.apply.ABM.to.your.own.research..If.yes..can.you.elaborate....Selected.Choice`)) +
  geom_bar(na.rm = TRUE) +
  scale_fill_discrete(name = paste0("Will apply ABM in the future (n=", sum(!is.na(post_surveys$Do.you.think.that.you.will.apply.ABM.to.your.own.research..If.yes..can.you.elaborate....Selected.Choice)), ")"),
                      labels = c("No", "Yes")) + facet_grid(~Workshop) +
  theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), legend.position="bottom")
ggsave("export/postworkshop/ABM_future_apply_face.png", width = 10)


