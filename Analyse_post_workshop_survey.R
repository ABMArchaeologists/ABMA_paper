### analyses of the surveys after the workshops
library(ggplot2)
library(dplyr)
library(lubridate)
library(forcats)

post_surveys <- readRDS("data/post_workshop_surveys.rds")

factor_levels <- c("Excellent", "Good", "Sufficient", "Insufficient", "Not shared")

# rating workshop in general
post_surveys %>%
  mutate(How.would.you.rate.the.workshop.in.general. = factor(How.would.you.rate.the.workshop.in.general., factor_levels)) %>%
  ggplot(aes(x=How.would.you.rate.the.workshop.in.general.)) + geom_bar() +
  xlab("Rating of the workshop") +
  scale_x_discrete(drop=FALSE)
ggsave("export/postworkshop/rating_workshop.png")

post_surveys %>%
  mutate(How.would.you.rate.the.workshop.in.general. = factor(How.would.you.rate.the.workshop.in.general., factor_levels)) %>%
  ggplot(aes(x=How.would.you.rate.the.workshop.in.general.)) + geom_bar() +
  xlab("Rating of the workshop") + facet_grid(~Workshop) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(drop=FALSE)
ggsave("export/postworkshop/rating_workshop_facet.png", width = 10)

# rating teaching material

post_surveys %>%
  mutate(How.would.you.rate.the.teaching.material. = factor(How.would.you.rate.the.teaching.material., factor_levels)) %>%
  ggplot(aes(x=How.would.you.rate.the.teaching.material.)) + geom_bar() +
  xlab("Rating of the teaching material") +
  scale_x_discrete(drop=FALSE)
ggsave("export/postworkshop/rating_teaching_material.png")

post_surveys %>%
  mutate(How.would.you.rate.the.teaching.material. = factor(How.would.you.rate.the.teaching.material., factor_levels)) %>%
  ggplot(aes(x=How.would.you.rate.the.teaching.material.)) + geom_bar() +
  xlab("Rating of the teaching material") + facet_grid(~Workshop) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(drop=FALSE)
ggsave("export/postworkshop/rating_teaching_material_facet.png", width = 10)

# rating teachers
post_surveys %>%
  mutate(How.would.you.rate.the.trainers.teachers. = factor(How.would.you.rate.the.trainers.teachers., factor_levels)) %>%
  ggplot(aes(x=How.would.you.rate.the.trainers.teachers.)) + geom_bar() +
  xlab("Rating of the teacher") + scale_x_discrete(drop=FALSE)
ggsave("export/postworkshop/rating_teachers.png")

post_surveys %>%
  mutate(How.would.you.rate.the.trainers.teachers. = factor(How.would.you.rate.the.trainers.teachers., factor_levels)) %>%
  ggplot(aes(x=How.would.you.rate.the.trainers.teachers.)) + geom_bar() +
  xlab("Rating of the teacher") + facet_grid(~Workshop) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(drop=FALSE)
ggsave("export/postworkshop/rating_teachers_facet.png", width = 10)

# Apply ABM in the future?

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


