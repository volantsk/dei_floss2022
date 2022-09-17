install.packages("tidyverse")
library(tidyverse)

# loading CSV dataset
path <- "DEI-LF_2021-cleaning_1.csv"
dei_data <- read.csv(path, header = TRUE, sep = ",")

# info about the dataset
dim (dei_data)

dei_data <- dei_data %>% mutate(Q49_race=case_when(
  Q49_01..With.which.racial.background.s..do.you.identify..If.multiracial..check.all.that.apply..Asian == "Asian" ~ "Asian",
  Q49_02..With.which.racial.background.s..do.you.identify..If.multiracial..check.all.that.apply..Black == "Black" ~ "Black",
  Q49_03..With.which.racial.background.s..do.you.identify..If.multiracial..check.all.that.apply..Hispanic.Latinx == "Hispanic Latinx" ~ "Hispanic",
  Q49_04..With.which.racial.background.s..do.you.identify..If.multiracial..check.all.that.apply..Native.or.Indigenous == "Native or Indigenous" ~ "Native",
  Q49_05..With.which.racial.background.s..do.you.identify..If.multiracial..check.all.that.apply..Pacific.Islander == "Pacific Islander" ~ "Pacific Islander",
  Q49_06..With.which.racial.background.s..do.you.identify..If.multiracial..check.all.that.apply..White == "White" ~ "White",
  Q49_07..With.which.racial.background.s..do.you.identify..If.multiracial..check.all.that.apply..Prefer.not.to.answer == "Prefer not to answer" ~ "Prefer not to answer",
  Q49_08_None.of.above.the.above..With.which.racial.background.s..do.you.identify..If.multiracial..check.all.that.apply..None.of.the.above == "None of the above" ~ "None of the above"
))

dei_data <- dei_data %>% mutate(Q50_Gender = case_when(
  Q50_01..What.is.your.gender..Woman == "Woman" ~ "Woman",
  Q50_02..What.is.your.gender..Man == "Man" ~ "Man",
  Q50_03..What.is.your.gender..Non.binary...third.gender == "Non-binary / third gender" ~ "Other",
  Q50_04..What.is.your.gender..Prefer.not.to.answer == "Prefer not to answer" ~ "Other",
  Q50_05..What.is.your.gender..Other == "Other" ~ "Other"
))

dei_data <- dei_data %>% mutate(Q53_sexual_orientation=case_when(
  Q53_01..What.is.your.sexual.orientation...check.all.that.apply...Asexual == "Asexual" ~ "Asexual",
  Q53_02..What.is.your.sexual.orientation...check.all.that.apply...Bisexual == "Bisexual" ~ "Bisexual",
  Q53_03..What.is.your.sexual.orientation...check.all.that.apply...Gay == "Gay" ~ "Gay",
  Q53_04..What.is.your.sexual.orientation...check.all.that.apply...Heterosexual.or.straight == "Heterosexual or straight" ~ "Heterosexual",
  Q53_05..What.is.your.sexual.orientation...check.all.that.apply...Questioning == "Questioning" ~ "Questioning",
  Q53_06..What.is.your.sexual.orientation...check.all.that.apply...Lesbian == "Lesbian" ~ "Lesbian",
  Q53_07..What.is.your.sexual.orientation...check.all.that.apply...Pansexual == "Pansexual" ~ "Pansexual",
  Q53_08..What.is.your.sexual.orientation...check.all.that.apply...Queer == "Queer" ~ "Queer",
  Q53_09..What.is.your.sexual.orientation...check.all.that.apply...Prefer.not.to.answer == "Prefer not to answer" ~ "Prefer not to answer",
  Q53_None_of_the_above..What.is.your.sexual.orientation...check.all.that.apply...None.of.the.above == "None of the above" ~ "None of the above"
))

# Create a copy of the dataset to work and simplify the information
world_dei <- dei_data

# Deleting the columns unified before
world_dei <- world_dei %>% select(everything(), -(Q49_01..With.which.racial.background.s..do.you.identify..If.multiracial..check.all.that.apply..Asian:
                                                  Q49_08_None.of.above.the.above..With.which.racial.background.s..do.you.identify..If.multiracial..check.all.that.apply..None.of.the.above), 
                                                -(Q50_01..What.is.your.gender..Woman:
                                                  Q50_05..What.is.your.gender..Other),
                                                -(Q53_01..What.is.your.sexual.orientation...check.all.that.apply...Asexual:
                                                  Q53_None_of_the_above..What.is.your.sexual.orientation...check.all.that.apply...None.of.the.above))

# Create a new dataset with only Europe info
euro_dei <- world_dei %>% filter(Q48..What.best.describes.your.location. == "Europe")

# Create a new dataset with USA and Canada info
usa_can_dei <- world_dei %>% filter(Q48..What.best.describes.your.location. == "North America (United States, Canada, and Mexico)", 
                                    Q2.Recoded..What.is.your.preferred.language. %in% c("English", "French"))

# Create a new dataset with LATAM info
latam_dei <- world_dei %>% filter(Q48..What.best.describes.your.location. %in% c("North America (United States, Canada, and Mexico)", 
                                                                                 "Central America and South America and the Caribbean"),
                                  Q2.Recoded..What.is.your.preferred.language. %in% c("Portuguese", "Spanish")
                                  )


view (latam_dei)

unique(latam_dei$Q1..What.is.your.age.)
prop.table(table(latam_dei$Q1..What.is.your.age.))

unique(latam_dei$Q53_sexual_orientation)
prop.table(table(latam_dei$Q53_sexual_orientation))

unique(latam_dei$Q44..Do.you.identify.as.a.person.with.a.dis.ability..For.the.purpose.of.this.survey.we.define.dis.ability.as.a.long.term.physical..mental..intellectual..or.sensory.impairment.which..in.interaction.with.various.attitudinal.and.environmental.barriers..hinders.your.full.and.effective.participation.in.society.on.an.equal.basis.with.others.)
prop.table(table(latam_dei$Q44..Do.you.identify.as.a.person.with.a.dis.ability..For.the.purpose.of.this.survey.we.define.dis.ability.as.a.long.term.physical..mental..intellectual..or.sensory.impairment.which..in.interaction.with.various.attitudinal.and.environmental.barriers..hinders.your.full.and.effective.participation.in.society.on.an.equal.basis.with.others.))
prop.table(table(world_dei$Q44..Do.you.identify.as.a.person.with.a.dis.ability..For.the.purpose.of.this.survey.we.define.dis.ability.as.a.long.term.physical..mental..intellectual..or.sensory.impairment.which..in.interaction.with.various.attitudinal.and.environmental.barriers..hinders.your.full.and.effective.participation.in.society.on.an.equal.basis.with.others.))

unique(usa_can_dei$Q49_race)
prop.table(table(usa_can_dei$Q49_race))
unique(latam_dei$Q49_race)
prop.table(table(latam_dei$Q49_race))

unique(latam_dei)
prop.table(table(latam_dei))

# Talvez nao gere problemas o nivel de conhecimento de ingles
unique(latam_dei$Q2.Recoded..What.is.your.preferred.language. )
prop.table(table(latam_dei$Q2.Recoded..What.is.your.preferred.language.))
unique(latam_dei$Q3..How.well.can.you.read.and.write.in.English.)
prop.table(table(latam_dei$Q3..How.well.can.you.read.and.write.in.English.))
unique(latam_dei$Q4..Has.the.general.preference.for.use.of.English.in.open.source.negatively.affected.your.ability.to.participate. )
prop.table(table(latam_dei$Q4..Has.the.general.preference.for.use.of.English.in.open.source.negatively.affected.your.ability.to.participate.))
