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

unique(latam_dei$Q50_Gender)
table(latam_dei$Q50_Gender)

# Verifying gender distribution by region
a<-table(latam_dei$Q50_Gender)
b<-table(euro_dei$Q50_Gender)
c<-table(usa_can_dei$Q50_Gender)
d<-table(world_dei$Q50_Gender)
nova<-c(d[1],c[1],b[1],a[1])
prop.table(a)

# Creating a data frame by Region x Gender
# Creating vars
Region <- c ("ALL", "USA_CAN", "EURO", "LATAM")
Man <- c (1910, 583, 705, 145)
Other <- c (102, 42, 38, 8)
Woman <- c (332, 137, 109, 11)
# Join vars to create a data frame
dei_df <- data.frame(Region, Man, Woman, Other)

# Region names -> Lines
rownames(dei_df) <- dei_df[,1]
dei_df <- dei_df[,-1]
view(dei_df)

install.packages("factoextra")
install.packages("fpc")
library(cluster) #algoritmo de cluster
library(dendextend) #compara dendogramas
library(factoextra) #algoritmo de cluster e visualizacao
library(fpc) #algoritmo de cluster e visualizacao
library(gridExtra) #para a funcao grid arrange


# Calculate distances using euclidian method
distance <- dist(dei_df, method = "euclidean")

# Cluster calculate: possible methods = "average", "single", "complete" e "ward.D"
hierarchical_cluster <- hclust(distance, method = "complete" )
hc1 <- hclust(distance, method = "single" )
hc2 <- hclust(distance, method = "average")
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
dend_list <- dendlist(dend1, dend2) 

# Dendrograma
plot(hierarchical_cluster, cex = 0.6, hang = -1)

rect.hclust(hierarchical_cluster, k=2)

plot(hc2, cex = 0.6, hang = -1)

rect.hclust(hc2, k=2)

dei_df %>% ggplot() +
  geom_point(aes(x = Region,
                 y = Woman),
             size = 3)



# plot gender info
#library(tidyverse)
ggplot(latam_dei) + 
  geom_bar(aes(x=Q50_Gender)) + 
  labs(x = "Man", y = "Count") + 
  theme_light()

ggplot(latam_dei) + 
  geom_bar(aes(x = interaction(Q50_Gender, Q53_sexual_orientation), fill = factor(Q50_Gender))) + 
  labs(x = "Man", y = "Count") + 
  theme_light()

