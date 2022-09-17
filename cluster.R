unique(latam_dei$Q50_Gender)
table(latam_dei$Q50_Gender)

# Verifying gender distribution by region (%)
a<-prop.table(table(latam_dei$Q50_Gender))
b<-prop.table(table(euro_dei$Q50_Gender))
c<-prop.table(table(usa_can_dei$Q50_Gender))
d<-prop.table(table(world_dei$Q50_Gender))

# Creating a data frame by Region x Gender
# Creating vars
Region <- c ("ALL", "USA_CAN", "EURO", "LATAM")
Man <- c(d[1],c[1],b[1],a[1])
Other <- c(d[2],c[2],b[2],a[2])
Woman <- c(d[3],c[3],b[3],a[3])
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
