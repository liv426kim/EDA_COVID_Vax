Olivia Kim File


library(tidyverse)
covid_cases_deaths <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/covid_california.csv")

ccd <- covid_cases_deaths

summarize(ccd)
summary(ccd)

names(ccd)
dim(ccd)



ccd |>
  mutate(week_date = as.Date(first_date)) |>
  group_by(week_date) |>
  summarize(pfizer_rate = sum(pfizer_doses_delivered, na.rm = TRUE) / sum(pfizer_doses_shipped, na.rm = TRUE),
    moderna_rate = sum(moderna_doses_delivered, na.rm = TRUE) / sum(moderna_doses_shipped, na.rm = TRUE),
    jj_rate = sum(jj_doses_delivered, na.rm = TRUE) / sum(jj_doses_shipped, na.rm = TRUE)) |>
  pivot_longer(cols= c(pfizer_rate, moderna_rate, jj_rate),
               names_to= "brand",
               values_to= "shipping_rate") |>
  mutate(brand = recode(brand,
                        "pfizer_rate"= "Pfizer",
                        "moderna_rate"= "Moderna",
                        "jj_rate" = "Johnson & Johnson")) |>
  ggplot(aes(x = week_date, y = shipping_rate, color= brand)) +
  geom_line(size = 1) +
  labs(
    title = "COVID-19 Vaccine Shipping Rate Over Time by Brand",
    x = "Week",
    y = "Shipping Rate (Delivered / Shipped)") +
  theme_minimal()

ccd |>
  group_by(county) |>
  summarize(Pfizer = sum(pfizer_doses_delivered, na.rm = TRUE),
    Moderna = sum(moderna_doses_delivered, na.rm = TRUE),
    Johnson_Johnson = sum(jj_doses_delivered, na.rm = TRUE)) |>
  pivot_longer(cols = c(Pfizer, Moderna, Johnson_Johnson),
               names_to = "Brand",
               values_to = "Doses_Delivered")|>
  ggplot(aes(x = reorder(county, -Doses_Delivered), y = Doses_Delivered, fill = Brand)) +
  geom_col() +
  coord_flip() +
  labs(title = "Total COVID-19 Vaccine Doses Delivered by County and Brand",
    x = "County",y = "Doses Delivered") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer()

#calc proportions by county
library(dplyr)
library(tidyr)
library(ggplot2)

ccd_prop <- ccd |>
  group_by(county) |>
  summarise(
    pfizer = sum(pfizer_doses_shipped, na.rm = TRUE),
    moderna = sum(moderna_doses_shipped, na.rm = TRUE),
    jj = sum(jj_doses_shipped, na.rm = TRUE)) |>
  mutate(total = pfizer + moderna + jj) |>
  mutate(pfizer = pfizer / total,
    moderna = moderna / total,
    jj = jj / total) |>
  select(-total) |>
  pivot_longer(cols = c(pfizer, moderna, jj),
               names_to = "vaccine",
               values_to = "proportion")

ggplot(ccd_prop, aes(x = fct_rev(factor(county)), y = proportion, fill = vaccine)) +
   geom_col(position = "stack") +
  scale_fill_manual(values = c("pfizer" = "darkblue", 
                               "moderna" = "firebrick", 
                               "jj" = "#FF9999"))  +
   coord_flip() +
   theme_minimal() +
   labs(title = "Proportion of Vaccine Doses Shipped by County",
     x = "County",
     y = "Proportion of Total Doses",
     fill = "Vaccine Type")

#cluster analysis

vax_features <- ccd |>
group_by(county) |>
  summarise(
    pfizer_prop = max(cumulative_pfizer_doses_delivered, na.rm = TRUE) / 
      max(cumulative_doses_delivered, na.rm = TRUE),
    moderna_prop = max(cumulative_moderna_doses_delivered, na.rm = TRUE) / 
      max(cumulative_doses_delivered, na.rm = TRUE),
    jj_prop = max(cumulative_jj_doses_delivered, na.rm = TRUE) / 
      max(cumulative_doses_delivered, na.rm = TRUE)) |>
  drop_na()

install.packages('factoextra')
library(factoextra)

std_vax_features <- vax_features |>
  select(-county) |>
  scale(center = TRUE, scale = TRUE)

kmeans_many_features <- std_vax_features |>
  kmeans(algorithm = "Hartigan-Wong", centers = 4, nstart = 1000)

vax_clustered <- vax_features |>
  mutate(cluster = as.factor(kmeans_many_features$cluster))

fviz_cluster(kmeans_many_features,
             data = std_vax_features,
             geom = "point",
             ellipse = FALSE,
             main = "Vaccine Delivery Clusters") +
  ggthemes::scale_color_colorblind() +
  theme_light()


#lecture 7 hierarchical clustering

dist_matrix <- dist(std_vax_features, method = "euclidean")

hc <- hclust(dist_matrix, method = "ward.D2")

cluster_labels <- cutree(hc, k = 4)
vax_clustered_hc <- vax_features |>
  mutate(cluster = as.factor(cluster_labels))

#dendrogram
plot(hc, labels = vax_features$county, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "", cex = 0.6)
rect.hclust(hc, k = 4, border = 2:5)

vax_clustered_hc |>
  group_by(cluster) |>
  summarise(
    n_counties = n(),
    avg_pfizer = mean(pfizer_prop, na.rm = TRUE),
    avg_moderna = mean(moderna_prop, na.rm = TRUE),
    avg_jj = mean(jj_prop, na.rm = TRUE)  )

fviz_cluster(list(data = std_vax_features, cluster = cluster_labels),
             geom = "point",
             ellipse = FALSE,
             main = "PCA Plot of Hierarchical Clusters")


#PCA -------------------------------------------------------

# How many counties in each group
table(vax_clustered$cluster)

# cluster sum square variance ... 
kmeans_many_features$tot.withinss       
kmeans_many_features$betweenss          
kmeans_many_features$betweenss /
  (kmeans_many_features$betweenss + kmeans_many_features$tot.withinss)

scales  <- attr(std_vax_features, "scaled:scale")
centers <- sweep(kmeans_many_features$centers,
                 2,
                 scales,
                 `*`)
vax_clustered |>
  group_by(cluster) |>
  summarise(across(pfizer_prop:jj_prop, mean))

install.packages("cluster")
library(cluster)
sil <- silhouette(kmeans_many_features$cluster,
                  dist(std_vax_features))
factoextra::fviz_silhouette(sil)
mean(sil[, "sil_width"])         
#value = 0.5000 , which is >= 0.5, therefore it is a strong separation

pca <- prcomp(std_vax_features)
factoextra::fviz_pca_var(pca, repel = TRUE)

#“Four distinct county profiles emerged.
#Cluster 1 (2 counties) received more than average Moderna vaccines with low JJ and no Pfizer.
#Cluster 2 (29 counties) mostly Pfizer dominant with minimal JJ or Moderna.
#Cluster 3 (24 counties) balanced Moderna/Pfizer distribution & low JJ
#Cluster 4 (3 counties) received higher than average JJ vaccines, mostly Moderna dominant. 

table(std_clustered$cluster)

#cutree---------------------------------------

d <- dist(std_vax_features, method = "euclidean")
hc <- hclust(d, method = "ward.D2")

cluster_labels <- cutree(hc, k = 4)

vax_clustered_hc <- vax_features |>
  mutate(cluster = as.factor(cluster_labels))

vax_clustered_hc |>
  group_by(cluster) |>
  summarise(
    n_counties = n(),
    avg_pfizer = mean(pfizer_prop, na.rm = TRUE),
    avg_moderna = mean(moderna_prop, na.rm = TRUE),
    avg_jj = mean(jj_prop, na.rm = TRUE) )
