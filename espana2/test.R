library(tidyverse)
covid_cases_deaths <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/covid_california.csv")
ccd <- covid_cases_deaths
view(ccd)

colnames(ccd)

head(ccd)


ccd |>
  select(pfizer_doses_shipped) |>
  ggplot(aes(x = pfizer_doses_shipped)) +
  geom_histogram(bins = 30) 
  theme_light()

  
ccd |>
  select(moderna_doses_shipped) |>
  ggplot(aes(moderna_doses_shipped)) +
  geom_histogram(bins = 30) 
  theme_light()
  
ccd |>
  select(jj_doses_shipped) |>
  ggplot(aes(jj_doses_shipped)) +
  geom_histogram(bins = 30) 
  theme_light()
  
  
library(dplyr)
library(tidyr)
library(ggplot2)

  ccd_long <- ccd |>
    select(pfizer_doses_shipped, moderna_doses_shipped, jj_doses_shipped) |>
    pivot_longer(
      cols = everything(),
      names_to = "vaccine",
      values_to = "doses_shipped"
    )

  ggplot(ccd_long, aes(x = doses_shipped)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "ite") +
    facet_wrap(~ vaccine, scales = "free") +
    theme_light() +
    labs(title = "Distribution of Doses Shipped by Vaccine Type", x = "Doses Shipped", y = "Count")
  
  
unique(ccd$county)

colnames(cdd)



library(dplyr)
library(tidyr)
library(ggplot2)

# Calculate proportions per county
ccd_prop <- ccd |>
  group_by(county) |>
  summarise(
    pfizer = sum(pfizer_doses_shipped, na.rm = TRUE),
    moderna = sum(moderna_doses_shipped, na.rm = TRUE),
    jj = sum(jj_doses_shipped, na.rm = TRUE)
  ) |>
  mutate(total = pfizer + moderna + jj) |>
  mutate(
    pfizer = pfizer / total,
    moderna = moderna / total,
    jj = jj / total
  ) |>
  select(-total) |>
  pivot_longer(cols = c(pfizer, moderna, jj), names_to = "vaccine", values_to = "proportion") |>
  mutate(county = factor(county, levels = rev(sort(unique(county)))))


# Plot
ggplot(ccd_prop, aes(x = county, y = proportion, fill = vaccine)) +
  geom_col(position = "stack") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Proportion of Vaccine Doses Shipped by County",
    x = "County",
    y = "Proportion of Total Doses",
    fill = "Vaccine Type"
  ) +
  scale_fill_manual(values = c(
    pfizer = "darkblue",
    moderna = "darkred",
    jj = "darkgrey"
  )) 
 
### link to why rural counties didnt like phzier: https://www.news-medical.net/news/20210208/Californiae28099s-smallest-county-makes-big-vaccination-gains.aspx

# top 5: Los Angeles, San Diego, Organe, Riverside, San Bernanrdino
# bottom 5: Alpine, Sierra, Modoc , Mono, Trinity

###

# Define target counties
top5 <- c("Los Angeles", "San Diego", "Orange", "Riverside", "San Bernardino")
bottom5 <- c("Alpine", "Sierra", "Modoc", "Mono", "Trinity")
target_counties <- c(top5, bottom5)

# Filter and calculate proportions for only top/bottom counties
ccd_prop <- ccd |>
  filter(county %in% target_counties) |>
  group_by(county) |>
  summarise(
    pfizer = sum(pfizer_doses_shipped, na.rm = TRUE),
    moderna = sum(moderna_doses_shipped, na.rm = TRUE),
    jj = sum(jj_doses_shipped, na.rm = TRUE)
  ) |>
  mutate(total = pfizer + moderna + jj) |>
  mutate(
    pfizer = pfizer / total,
    moderna = moderna / total,
    jj = jj / total
  ) |>
  select(-total) |>
  pivot_longer(cols = c(pfizer, moderna, jj), names_to = "vaccine", values_to = "proportion") |>
  mutate(county = factor(county, levels = rev(target_counties)))  # keep desired order

# Plot
ggplot(ccd_prop, aes(x = county, y = proportion, fill = vaccine)) +
  geom_col(position = "stack") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Proportion of Vaccine Doses Shipped (Top 5 vs. Bottom 5 Counties by Population)",
    x = "County",
    y = "Proportion of Total Doses",
    fill = "Vaccine Type"
  ) +
  scale_fill_manual(values = c("pfizer" = "darkblue", 
                               "moderna" = "firebrick", 
                               "jj" = "#FF9999"))                        

ggplot(ccd, aes(x = first_date, y = doses_shipped, color = county)) +
  geom_line() +
  labs(title = "Weekly Vaccine Shipments by County", x = "Week", y = "Doses Shipped") +
  theme_minimal()

########### Custering Analysis 
library(tidyverse)
library(factoextra)
library(ggthemes)
library(patchwork)
library(ggrepel)

###########################
### Clustering Analysis ###
###########################

# Step 1: Proportions by county
vax_proportions <- ccd |>
  group_by(county) |>
  summarise(
    pfizer_prop = max(cumulative_pfizer_doses_delivered, na.rm = TRUE) / 
      max(cumulative_doses_delivered, na.rm = TRUE),
    moderna_prop = max(cumulative_moderna_doses_delivered, na.rm = TRUE) / 
      max(cumulative_doses_delivered, na.rm = TRUE),
    jj_prop = max(cumulative_jj_doses_delivered, na.rm = TRUE) / 
      max(cumulative_doses_delivered, na.rm = TRUE)
  ) |>
  drop_na()

# Step 2: Standardize the proportions
std_vax_proportions <- vax_proportions |>
  select(-county) |>
  scale(center = TRUE, scale = TRUE)

# Step 3: Perform clustering
set.seed(123)
kmeans_result <- kmeans(std_vax_proportions,
                        centers = 3,  # Change as needed
                        algorithm = "Hartigan-Wong",
                        nstart = 1000)

# Step 4: Add clusters
vax_clustered <- vax_proportions |>
  mutate(cluster = as.factor(kmeans_result$cluster))

############################
### Cluster Scatter Plot ###
############################

cluster_plot <- fviz_cluster(kmeans_result,
                             data = std_vax_proportions,
                             geom = c("point", "text"),
                             repel = TRUE,
                             ellipse.type = "norm",
                             ggtheme = theme_minimal(base_size = 12)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(title = "California County Vaccine Distribution Patterns",
       subtitle = "Clustered by Pfizer, Moderna, and J&J vaccine delivery proportions",
       x = "Dimension 1", y = "Dimension 2", color = "Cluster") +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10),
        legend.position = "bottom")

#############################
### Cluster Profile Plot ###
#############################

centers_df <- as.data.frame(kmeans_result$centers)
centers_df$cluster <- factor(1:nrow(centers_df))
centers_long <- centers_df |>
  pivot_longer(cols = -cluster, names_to = "vaccine", values_to = "z_score")

profile_plot <- ggplot(centers_long, aes(x = vaccine, y = z_score, color = cluster, group = cluster)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(title = "Cluster Profiles",
       subtitle = "Standardized mean vaccine proportions by cluster",
       x = "Vaccine Type", y = "Standardized Proportion Score",
       color = "Cluster") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom")

##########################
### Combine and Save ###
##########################

final_plot <- cluster_plot + profile_plot +
  plot_layout(ncol = 1) +
  plot_annotation(
    title = "COVID-19 Vaccine Distribution Clusters in California Counties",
    subtitle = "Clustered by relative proportions of Pfizer, Moderna, and J&J vaccine deliveries",
    caption = "Data: California Department of Public Health | Analysis: Your Name",
    theme = theme(plot.title = element_text(size = 16, face = "bold"),
                  plot.subtitle = element_text(size = 12))
  )

# Display
final_plot

# Save
ggsave("vaccine_clusters.png", final_plot, width = 10, height = 12, dpi = 300)



########################

#### old clusting 

#reclean
vax_features <- ccd |>
  group_by(county) |>
  summarise(
    pfizer_prop = max(cumulative_pfizer_doses_delivered, na.rm = TRUE) / 
      max(cumulative_doses_delivered, na.rm = TRUE),
    moderna_prop = max(cumulative_moderna_doses_delivered, na.rm = TRUE) / 
      max(cumulative_doses_delivered, na.rm = TRUE),
    jj_prop = max(cumulative_jj_doses_delivered, na.rm = TRUE) / 
      max(cumulative_doses_delivered, na.rm = TRUE)
  ) |>
  drop_na()


#Stand
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
