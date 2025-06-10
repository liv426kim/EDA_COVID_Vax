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
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
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
    title = "Proportion of Vaccine Doses Shipped (Top 5 vs. Bottom 5 Counties)",
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
