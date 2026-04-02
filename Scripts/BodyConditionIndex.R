library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)
library(car)

data1 <- read.csv("/Users/semmeijer/Downloads/Ecology&Conservation/Flycatcher_Hybridization/Data/database_preferences.csv") |>
  mutate(patch_h = as.numeric(patch_h),
         patch_b = as.numeric(patch_b),
         tail = as.numeric(tail),
         tarsus = as.numeric(tarsus),
         wing = as.numeric(wing),
         mass = as.numeric(mass),
         beak = as.numeric(beak),
         patch_size = patch_h*patch_b) |>
  filter(species == "PF" | species == "CF")

bad_birds <- data1 |>
  group_by(ring_nb) |>
  summarise(n_sexes = n_distinct(sex)) |>
  filter(n_sexes > 1) |>
  pull(ring_nb)
bad_nests <- data1 |>
  filter(ring_nb %in% bad_birds) |>
  pull(yearAreaBox) |>
  unique()
bad_nests

data1_clean <- data1 |>
  filter(!yearAreaBox %in% bad_nests) |>
  filter(nestbox != "NANA") |>
  group_by(yearAreaBox) |>
  mutate(hybridnest = ifelse(n_distinct(species) > 1, 1, 0)) |>
  mutate(n_birds=n())|>
  ungroup()

paired_male_data <- data1_clean |>
  filter(sex=="male" & n_birds==2) |>
  filter(tarsus<25,
         tail>40 & tail<60,
         beak<18,
         mass>7 & mass<17,
         wing>70)

collared_data <- paired_male_data |> filter(species=="CF")
pied_data <- paired_male_data |> filter(species=="PF")
view(collared_data)

str(collared_data)


pairs(~ mass + tarsus + wing + tail + beak, data = collared_data)
plot(log(collared_data$tarsus), log(collared_data$mass))
plot(log(collared_data$wing), log(collared_data$mass))
plot(log(collared_data$tail), log(collared_data$mass))
plot(log(collared_data$beak), log(collared_data$mass))
cor(collared_data[, c("mass", "tarsus", "wing", "tail", "beak")], use = "complete.obs")

pairs(~ mass + tarsus + wing + tail + beak, data = pied_data)
plot(log(pied_data$tarsus), log(pied_data$mass))
plot(log(pied_data$wing), log(pied_data$mass))
plot(log(pied_data$tail), log(pied_data$mass))
plot(log(pied_data$beak), log(pied_data$mass))
cor(pied_data[, c("mass", "tarsus", "wing", "tail", "beak")], use = "complete.obs")

#PCA of morphological traits
collared_pca <- prcomp(collared_data[, c("tarsus", "wing")], scale. = TRUE)
pied_pca <- prcomp(pied_data[, c("tarsus", "wing")], scale. = TRUE)
summary(collared_pca)
summary(pied_pca)

#plot log PC1 to mass
collared_data$PC1 <- collared_pca$x[, 1]
pied_data$PC1 <- pied_pca$x[, 1]
plot(log(collared_data$PC1), log(collared_data$mass))
plot(log(pied_data$PC1), log(pied_data$mass))
#no strong correlations