library(tidyverse)
library(lme4)
library(lmerTest)
data <- read.csv("/Users/semmeijer/Downloads/Ecology&Conservation/Flycatcher_Hybridization/Data/database_preferences.csv") |>
  mutate(patch_h = as.numeric(patch_h),
         patch_b = as.numeric(patch_b))
view(data)
str(data)
unique(data$species)

data <- data |> filter(species == "PF" | species == "CF")

#count amount of ring_nb with multiple distinct species entries
data |>
  group_by(ring_nb) %>%
  summarise(n_distinct_species = n_distinct(species)) %>%
  filter(n_distinct_species > 1)
#results show 16 birds "switching" species, excluding them for now
data <- data |> group_by(ring_nb) %>%
  filter(n_distinct(species) == 1)

Hybrid_data <- data |> group_by(yearAreaBox) |>
  filter(n_distinct(species) > 1) |>
  filter(nestbox!="NANA")
view(Hybrid_data)
#### 669 entries,  <335 hybrid pairs ####

#count ring_nb with more than 1 entry
Hybrid_data_multiple <- Hybrid_data |>
  group_by(ring_nb) %>%
  mutate(n_entries = n()) %>%
  filter(n_entries > 1)
view(Hybrid_data_multiple)
### 67 entries more than 1 times hybridization

#add column for each entry that is hybrid and then merge it back to the full dataset
Hybrid_data <- Hybrid_data |>
  mutate(hybridized = 1)

data <- data |> left_join(Hybrid_data |> select(ring_nb, yearAreaBox, hybridized), by = c("ring_nb", "yearAreaBox")) |>
  mutate(hybridized = ifelse(is.na(hybridized), 0, hybridized)) |> 
  mutate(patchsize = patch_h*patch_b) 
view(data)
data <- data |>
  filter(patchsize < 300) #remove entries with patchsize 0, which are likely errors in the data

ggplot(subset(data,sex=="male"), aes(x=factor(hybridized),y=patchsize)) +
  geom_boxplot() +
  labs(x="Hybridization status", y="Patch size", title="Patch size vs Hybridization status") +
  theme_minimal()

#histogram of male pathsizes
hist(data$patchsize[data$sex=="male"])

m1 <- lmer(patchsize ~ hybridized + (1|ring_nb),data=subset(data,sex=="male"))
summary(m1)

m2 <- lm(patchsize ~ hybridized,data=subset(data,sex=="male"))
par(mfrow=c(2,2))
plot(m2)
