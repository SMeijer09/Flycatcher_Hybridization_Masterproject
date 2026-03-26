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
  group_by(ring_nb) |>
  summarise(n_distinct_species = n_distinct(species)) |>
  filter(n_distinct_species > 1)
#results show 16 birds "switching" species, excluding them for now
data <- data |> group_by(ring_nb) |>
  filter(n_distinct(species) == 1)

Hybrid_data <- data |> group_by(yearAreaBox) |>
  filter(n_distinct(species) > 1) |>
  filter(nestbox!="NANA")
view(Hybrid_data)
#### 669 entries,  <335 hybrid pairs ####

#count ring_nb with more than 1 entry
Hybrid_data_multiple <- Hybrid_data |>
  group_by(ring_nb) |>
  mutate(n_entries = n()) |>
  filter(n_entries > 1)
view(Hybrid_data_multiple)
### 67 entries more than 1 times hybridization

#add column for each entry that is hybrid and then merge it back to the full dataset
Hybrid_data <- Hybrid_data |>
  mutate(hybridnest = 1)

data <- data |> left_join(Hybrid_data |> select(ring_nb, yearAreaBox, hybridnest), by = c("ring_nb", "yearAreaBox")) |>
  mutate(hybridnest = ifelse(is.na(hybridnest), 0, hybridnest)) |> 
  mutate(patchsize = patch_h*patch_b) 
view(data)
data <- data |>
  filter(patchsize < 300) #remove entries with patchsize 0, which are likely errors in the data

ggplot(subset(data,sex=="male"), aes(x=factor(hybridnest),y=patchsize)) +
  geom_boxplot() +
  labs(x="Hybridization status", y="Patch size", title="Patch size vs Hybridization status") +
  theme_minimal()

#histogram of male pathsizes
hist(data$patchsize[data$sex=="male"])

m1 <- lmer(patchsize ~ hybridnest + (1|ring_nb),data=subset(data,sex=="male"))

m2 <- lm(patchsize ~ hybridnest,data=subset(data,sex=="male"))
par(mfrow=c(2,2))
plot(m2)

#filter out hybrid pairs with female collared flycatchers
data1 <- read.csv("/Users/semmeijer/Downloads/Ecology&Conservation/Flycatcher_Hybridization/Data/database_preferences.csv") |>
  mutate(patch_h = as.numeric(patch_h),
         patch_b = as.numeric(patch_b),
         tail = as.numeric(tail),
         tarsus = as.numeric(tarsus),
         wing = as.numeric(wing),
         mass = as.numeric(mass),
         beak = as.numeric(beak)) |>
  filter(species == "PF" | species == "CF") |>  


view(data1)
str(data1)

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
view(data1_clean)


###### some random plots to check the data ######
plotdata <- data1_clean |>
  filter(tarsus<40 & tarsus>2,
         tail>0 & tail<90,
         mass>0 & mass<100,
         beak<30)
hist(plotdata$tarsus,breaks=100)
hist(plotdata$tail,breaks=100)
hist(plotdata$wing,breaks=100)
hist(plotdata$mass,breaks=500)
hist(plotdata$beak,breaks=500)

boxplot(plotdata$mass ~ plotdata$sex,outline=FALSE)
boxplot(plotdata$beak ~ plotdata$sex)

boxplot(plotdata$mass ~ plotdata$species,outline=FALSE)
boxplot(plotdata$beak ~ plotdata$species)

boxplot(plotdata$fledge_nb ~ plotdata$hybridnest,outline=FALSE)
boxplot(plotdata$fledge_nb ~ plotdata$species)
boxplot(plotdata$fledge_nb ~ plotdata$sex)

ggplot(plotdata, aes(x=mass, y=beak, color=species)) +
  geom_point() +
  theme_minimal() +
  labs(title="Mass vs Beak length by species", x="Mass (g)", y="Beak length (mm)")

ggplot(plotdata, aes(x=tarsus, y=tail, color=species)) +
  geom_point() +
  theme_minimal() +
  labs(title="Tarsus length vs Tail length by species", x="Tarsus length (mm)", y="Tail length (mm)")

ggplot(plotdata, aes(x=wing, y=beak, color=species)) +
  geom_point() +
  theme_minimal() +
  labs(title="Wing length vs Beak length by species", x="Wing length (mm)", y="Beak length (mm)")
