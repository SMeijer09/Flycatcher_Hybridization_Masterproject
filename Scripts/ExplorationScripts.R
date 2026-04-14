library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)
library(car)
library(emmeans)
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
         beak = as.numeric(beak),
         patch_size = patch_h*patch_b) |>
  filter(species == "PF" | species == "CF")


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
  ungroup() |> 
  mutate(
    tarsus = ifelse(tarsus <=2 | tarsus >= 50, NA, tarsus),
    tail   = ifelse(tail >= 100, NA, tail),
    beak   = ifelse(beak >= 22, NA, beak),
    mass   = ifelse(mass <= 4 | mass >= 80, NA, mass),
    wing   = ifelse(wing <= 8, NA, wing),
    sum_of_white_on_primaries = ifelse(sum_of_white_on_primaries >= 200, NA, sum_of_white_on_primaries),
    patch_size = ifelse(patch_size >= 300, NA, patch_size))
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

#filter data1_clean to only include entries with hybridnest = 1 and species PF are paired with one CF
data_hybrid <- data1_clean |>
  group_by(yearAreaBox) |>
  filter(any(sex == "female" & species == "PF")) |>
  mutate(
    pair = case_when(
      first(n_birds) == 1 ~ "unknown",
      first(n_birds) > 2 ~ "extra_pair",
      any(sex == "male" & species == "CF") ~ "fPF_mCF",
      TRUE ~ "fPF_mPF"
    ),
    pair_ID = cur_group_id()
  )

view(data_hybrid)

model_data <- data_hybrid |>
  filter(pair == "fPF_mCF" | pair == "fPF_mPF",
         sex=="male") 
view(model_data)

ggplot(model_data, aes(x=pair, y=mass)) +
  geom_boxplot() +
  labs(title="Mass by pair type", x="Pair type", y="Mass (g)") +
  facet_wrap(~age_category)

ggplot(model_data, aes(x=pair, y=patch_size)) +
  geom_boxplot() +
  labs(title="Patch Size by pair type", x="Pair type", y="Patch size") +
  facet_wrap(~age_category)

ggplot(model_data, aes(x=pair, y=sum_of_white_on_primaries)) +
  geom_boxplot() +
  labs(title="Wing patch by pair type", x="Pair type", y="Wing patch") + 
  facet_wrap(~age_category)

ggplot(model_data, aes(x=pair, y=wing)) +
  geom_boxplot() +
  labs(title="Wing length by pair type", x="Pair type", y="Wing length (mm)") +
  facet_wrap(~age_category)

ggplot(model_data, aes(x=pair, y=beak)) +
  geom_boxplot() +
  labs(title="Beak length by pair type", x="Pair type", y="Beak length (mm)") +
  facet_wrap(~age_category)

ggplot(model_data, aes(x=pair, y=tarsus)) +
  geom_boxplot() +
  labs(title="Tarsus length by pair type", x="Pair type", y="Tarsus length (mm)") +
  facet_wrap(~age_category)

p1 <- ggplot(subset(model_data,patch_size<300), aes(x=sum_of_white_on_primaries,y=patch_size,color=pair)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Wing patch vs Patch size", x="Wing patch", y="Patch size") +
  theme_minimal()

paired_male_data <- data1_clean |>
  filter(sex=="male" & n_birds==2)

p2 <- ggplot(subset(paired_male_data,patch_size<300 & sum_of_white_on_primaries<150), aes(x=sum_of_white_on_primaries,y=patch_size,color=species)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Wing patch vs Patch size", x="Wing patch", y="Patch size") +
  theme_minimal()

p1 + p2
p1
ggplot(subset(paired_male_data,mass<60), aes(x=mass,y=tail,color=species)) +
  geom_point(alpha=0.3) +
  geom_smooth(method="lm", se=FALSE) +
  theme_minimal()

ggplot(paired_male_data, aes(x=species,y=tarsus)) +
         geom_boxplot(outliers=FALSE) +
         labs(title="Mass by species", x="Species") +
         theme_minimal()

#test collinearity between the morphological traits
cor_data <- paired_male_data |>
  select(mass, beak, tarsus, tail, wing)
cor_matrix <- cor(cor_data, use = "complete.obs")
cor_matrix
pairs(cor_data)

model <- lm(mass ~ beak + tarsus + tail + wing, data = paired_male_data)
vif(model)

m1 <- glm(hybridnest ~ patch_size, data = subset(model_data,patch_size<300), family=binomial)
summary(m1)
par(mfrow=c(2,2))
plot(m1)

### test plots again###

ggplot(paired_male_data, aes(x=species, y=sum_of_white_on_primaries, color=species)) +
  geom_boxplot(outliers=FALSE)

view(model_data)

ggplot(subset(model_data,species=="CF"),aes(x=factor(age_category))) +
  geom_bar()
#count the numbers
paired_male_data |>
  group_by(species, age_category,hybridnest) |>
  summarise(n = n()) 
ggplot(paired_male_data, aes(x=factor(age_category), fill=species)) +
  geom_bar(position="dodge") +
  facet_wrap(~hybridnest) +
  labs(title="Age category distribution by species and hybridization status", x="Age category", y="Count") +
  theme_minimal()

ggplot(paired_male_data, aes(x=factor(age_category), y=sum_of_white_on_primaries)) +
  geom_boxplot() +
  facet_wrap(~species) 

ggplot(subset(paired_male_data,species=="CF"), aes(x=factor(hybridnest), y=sum_of_white_on_primaries)) +
  geom_boxplot() +
  facet_wrap(~age_category) 

ggplot(subset(paired_male_data,species=="CF"), aes(x=factor(hybridnest), y=tarsus)) +
  geom_boxplot() +
  facet_wrap(~age_category) 


wingpatch_model <- lmer(sum_of_white_on_primaries ~ hybridnest*age_category + (1|ring_nb) + (1|year),data=subset(paired_male_data,species=="CF"))
summary(wingpatch_model)
plot(residuals(wingpatch_model))
qqnorm(residuals(wingpatch_model))
qqline(residuals(wingpatch_model))
vif(wingpatch_model)

patch_size_model <- lmer(patch_size ~ age_category + (1|ring_nb) + (1|year),data=subset(paired_male_data,species=="CF"))
summary(patch_size_model)
plot(residuals(patch_size_model))
qqnorm(residuals(patch_size_model))
qqline(residuals(patch_size_model))

wing_model <- lmer(wing ~ age_category + (1|ring_nb) + (1|year),data=subset(paired_male_data,species=="CF"))
summary(wing_model)
plot(residuals(patch_size_model))
qqnorm(residuals(patch_size_model))
qqline(residuals(patch_size_model))


mass_model <- lmer(mass ~ age_category + (1|ring_nb) + (1|year),data=subset(paired_male_data,species=="CF"))
summary(mass_model)
plot(residuals(mass_model))
qqnorm(residuals(mass_model))
qqline(residuals(mass_model))

tarsus_model <- lmer(tarsus ~ age_category + (1|ring_nb) + (1|year), data=subset(paired_male_data,species=="CF"))
summary(beak_model)
plot(residuals(beak_model))
qqnorm(residuals(beak_model))
qqline(residuals(beak_model))

wingpatch_PFCF_model <- lmer(sum_of_white_on_primaries ~ species* age_category + (1|ring_nb) + (1|year),data=model_data)
summary(wingpatch_PFCF_model)
plot(residuals(wingpatch_PFCF_model))
qqnorm(residuals(wingpatch_PFCF_model))
qqline(residuals(wingpatch_PFCF_model))
vif(wingpatch_PFCF_model)

hist(log(model_data$sum_of_white_on_primaries),breaks=100)

patch_size_PFCF_model <- lmer(patch_size ~ species + age_category + (1|ring_nb) + (1|year),data=model_data)
summary(patch_size_PFCF_model)
plot(residuals(patch_size_PFCF_model))
qqnorm(residuals(patch_size_PFCF_model))
qqline(residuals(patch_size_PFCF_model))

tarsus_PFCF_model <- lmer(tarsus ~ age_category + (1|ring_nb) + (1|year),data=model_data)
summary(tarsus_PFCF_model)
plot(residuals(tarsus_PFCF_model))
qqnorm(residuals(tarsus_PFCF_model))
qqline(residuals(tarsus_PFCF_model))

beak_PFCF_model <- lmer(beak ~ age_category + (1|ring_nb) +(1|year),data=model_data)
summary(beak_PFCF_model)
plot(residuals(beak_PFCF_model))
qqnorm(residuals(beak_PFCF_model))
qqline(residuals(beak_PFCF_model))

wing_PFCF_model <- lmer(wing ~ species + age_category + (1|ring_nb) + (1|year),data=model_data)
summary(wing_PFCF_model)
plot(residuals(wing_PFCF_model))
qqnorm(residuals(wing_PFCF_model))
qqline(residuals(wing_PFCF_model))

mass_PFCF_model <- lmer(mass ~ species + age_category + (1|ring_nb)+(1|year),data=model_data)
summary(mass_PFCF_model) 
plot(residuals(mass_PFCF_model))
qqnorm(residuals(mass_PFCF_model))
qqline(residuals(mass_PFCF_model))


#Hybrid paired males that have more than 1 entry in a year, double paired?
a <- paired_male_data |> 
  group_by(ring_nb, year) |>
  mutate(n_entries = n()) |>
  filter(n_entries > 1 & hybridnest==1) 
view(a)

#### Making the plots again but adjusted for mass or tarsus - only CF ###
adj_tarsus_male_data <- paired_male_data |>
  mutate(adj.wing = wing/tarsus,
         adj.beak = beak/tarsus,
         adj.patch_size = patch_size/tarsus,
         adj.wingpatch = sum_of_white_on_primaries/tarsus)
adj_mass_male_data <- paired_male_data |>
  mutate(adj.tarsus = tarsus/mass,
         adj.beak = beak/mass,
         adj.patch_size = patch_size/mass,
         adj.wingpatch = sum_of_white_on_primaries/mass,
         adj.wing = wing/mass)

#### CF hybrid, CF Pure ######
### adjusted by tarsus length ####
ggplot(subset(adj_tarsus_male_data,species=="CF"), aes(x=factor(hybridnest), y=adj.wing)) +
  geom_boxplot() +
  facet_wrap(~age_category) 

ggplot(subset(adj_tarsus_male_data,species=="CF"), aes(x=factor(hybridnest), y=adj.beak)) +
  geom_boxplot() +
  facet_wrap(~age_category) 

ggplot(subset(adj_tarsus_male_data,species=="CF"), aes(x=factor(hybridnest), y=adj.patch_size)) +
  geom_boxplot() +
  facet_wrap(~age_category) 

ggplot(subset(adj_tarsus_male_data,species=="CF"), aes(x=factor(hybridnest), y=adj.wingpatch)) +
  geom_boxplot() +
  facet_wrap(~age_category) 

### adjusted by mass ####
ggplot(subset(adj_mass_male_data,species=="CF"), aes(x=factor(hybridnest), y=adj.wing)) +
  geom_boxplot() +
  facet_wrap(~age_category) 

ggplot(subset(adj_mass_male_data,species=="CF"), aes(x=factor(hybridnest), y=adj.beak)) +
  geom_boxplot() +
  facet_wrap(~age_category) 

ggplot(subset(adj_mass_male_data,species=="CF"), aes(x=factor(hybridnest), y=adj.tarsus)) +
  geom_boxplot() +
  facet_wrap(~age_category) 

ggplot(subset(adj_mass_male_data,species=="CF"), aes(x=factor(hybridnest), y=adj.wingpatch)) +
  geom_boxplot() +
  facet_wrap(~age_category) 

ggplot(subset(adj_mass_male_data,species=="CF"), aes(x=factor(hybridnest), y=adj.patch_size)) +
  geom_boxplot() +
  facet_wrap(~age_category) 

### adjusted by mass and tarsus, only hybrid CF and PF pure ####
adj_tarsus_model_data <- model_data |>
  mutate(adj.wing = wing/tarsus,
         adj.beak = beak/tarsus,
         adj.patch_size = patch_size/tarsus,
         adj.wingpatch = sum_of_white_on_primaries/tarsus)
adj_mass_model_data <- model_data |> 
  mutate(adj.tarsus = tarsus/mass,
         adj.beak = beak/mass,
         adj.patch_size = patch_size/mass,
         adj.wingpatch = sum_of_white_on_primaries/mass,
         adj.wing = wing/mass)

### adjusted by tarsus ###
ggplot(adj_tarsus_model_data, aes(x=species, y=adj.wing)) +
  geom_boxplot() +
  facet_wrap(~age_category)

ggplot(adj_tarsus_model_data, aes(x=species, y=adj.beak)) +
  geom_boxplot() +
  facet_wrap(~age_category)

ggplot(adj_tarsus_model_data, aes(x=species, y=adj.patch_size)) +
  geom_boxplot() +
  facet_wrap(~age_category)

ggplot(adj_tarsus_model_data, aes(x=species, y=adj.wingpatch)) +
  geom_boxplot() +
  facet_wrap(~age_category)

### adjusted by mass ###
ggplot(adj_mass_model_data, aes(x=species, y=adj.wing)) +
  geom_boxplot() +
  facet_wrap(~age_category)

ggplot(adj_mass_model_data, aes(x=species, y=adj.beak)) +
  geom_boxplot() +
  facet_wrap(~age_category)

ggplot(adj_mass_model_data, aes(x=species, y=adj.tarsus)) +
  geom_boxplot() +
  facet_wrap(~age_category)

ggplot(adj_mass_model_data, aes(x=species, y=adj.wingpatch)) +
  geom_boxplot() +
  facet_wrap(~age_category)

ggplot(adj_mass_model_data, aes(x=species, y=adj.patch_size)) +
  geom_boxplot()
  facet_wrap(~age_category)



