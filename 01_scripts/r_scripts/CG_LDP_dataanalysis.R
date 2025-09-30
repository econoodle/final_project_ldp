# Cindy Gao
# LDP Final Project
# Study question: How do penguin traits vary across the islands
# of the Palmer Archipelago?
# Traits being studied: bill length (mm), flipper length (mm),
# and body mass (g)


# setup; loading packages and reading in data -----------------------------
library(tidyverse)
library(emmeans)

penguins <- read.csv('00_rawdata/CG_LDP_penguins_1.csv')

# exploratory data analysis -----------------------------------------------
# taking a glimpse at the mean trait values for each island
penguins |>
  group_by(island) |>
  summarise(
    count = n(),
    mean_bill_length = mean(bill_length_mm, na.rm = T),
    mean_flipper_length = mean(flipper_length_mm, na.rm = T),
    mean_body_mass = mean(body_mass_g, na.rm = T)
  )

# examining the distribution of bill length 
# for each island
penguins |>
  ggplot(aes(x = bill_length_mm)) +
  geom_histogram() +
  facet_wrap(~island)

# examining the distribution of flipper length
# for each island
penguins |>
  ggplot(aes(x = flipper_length_mm)) +
  geom_histogram() +
  facet_wrap(~island)

# examining the distribution of body mass
# for each island
penguins |>
  ggplot(aes(x = body_mass_g)) +
  geom_histogram() +
  facet_wrap(~island)

# data preparation --------------------------------------------------------

# getting rid of NAs and bill depth measurements
# i am not examining bill depth for this study
penguins_final <- penguins |>
  drop_na() |>
  select(-bill_depth_mm)

# writing cleaned dataset to outputs folder
write.csv(penguins_final, '02_outdata/CG_LDP_cleanedpenguins_1.csv', row.names = F)

# assessing for differences in mean trait value across islands ------------

# my first question is: Is there a significant difference in mean trait value
# across the islands?

# using ANOVA to test for differences in mean flipper length across islands
flipper_length_anova <- aov(flipper_length_mm ~ island, data = penguins_final)
summary(flipper_length_anova)

# using ANOVA to test for differences in mean bill length across islands
bill_length_anova <- aov(bill_length_mm ~ island, data = penguins_final)
summary(bill_length_anova)

# using ANOVA to test for differences in mean body mass across islands
body_mass_anova <- aov(body_mass_g ~ island, data = penguins_final)
summary(body_mass_anova)

# from the summaries, it seems that all traits exhibit significant differences
# in mean trait value across the islands
# i'll identify important pairwise comparisons using a Tukey Test

flipper_length_tukey <- emmeans(flipper_length_anova, 'island')
pairs(flipper_length_tukey)
# for flipper length, the significant pairwise comparisons are biscoe and dream island
# as well as biscoe and torgersen island

bill_length_tukey <- emmeans(bill_length_anova, 'island')
pairs(bill_length_tukey)
# for bill length, the significant pairwise comparisons are biscoe and torgersen,
# as well as dream and torgersen

body_mass_tukey <- emmeans(body_mass_anova, 'island')
pairs(body_mass_tukey)
# for body mass, the significant pairwise comparisons are biscoe and dream,
# as well as biscoe and torgersen


# assessing differences in trait-trait relationships across islands -------
# my second question is: Do trait-trait relationships vary across the islands?

# testing if the relationship between flipper length and bill length changes
# across the islands
flipper_bill_model <- lm(flipper_length_mm ~ bill_length_mm * island, data = penguins_final)
summary(flipper_bill_model)

# examining pairwise comparisons of slope to see which pairs of islands are significantly
# different using a Tukey Test
slopes_flipper_bill <- emtrends(flipper_bill_model, ~ island, var = "bill_length_mm")
pairs(slopes_flipper_bill)
# biscoe and dream, as well as biscoe and torgersen have significant differences in slope
# for the flipper length x bill length model


# testing if the relationship between flipper length and body mass changes 
# across the islands
flipper_body_model <- lm(flipper_length_mm ~ body_mass_g * island, data = penguins_final)
summary(flipper_body_model)

# examining pairwise comparisons of slope to see which pairs of islands are significantly
# different using a Tukey Test
slopes_flipper_body <- emtrends(flipper_body_model, ~ island, var = 'body_mass_g')
pairs(slopes_flipper_body)
# biscoe and dream, as well as biscoe and torgersen have significant differences in slope
# for the flipper length x body mass model

