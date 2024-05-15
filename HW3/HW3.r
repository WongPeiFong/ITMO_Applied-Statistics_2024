library(readxl)
lizard_data <- read_excel("C:\\Users\\po333\\Downloads\\Sleepy lizard.xlsx", sheet = "data")
# To compare the blood composition between lizards from unmodified and heavily modified landscapes, should perform a series of t-tests or non-parametric tests for each blood characteristic (Tot_WBC, Het_ABS, Lym_ABS, H:L Ratio, Mon_ABS, OthG_ABS) between the two landscape types.
unmodified_data <- subset(lizard_data, Treatment == 1)
modified_data <- subset(lizard_data, Treatment == 2)
blood_characteristics <- c("Tot_WBC", "Het_ABS", "Lym_ABS", "H:L Ratio", "Mon_ABS", "OthG_ABS")

t_test_results <- lapply(blood_characteristics, function(char) {
  t_test_result <- t.test(unmodified_data[[char]], modified_data[[char]])
  return(t_test_result)
})

names(t_test_results) <- blood_characteristics
t_test_results
# a. Yes

# conduct an analysis of variance (ANOVA) to test if there are significant differences in blood composition among different habitats within the highly modified landscape.
habitat_anova_results <- lapply(blood_characteristics, function(char) {
  aov_result <- aov(lizard_data[[char]] ~ Habitat, data = modified_data)
  return(summary(aov_result))
})
names(habitat_anova_results) <- blood_characteristics
habitat_anova_results

connectivity_anova_results <- lapply(blood_characteristics, function(char) {
  aov_result <- aov(lizard_data[[char]] ~ Connectivity, data = modified_data)
  return(summary(aov_result))
})

names(connectivity_anova_results) <- blood_characteristics
connectivity_anova_results

manova_result <- manova(cbind(Tot_WBC, Het_ABS, Lym_ABS, `H:L Ratio`, Mon_ABS, OthG_ABS) ~ Habitat * Connectivity, data = modified_data)
summary(manova_result)
