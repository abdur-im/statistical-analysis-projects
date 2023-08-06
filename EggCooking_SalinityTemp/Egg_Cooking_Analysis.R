# Load necessary library
library(stats)

# Load the data
data <- read.csv('EggCookingData_Revised.csv')

# Convert 'Treatment', 'Salted', and 'Temperature' columns to factors
data$Treatment <- interaction(data$Temperature, data$Salted, sep="_")
data$Treatment <- as.factor(data$Treatment)
data$Salted <- as.factor(data$Salted)
data$Temperature <- as.factor(data$Temperature)

# Define contrasts for specific comparisons
contr_matrix <- matrix(c(-1, 0, 0, 1, 1/3, 1/3, 1/3, -1), nrow=4)
rownames(contr_matrix) <- levels(data$Treatment)
colnames(contr_matrix) <- c("compare_20_1_vs_10_0", "compare_20_1_vs_others")

# Assign contrasts
contrasts(data$Treatment) <- contr_matrix

# Run a two-way ANOVA on the interaction effect
interaction_model <- aov(Egg_Temperature ~ Salted*Temperature, data = data)
summary(interaction_model)

# If there's no significant interaction (p-value > 0.05), run two-way ANOVA on the additive model
additive_model <- aov(Egg_Temperature ~ Salted + Temperature, data = data)
summary(additive_model)

# Fit the ANOVA model using Treatment with contrasts
treatment_model_contrast <- aov(Egg_Temperature ~ Treatment, data = data)

# Tukey's HSD post hoc test 
tukey_results <- TukeyHSD(treatment_model_contrast)
print(tukey_results)

# Extract summary of contrast model
summary_data <- summary.lm(treatment_model_contrast)
contrast_summary_data <- summary_data$coefficients

# Extract p-values for the contrasts
pvalue_contrast1 <- contrast_summary_data["Treatmentcompare_20_1_vs_10_0", "Pr(>|t|)"]
pvalue_contrast2 <- contrast_summary_data["Treatmentcompare_20_1_vs_others", "Pr(>|t|)"]

# Adjust the p-values using Bonferroni correction with n = 2 independent tests
adjusted_pvalue_contrast1 <- p.adjust(pvalue_contrast1, method = "bonferroni", n = 2)
adjusted_pvalue_contrast2 <- p.adjust(pvalue_contrast2, method = "bonferroni", n = 2)

# Print the adjusted p-values
cat("Adjusted p-value for comparison of average internal temperatures of treatment 4 vs. treatment 1:", adjusted_pvalue_contrast1, "\n")
cat("Adjusted p-value for comparison of average internal temperatures of treatment 4 vs. others:", adjusted_pvalue_contrast2, "\n")

