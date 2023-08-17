Egg Cooking Data Analysis
================

# Introduction

This document presents a detailed analysis of egg cooking data. We will
read the data, preprocess it, define contrasts for comparison, perform
two-way ANOVA, and carry out Tukey’s HSD post hoc test. We’ll also make
adjustments for multiple comparisons using the Bonferroni correction.

``` r
#Load the libraries required for our statistical analysis
library(stats)
```

``` r
# Convert 'Treatment', 'Salted', and 'Temperature' columns to factors
data$Treatment <- interaction(data$Temperature, data$Salted, sep="_")
data$Treatment <- as.factor(data$Treatment)
data$Salted <- as.factor(data$Salted)
data$Temperature <- as.factor(data$Temperature)
```

``` r
# Define contrasts for specific comparisons
contr_matrix <- matrix(c(-1, 0, 0, 1, 1/3, 1/3, 1/3, -1), nrow=4)
rownames(contr_matrix) <- levels(data$Treatment)
colnames(contr_matrix) <- c("compare_20_1_vs_10_0", "compare_20_1_vs_others")
contr_matrix
```

    ##      compare_20_1_vs_10_0 compare_20_1_vs_others
    ## 10_0                   -1              0.3333333
    ## 20_0                    0              0.3333333
    ## 10_1                    0              0.3333333
    ## 20_1                    1             -1.0000000

``` r
# Assign contrasts
contrasts(data$Treatment) <- contr_matrix
```

``` r
# Run a two-way ANOVA on the interaction effect
interaction_model <- aov(Egg_Temperature ~ Salted*Temperature, data = data)
summary(interaction_model)
```

    ##                    Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Salted              1 102.08  102.08  16.150 0.000285 ***
    ## Temperature         1 283.56  283.56  44.860 8.21e-08 ***
    ## Salted:Temperature  1   0.03    0.03   0.005 0.945230    
    ## Residuals          36 227.55    6.32                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Run a two-way ANOVA on the additive model
additive_model <- aov(Egg_Temperature ~ Salted + Temperature, data = data)
summary(additive_model)
```

    ##             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Salted       1  102.1  102.08    16.6 0.000234 ***
    ## Temperature  1  283.6  283.56    46.1 5.41e-08 ***
    ## Residuals   37  227.6    6.15                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Fit the ANOVA model using Treatment with contrasts
treatment_model_contrast <- aov(Egg_Temperature ~ Treatment, data = data)
```

``` r
# Tukey's HSD post hoc test 
tukey_results <- TukeyHSD(treatment_model_contrast)
print(tukey_results)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Egg_Temperature ~ Treatment, data = data)
    ## 
    ## $Treatment
    ##            diff        lwr        upr     p adj
    ## 20_0-10_0  5.38  2.3518456  8.4081544 0.0001639
    ## 10_1-10_0  3.25  0.2218456  6.2781544 0.0314391
    ## 20_1-10_0  8.52  5.4918456 11.5481544 0.0000000
    ## 10_1-20_0 -2.13 -5.1581544  0.8981544 0.2483824
    ## 20_1-20_0  3.14  0.1118456  6.1681544 0.0396852
    ## 20_1-10_1  5.27  2.2418456  8.2981544 0.0002198

``` r
# Extract summary of contrast model
summary_data <- summary.lm(treatment_model_contrast)
contrast_summary_data <- summary_data$coefficients
```

``` r
# Extract p-values for the contrasts
pvalue_contrast1 <- contrast_summary_data["Treatmentcompare_20_1_vs_10_0", "Pr(>|t|)"]
pvalue_contrast2 <- contrast_summary_data["Treatmentcompare_20_1_vs_others", "Pr(>|t|)"]
```

``` r
# Adjust the p-values using Bonferroni correction with n = 2 independent tests
adjusted_pvalue_contrast1 <- p.adjust(pvalue_contrast1, method = "bonferroni", n = 2)
adjusted_pvalue_contrast2 <- p.adjust(pvalue_contrast2, method = "bonferroni", n = 2)
```

``` r
# Print the adjusted p-values
cat("Adjusted p-value for comparison of average internal temperatures of treatment 4 vs. treatment 1:", adjusted_pvalue_contrast1, "\n")
```

    ## Adjusted p-value for comparison of average internal temperatures of treatment 4 vs. treatment 1: 0.0001681797

``` r
cat("Adjusted p-value for comparison of average internal temperatures of treatment 4 vs. others:", adjusted_pvalue_contrast2, "\n")
```

    ## Adjusted p-value for comparison of average internal temperatures of treatment 4 vs. others: 1
