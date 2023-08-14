# Analysis of Smartphone Usage and Mental Health

# Libraries
library(ggplot2)
library(ltm) # for cronbach.alpha
library(BSDA)
library(psych)

# Data Processing

# Load the data
dataset <- read.csv('c://FALL22/STA304/project/dataset.csv')

# High Stress/Anxiety => Bad
# High Q of sleep | High social interactions => Good

# Reverse Stress/Anxiety scores
dataset$Inversed.Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7. <- dataset$Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7.
dataset$Inversed.Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7. <- dataset$Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7.
dataset$Inversed.Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7.[dataset$Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7. == 7] <- 1
dataset$Inversed.Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7.[dataset$Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7. == 6] <- 2
dataset$Inversed.Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7.[dataset$Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7. == 5] <- 3
dataset$Inversed.Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7.[dataset$Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7. == 4] <- 4
dataset$Inversed.Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7.[dataset$Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7. == 3] <- 5
dataset$Inversed.Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7.[dataset$Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7. == 2] <- 6
dataset$Inversed.Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7.[dataset$Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7. == 1] <- 7
dataset$Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7.
dataset$Inversed.Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7.
dataset$Inversed.Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7.[dataset$Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7. == 7] <- 1
dataset$Inversed.Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7.[dataset$Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7. == 6] <- 2
dataset$Inversed.Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7.[dataset$Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7. == 5] <- 3
dataset$Inversed.Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7.[dataset$Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7. == 4] <- 4
dataset$Inversed.Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7.[dataset$Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7. == 3] <- 5
dataset$Inversed.Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7.[dataset$Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7. == 2] <- 6
dataset$Inversed.Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7.[dataset$Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7. == 1] <- 7
dataset$Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7.
dataset$Inversed.Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7.

# Create mental health score column
dataset$mental.health.score.before.COVID.19 <-
  dataset$Inversed.Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7. + dataset$Quality.Of.Sleep.Before.COVID.19..scale.from.1.7. +
  dataset$Rating.Social.Interactions.Before.COVID.19..scale.from.1.7.
dataset$mental.health.score.during.COVID.19 <-
  dataset$Inversed.Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7. + dataset$Quality.Of.Sleep.During.COVID.19..scale.from.1.7. +
  dataset$Rating.Social.Interactions.During.COVID.19..scale.from.1.7.

# Assumptions

# Normality test on smartphone diff 
smartphone_differences =
  abs(dataset$Hours.Spent.Daily.On.Smartphone.Before.COVID.19..0.8.hours. -
        dataset$Hours.Spent.Daily.On.Smartphone.During.COVID.19..0.8.hours.)
shapiro.test(smartphone_differences)

# Normality test on mental health diff
mental_differences = abs(dataset$mental.health.score.before.COVID.19 -
                           dataset$mental.health.score.during.COVID.19)
shapiro.test(mental_differences)

# Graphs for differences distribution
smartphone_before=dataset$Hours.Spent.Daily.On.Smartphone.Before.COVID.19..0.8.hours.
smartphone_during=dataset$Hours.Spent.Daily.On.Smartphone.During.COVID.19..0.8.hours.
differences = abs(smartphone_before-smartphone_during)
hist(differences, breaks="Scott", main="Smartphone Usage Differences")

differences = abs(dataset$mental.health.score.before.COVID.19 -
                    dataset$mental.health.score.during.COVID.19)
hist(differences, breaks="Scott", main="Mental Health Score Differences")

# Paired-Sign Test for Hypothesis

# Paired sign test for pairs diff means (two-sided, !=)
SIGN.test(x =
            dataset$Hours.Spent.Daily.On.Smartphone.Before.COVID.19..0.8.hours.,
          y =
            dataset$Hours.Spent.Daily.On.Smartphone.During.COVID.19..0.8.hours.,
          alternative = "two.sided",
          conf.level = 0.95)
SIGN.test(x = dataset$mental.health.score.before.COVID.19,
          y = dataset$mental.health.score.during.COVID.19,
          alternative = "two.sided",
          conf.level = 0.95)

# Graphs

# mental health score before covid boxplot per gender
g_box_mental_bfr<- ggplot(dataset, aes(x=Gender,
                                       y=mental.health.score.before.COVID.19, fill=Gender)) +
  geom_boxplot(lwd=1) +
  ylab("Mental Health Score before Covid-19") +
  xlab("Gender") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 9.5)) +
  theme(axis.text.y = element_text(size = 9.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Mental Health Score before Covid-19 per Gender")
g_box_mental_bfr

# mental health score during covid per gender
g_box_mental_dur <- ggplot(dataset, aes(x=Gender,
                                        y=mental.health.score.during.COVID.19, fill=Gender)) +
  geom_boxplot(lwd=1) +
  theme(legend.position = "none") +
  labs(color = "Gender") +
  ylab("Mental Health Score during Covid-19") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Mental Health Score during Covid-19 per Gender") +
  xlab("Gender") +
  theme(axis.text.x = element_text(size = 9.5)) +
  theme(axis.text.y = element_text(size = 9.5))
g_box_mental_dur

# smartphone hours before covid per gender boxplot
g_box_smart_bfr <- ggplot(dataset, aes(x= Gender, y =
                                         Hours.Spent.Daily.On.Smartphone.Before.COVID.19..0.8.hours., fill=Gender)) +
  geom_boxplot(lwd=1) + ylim(c(0,9)) +
  theme(legend.position = "none") +
  labs(color = "Gender") + ylab("Smartphone Usage - Hours Spent Daily (hrs)
Before COVID") +
  xlab("Gender") +
  theme(axis.text.x = element_text(size = 9.5)) +
  theme(axis.text.y = element_text(size = 9.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Smartphone Usage before Covid-19 per Gender")
g_box_smart_bfr

# smartphone hours during covid per gender boxplot
g_box_smart_dur <- ggplot(dataset, aes(x= Gender, y =
                                         Hours.Spent.Daily.On.Smartphone.During.COVID.19..0.8.hours., fill =
                                         Gender)) + geom_boxplot(lwd=1) + scale_fill_discrete("Gender",
                                                                                              labels=c('Female', 'Male')) + labs(color = "Gender") + ylab("Smartphone
Usage - Hours Spent Daily (hrs) During COVID") +
  xlab("Gender") + theme(axis.text.x = element_text(size = 9.5)) +
  theme(axis.text.y = element_text(size = 9.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  ggtitle("Smartphone Usage during Covid-19 per Gender")

g_box_smart_dur

# Advanced Methodologies

# Getting cronbach alpha of multi q's for mental health before covid
mental_health_qs_before_covid <- data.frame(
  stress_before_covid=dataset$Inversed.Stress.and.Anxiety.Level.Overall.Before.COVID.19..scale.from.1.7.,q_of_sleep_before_covid=dataset$Quality.Of.Sleep.Before.COVID.19..scale.from.1.7.,social_before_covid=dataset$Rating.Social.Interactions.Before.COVID.19..scale.from.1.7.
)
cronbach.alpha(mental_health_qs_before_covid)

# Getting cronbach alpha of multi q's for mental health during covid
mental_health_qs_during_covid <- data.frame(
  stress_before_covid=dataset$Inversed.Stress.and.Anxiety.Level.Overall.During.COVID.19..scale.from.1.7.,
  q_of_sleep_before_covid=dataset$Quality.Of.Sleep.During.COVID.19..scale.from.1.7.,social_before_covid=dataset$Rating.Social.Interactions.During.COVID.19..scale.from.1.7.
)
cronbach.alpha(mental_health_qs_during_covid)

