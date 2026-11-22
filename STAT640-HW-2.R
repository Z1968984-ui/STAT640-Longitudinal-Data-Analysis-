install.packages("tidyr")
install.packages("lme4")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)

# Read the data
schiz <- read.table("~/OneDrive - Northern Illinois University/STAT-640-Homework-Files/schizrep1.dat", header = TRUE)

# Filter the data
schiz <- schiz %>% filter(week %in% c(0, 1, 3, 6))

# Set up for plotting
# Observed Data, All Subjects
ggplot(schiz, aes(x = week, y = imps, group = id)) +
  geom_point() +
  theme_minimal()

ggplot(schiz, aes(x = week, y = imps, group = id)) +
  geom_line(size = 1) +
  theme_minimal()

# Panel plots
ggplot(schiz, aes(x = week, y = imps, group = id)) +
  geom_line(size = 1) +
  facet_grid(group ~ sex) +
  theme_minimal()

# Means across Week by Group
# ggplot(schiz, aes(x = factor(week), y = imps, group = group)) +
#   stat_summary(fun = mean, geom = "line", size = 2) +
#   geom_point(size = 2) +
#   geom_text(stat = "summary", aes(label = ..y..), fun = mean) +
#   theme_minimal()

ggsave("plot1a_spaghetti.png", width = 8, height = 5, dpi = 300)

# Part 1(b) - Means across Week by Group and Sex
ggplot(schiz, aes(
  x = week,
  y = imps,
  color = factor(group, labels = c("Placebo (0)", "Drug (1)")),
  linetype = factor(sex, labels = c("Female (0)", "Male (1)")),
  group = interaction(group, sex)
)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(label = round(after_stat(y), 2)),
    vjust = -0.8,
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = c(0, 1, 3, 6)) +
  labs(
    title = "Mean IMPS Score Across Week by Group and Sex",
    x = "Week",
    y = "Mean IMPS",
    color = "Treatment Group",
    linetype = "Sex"
  ) +
  theme_minimal()

# END OF PART 1(b).

# Box Plots for IMPS Measures Across Time
schiz <- schiz %>% arrange(sex, group, week)
ggplot(schiz, aes(x = factor(week), y = imps)) +
  geom_boxplot() +
  facet_grid(sex ~ group) +
  theme_minimal()

# Alternative Box Plot
ggplot(schiz, aes(x = factor(week), y = imps, fill = group)) +
  geom_boxplot() +
  facet_wrap(~ sex) +
  theme_minimal()

# Estimate model
model1 <- lmer(imps ~ sex * group + week + I(week^2) + (1 | id), data = schiz)
summary(model1)

model2 <- lmer(imps ~ group + week + I(week^2) + (1 | id), data = schiz)
summary(model2)

# Collect intercept and slope by id
# randcoef <- data.frame(id = rownames(ranef(model1)$id), estimate = ranef(model1)$id[,1])
randcoef <- data.frame(id = rownames(ranef(model1)$id), estimate = ranef(model1)$id[,1])
randcoef1 <- randcoef %>% pivot_wider(names_from = id, values_from = estimate, names_prefix = "b")

# Merge estimated coefficients with raw data
schiz <- schiz %>% arrange(id, week)
schiz1 <- schiz %>%
  left_join(randcoef, by = "id") %>%
  mutate(yhat = 5.1860 + (1 - group) * 0.3621 - 0.5833 * week + 0.04245 * week^2 + b1 + b2 * week)

# Estimated Data, All Subjects
ggplot(schiz1, aes(x = week, y = yhat, group = id)) +
  geom_line(size = 1) +
  facet_wrap(~ group) +
  theme_minimal()

# Add the predictions as a new column in your original data
schiz$predicted_imps <- predict(model1)

# Generate the spaghetti plot using the predicted values
ggplot(schiz, aes(x = week, y = predicted_imps, group = id, color = as.factor(group))) +
  geom_line(alpha = 0.5) +
  labs(title = "Spaghetti Plot of Estimated IMPS by Group", x = "Week", y = "Estimated IMPS", color = "Group") +
  theme_minimal()

schiz$predicted_imps <- predict(model1)

ggplot(schiz, aes(x = week, y = predicted_imps, group = id, color = as.factor(group))) +
  geom_line(alpha = 0.5) +
  labs(title = "Spaghetti Plot of Estimated IMPS by Group", x = "Week", y = "Estimated IMPS", color = "Group") +
  theme_minimal()
