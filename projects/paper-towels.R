# install.packages('ggplot2')
library(ggplot2)

towels <- read.csv('paper-towels.csv', header = T)

# Plot raw data grouped by paper towel brand
ggplot(towels, aes(weights, colour = brand, fill = brand)) +
  geom_histogram(position = "dodge", binwidth = 1)

# Plot raw data, grouped by paper towel brand and by student group
ggplot(towels, aes(weights, colour = brand, fill = brand)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  facet_grid(rows = vars(group))

# Compute mean and standard deviation by group-brand pairs
towelSummaryStats <- aggregate(. ~ group + brand, towels[,c('group', 'brand', 'weights')], mean)
names(towelSummaryStats) <- c('group', 'brand', 'mean')
towelSummaryStats$standardDeviation <- aggregate(. ~ group + brand, towels[,c('group', 'brand', 'weights')], sd)$weights

# Sort the data so it is easier to look at
towelSummaryStats <- towelSummaryStats[with(towelSummaryStats, order(group, brand)),]

# Look at the data
towelSummaryStats