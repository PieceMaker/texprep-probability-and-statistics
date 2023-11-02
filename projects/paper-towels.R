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
towelSummaryStats <- aggregate(. ~ group + brand + towel, towels[,c('group', 'brand', 'towel', 'weights')], mean)
names(towelSummaryStats) <- c('group', 'brand', 'towel', 'mean')
towelSummaryStats$standardDeviation <- aggregate(. ~ group + brand + towel, towels[,c('group', 'brand', 'towel', 'weights')], sd)$weights

# Sort the data so it is easier to look at
towelSummaryStats <- towelSummaryStats[with(towelSummaryStats, order(group, towel, brand)),]

towelSummaryStats$lower <- towelSummaryStats$mean - 2 * towelSummaryStats$standardDeviation / 5^0.5
towelSummaryStats$upper <- towelSummaryStats$mean + 2 * towelSummaryStats$standardDeviation / 5^0.5
towelSummaryStats[(towelSummaryStats$group == 5) & (towelSummaryStats$towel == 'A'),]$lower <- towelSummaryStats[(towelSummaryStats$group == 5) & (towelSummaryStats$towel == 'A'),]$mean - 2 * towelSummaryStats[(towelSummaryStats$group == 5) & (towelSummaryStats$towel == 'A'),]$standardDeviation  / (4)^0.5
towelSummaryStats[(towelSummaryStats$group == 5) & (towelSummaryStats$towel == 'A'),]$upper <- towelSummaryStats[(towelSummaryStats$group == 5) & (towelSummaryStats$towel == 'A'),]$mean + 2 * towelSummaryStats[(towelSummaryStats$group == 5) & (towelSummaryStats$towel == 'A'),]$standardDeviation  / (4)^0.5

# Look at the data
towelSummaryStats
