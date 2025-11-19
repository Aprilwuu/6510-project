# load data and packages
library(dplyr)
library(ggplot2)
library(agridat)


data("chakravertti.factorial")
df <- chakravertti.factorial
str(df)

df <- df %>%
  mutate(
    date    = factor(date,    levels = c("A01","A16","J16","S01","S16")),
    variety = factor(gen,     levels = c("Bhasakalma","Bhasamanik","Nehara")),
    spacing = factor(spacing, levels = c(6, 9, 12)),
    seed    = factor(seeds,   levels = c("1","2","local")),
    block   = factor(block)
  )

# summary statistics
summary(df$yield)
df %>% group_by(date) %>% summarise(mean_yield = mean(yield))
df %>% group_by(variety) %>% summarise(mean_yield = mean(yield))
df %>% group_by(spacing) %>% summarise(mean_yield = mean(yield))
df %>% group_by(seed) %>% summarise(mean_yield = mean(yield))

# Box plots for single factor
# main plot factor-planting date
ggplot(df, aes(date, yield)) +
  geom_boxplot(fill="skyblue") +
  labs(
    title="Yield by Planting Date", 
    x = "Planting Date",
    y = "Yield"
  ) +
  theme_bw()

# subplot factor-rice genotype
ggplot(df, aes(variety, yield)) +
  geom_boxplot(fill="lightgreen") +
  labs(
    title="Yield by Variety", 
    x = "Variety",
    y = "Yield"
  ) +
  theme_bw()

# sub-subplot factor-plant spacing
ggplot(df, aes(spacing, yield)) +
  geom_boxplot(fill="orange") +
  labs(
    title = "Yield by Plant Spacing",
    x = "Spacing",
    y = "Yield"
  ) +
  theme_bw()

# sub-subplot factor-seedlings per hill
ggplot(df, aes(seed, yield)) +
  geom_boxplot(fill="pink") +
  labs(
    title = "Yield by Seedlings per Hill", 
    x = "Seedlings per Hill",
    y = "Yield"
  ) +
  theme_bw()

# Interaction Plots

# dates * genotype
cols <- c("red", "darkgreen", "black")
ltys <- c(1, 2, 3)
pchs <- c(16, 16, 16)

interaction.plot(
  x.factor     = df$date,
  trace.factor = df$variety,
  response     = df$yield,
  type = "b",
  lwd = 2,
  col = cols,
  lty = ltys,
  pch = pchs,
  xlab = "Planting Date",
  ylab = "Mean Yield",
  main = "Interaction: Planting Date × Variety",
  legend = FALSE
)

legend("topright", 
       legend=levels(df$variety), col=cols, lty=ltys, lwd=2, pch=pchs, title="Variety", bty="n",cex=0.8)


# genotype * spacing
interaction.plot(
  x.factor     = df$variety,    # x-axis: rice variety
  trace.factor = df$spacing,    # trace: spacing levels (6, 9, 12)
  response     = df$yield,      # response variable: yield
  type  = "b",                  # “b” = both lines and points
  lwd   = 2,                    # line width
  col   = cols,                # colors for each spacing level
  lty   = ltys,                 # line types
  pch   = pchs,                 # point shape
  xlab  = "Variety",            # x-axis label
  ylab  = "Mean Yield",         # y-axis label
  main  = "Interaction: Variety × Spacing",  # title
  legend = FALSE                # disable the default legend
)

# Add a custom legend (placed in the top-right corner)
legend(
  "topright",
  legend = levels(df$spacing),  # spacing levels (e.g., "6", "9", "12")
  col    = cols,
  lty    = ltys,
  lwd    = 2,
  pch    = pchs,
  title  = "Spacing (inches)",  # legend title
  bty    = "n",                 # no box border
  cex    = 0.8                  # reduce text size
)


# spacing * seedlings

# Base interaction plot (legend disabled for manual control)
interaction.plot(
  x.factor     = df$spacing,   # x-axis: spacing (inches)
  trace.factor = df$seed,      # trace lines: number of seedlings per hill
  response     = df$yield,     # response variable: yield
  type  = "b",
  lwd   = 2,
  col   = cols,
  lty   = ltys,
  pch   = pchs,
  xlab  = "Plant Spacing (inches)",
  ylab  = "Mean Yield",
  main  = "Interaction: Spacing × Seedlings",
  legend = FALSE
)

# Add a custom legend
legend(
  "topright",
  legend = levels(df$seed),   # seed levels: "1", "2", "local"
  col    = cols,
  lty    = ltys,
  lwd    = 2,
  pch    = pchs,
  title  = "Seedlings per Hill",
  bty    = "n",
  cex    = 0.8                # slightly smaller legend text
)

# block statistic summary
df %>% group_by(block) %>% summarise(mean_yield = mean(yield))

ggplot(df, aes(block, yield)) +
  geom_boxplot(fill = "lightgray") +
  labs(title = "Yield by Block", x = "Block", y = "Yield") +
  theme_bw()



