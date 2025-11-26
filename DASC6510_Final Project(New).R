
## EDA
library(dplyr)
library(ggplot2)
library(agridat)
library(emmeans)
library(dplyr)
library(knitr)

## EDA---------------------------------------------------------------
data("chakravertti.factorial")
df <- chakravertti.factorial
str(df)
df <- df %>%
  mutate(
    date  = factor(date, 
                   levels = c("J16", "A01", "A16", "S01", "S16"),
                   labels = c("Jul 16", "Aug 1", "Aug 16", "Sep 1", "Sep 16")),
    variety = factor(gen, levels= c("Bhasakalma","Bhasamanik","Nehara")),
    spacing = factor(spacing, levels = c(6, 9, 12)),
    seed    = factor(seeds, levels = c("1","2","local")),
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


## Model fitting

## Split-Split-Plot ANOVA-------------------------------------------

# Loading the lme4 package
library(lmerTest) 

## Fitting the mixed-effects model ##
# Fixed effects include all factors and their interactions. 
# Random effects capture the appropriate 
# error structures for each plot size.
splitSplitAnova_fit <- lmer(yield ~ date * variety * seed * spacing +
                              (1 | block) +
                              (1 | block:date) + 
                              (1 | block:date:variety),
                            data = df)
anova(splitSplitAnova_fit)

# Updating the model formula to remove the highest-order 
#   non-significant interaction term:
#     - date:variety:seed:spacing
splitSplitAnova2 <- update(splitSplitAnova_fit, .~. 
                           -date:variety:seed:spacing)
anova(splitSplitAnova2)

# Updating the model formula to remove non-significant  
#   3-way interaction terms:
#     - variety:seed:spacing
#     - date:seed:spacing
splitSplitAnova3 <- update(splitSplitAnova2, .~. 
                           -variety:seed:spacing
                           -date:seed:spacing)
anova(splitSplitAnova3)

# Updating the model formula to remove non-significant  
#   2-way interaction terms:
#     - seed:spacing
#     - variety:spacing
#     - variety:seed
splitSplitAnova4 <- update(splitSplitAnova3, .~. 
                           -seed:spacing
                           -variety:spacing
                           -variety:seed)
anova(splitSplitAnova4)
# The final reduced split-split-plot ANOVA model
splitSplitAnova_reduced <- splitSplitAnova4
anova(splitSplitAnova_reduced)


## Assumption check+effect & interaction analysis

## Assumption check+effect & interaction analysis---------------------
res <- resid(splitSplitAnova_reduced)
fit <- fitted(splitSplitAnova_reduced)
plot(fit, res,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)

qqnorm(res)
qqline(res, col="red")

#check block effect
library(lmerTest)
ranova(splitSplitAnova_reduced)

#interaction plot
### date x variety
library(emmeans)
emm1    <- emmeans(splitSplitAnova_reduced, ~ date | variety)
emm_df1 <- as.data.frame(emm1)

ggplot(emm_df1, aes(x = emmean,
                    y = date,
                    color = variety)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL),
                 height = 0.2, linewidth = 1) +
  facet_wrap(~ variety, ncol = 1) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 14) +
  labs(x = "Predicted mean yield",
       y = "Planting date",
       title = "Date × Variety interaction")

### date x spacing
df$spacing <- as.factor(df$spacing)
emm2    <- emmeans(splitSplitAnova_reduced, ~ date | spacing)
emm_df2 <- as.data.frame(emm2)

ggplot(emm_df2, aes(x = emmean,
                    y = date,
                    color = spacing)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL),
                 height = 0.2, linewidth = 1) +
  facet_wrap(~ spacing, ncol = 1) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 14) +
  labs(x = "Predicted mean yield",
       y = "Planting date",
       title = "Date × Spacing interaction")

### date x variety x seed
emm_seed <- emmeans(
  splitSplitAnova_reduced,
  ~ date * variety | seed
)

emm_seed_df <- as.data.frame(emm_seed)
ggplot(emm_seed_df,
       aes(x = date,
           y = emmean,
           color = variety,
           group = variety)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ seed, nrow = 1) +
  theme_bw(base_size = 13) +
  labs(
    title = "Date × Variety interaction at each Seed rate",
    x = "Planting date",
    y = "Predicted mean yield",
    color = "Variety"
  )

## Tukey Comparisons & Optimal Combination ----------------------------------

alpha <- 0.05

# Helper: add significance stars based on p-value
add_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}


# Tukey for Main Effects    #


## Planting Date
emm_date <- emmeans(splitSplitAnova_reduced, ~ date)
tukey_date <- pairs(emm_date, adjust = "tukey") |>
  summary(infer = TRUE) |>
  as.data.frame() |>
  mutate(
    contrast = gsub(" - ", " vs ", contrast),
    estimate = round(estimate, 1),
    SE       = round(SE, 1),
    lower.CL = round(lower.CL, 1),
    upper.CL = round(upper.CL, 1),
    p.value  = signif(p.value, 3),
    Sig      = add_stars(p.value)
  ) |>
  select(
    Contrast = contrast,
    `Mean diff` = estimate,
    SE,
    `Lower CL` = lower.CL,
    `Upper CL` = upper.CL,
    `t-ratio`  = t.ratio,
    `p-value`  = p.value,
    Sig
  )

kable(tukey_date, caption = "Tukey HSD for Planting Date")

## Variety (Genotype)
emm_var <- emmeans(splitSplitAnova_reduced, ~ variety)
tukey_var <- pairs(emm_var, adjust = "tukey") |>
  summary(infer = TRUE) |>
  as.data.frame() |>
  mutate(
    contrast = gsub(" - ", " vs ", contrast),
    estimate = round(estimate, 1),
    SE       = round(SE, 1),
    lower.CL = round(lower.CL, 1),
    upper.CL = round(upper.CL, 1),
    p.value  = signif(p.value, 3),
    Sig      = add_stars(p.value)
  ) |>
  select(
    Contrast = contrast,
    `Mean diff` = estimate,
    SE,
    `Lower CL` = lower.CL,
    `Upper CL` = upper.CL,
    `t-ratio`  = t.ratio,
    `p-value`  = p.value,
    Sig
  )

kable(tukey_var, caption = "Tukey HSD for Variety")

## Spacing
emm_space <- emmeans(splitSplitAnova_reduced, ~ spacing)
tukey_space <- pairs(emm_space, adjust = "tukey") |>
  summary(infer = TRUE) |>
  as.data.frame() |>
  mutate(
    contrast = gsub(" - ", " vs ", contrast),
    estimate = round(estimate, 1),
    SE       = round(SE, 1),
    lower.CL = round(lower.CL, 1),
    upper.CL = round(upper.CL, 1),
    p.value  = signif(p.value, 3),
    Sig      = add_stars(p.value)
  ) |>
  select(
    Contrast = contrast,
    `Mean diff` = estimate,
    SE,
    `Lower CL` = lower.CL,
    `Upper CL` = upper.CL,
    `t-ratio`  = t.ratio,
    `p-value`  = p.value,
    Sig
  )

kable(tukey_space, caption = "Tukey HSD for Plant Spacing")

## Seedlings per hill
emm_seed_main <- emmeans(splitSplitAnova_reduced, ~ seed)
tukey_seed <- pairs(emm_seed_main, adjust = "tukey") |>
  summary(infer = TRUE) |>
  as.data.frame() |>
  mutate(
    contrast = gsub(" - ", " vs ", contrast),
    estimate = round(estimate, 1),
    SE       = round(SE, 1),
    lower.CL = round(lower.CL, 1),
    upper.CL = round(upper.CL, 1),
    p.value  = signif(p.value, 3),
    Sig      = add_stars(p.value)
  ) |>
  select(
    Contrast = contrast,
    `Mean diff` = estimate,
    SE,
    `Lower CL` = lower.CL,
    `Upper CL` = upper.CL,
    `t-ratio`  = t.ratio,
    `p-value`  = p.value,
    Sig
  )

kable(tukey_seed, caption = "Tukey HSD for Seedlings per Hill")



# Tukey for Interaction Effects            #
#    (Date × Variety, Date × Spacing, etc.)   #


###  Date × Variety (compare varieties within each date)
emm_d_v <- emmeans(splitSplitAnova_reduced, ~ variety | date)
tukey_d_v <- contrast(emm_d_v, method = "pairwise", adjust = "tukey") |>
  summary(infer = TRUE) |>
  as.data.frame() |>
  mutate(
    contrast = gsub(" - ", " vs ", contrast),
    estimate = round(estimate, 1),
    SE       = round(SE, 1),
    lower.CL = round(lower.CL, 1),
    upper.CL = round(upper.CL, 1),
    p.value  = signif(p.value, 3),
    Sig      = add_stars(p.value)
  ) |>
  select(
    Date = date,
    Contrast = contrast,
    `Mean diff` = estimate,
    SE,
    `Lower CL` = lower.CL,
    `Upper CL` = upper.CL,
    `t-ratio`  = t.ratio,
    `p-value`  = p.value,
    Sig
  )

kable(tukey_d_v,
      caption = "Tukey Comparison: Variety Differences Within Each Planting Date")


###  Date × Spacing (compare spacing within each date)
emm_d_s <- emmeans(splitSplitAnova_reduced, ~ spacing | date)
tukey_d_s <- contrast(emm_d_s, method = "pairwise", adjust = "tukey") |>
  summary(infer = TRUE) |>
  as.data.frame() |>
  mutate(
    contrast = gsub(" - ", " vs ", contrast),
    estimate = round(estimate, 1),
    SE       = round(SE, 1),
    lower.CL = round(lower.CL, 1),
    upper.CL = round(upper.CL, 1),
    p.value  = signif(p.value, 3),
    Sig      = add_stars(p.value)
  ) |>
  select(
    Date = date,
    Contrast = contrast,
    `Mean diff` = estimate,
    SE,
    `Lower CL` = lower.CL,
    `Upper CL` = upper.CL,
    `t-ratio`  = t.ratio,
    `p-value`  = p.value,
    Sig
  )

kable(tukey_d_s,
      caption = "Tukey Comparison: Spacing Differences Within Each Planting Date")


### Date × Variety × Seed (variety within each Date × Seed)
emm_d_v_seed <- emmeans(
  splitSplitAnova_reduced,
  ~ variety | date * seed
)

tukey_d_v_seed <- contrast(emm_d_v_seed,
                           method = "pairwise",
                           adjust = "tukey") |>
  summary(infer = TRUE) |>
  as.data.frame() |>
  mutate(
    contrast = gsub(" - ", " vs ", contrast),
    estimate = round(estimate, 1),
    SE       = round(SE, 1),
    lower.CL = round(lower.CL, 1),
    upper.CL = round(upper.CL, 1),
    p.value  = signif(p.value, 3),
    Sig      = add_stars(p.value)
  ) |>
  select(
    Date = date,
    Seed = seed,
    Contrast = contrast,
    `Mean diff` = estimate,
    SE,
    `Lower CL` = lower.CL,
    `Upper CL` = upper.CL,
    `t-ratio`  = t.ratio,
    `p-value`  = p.value,
    Sig
  )

kable(tukey_d_v_seed,
      caption = "Tukey Comparison: Variety Differences Within Each Date × Seed Level")


###  Date × Variety × Spacing (spacing within each Date × Variety)
emm_d_v_space <- emmeans(
  splitSplitAnova_reduced,
  ~ spacing | date * variety
)

tukey_d_v_space <- contrast(emm_d_v_space,
                            method = "pairwise",
                            adjust = "tukey") |>
  summary(infer = TRUE) |>
  as.data.frame() |>
  mutate(
    contrast = gsub(" - ", " vs ", contrast),
    estimate = round(estimate, 1),
    SE       = round(SE, 1),
    lower.CL = round(lower.CL, 1),
    upper.CL = round(upper.CL, 1),
    p.value  = signif(p.value, 3),
    Sig      = add_stars(p.value)
  ) |>
  select(
    Date = date,
    Variety = variety,
    Contrast = contrast,
    `Mean diff` = estimate,
    SE,
    `Lower CL` = lower.CL,
    `Upper CL` = upper.CL,
    `t-ratio`  = t.ratio,
    `p-value`  = p.value,
    Sig
  )

kable(tukey_d_v_space,
      caption = "Tukey Comparison: Spacing Differences Within Each Date × Variety Combination")



#  Optimal Treatment Combinations           


# Estimated marginal means for all Date × Variety × Spacing × Seed combinations
emm_full <- emmeans(
  splitSplitAnova_reduced,
  ~ date * variety * spacing * seed
)

emm_full_df <- as.data.frame(emm_full)

# Top 10 combinations ranked by predicted yield
top10_table <- emm_full_df |>
  arrange(desc(emmean)) |>
  slice_head(n = 10) |>
  transmute(
    Rank  = row_number(),
    Date  = date,
    Variety = variety,
    Spacing = spacing,
    Seedlings = seed,
    `Predicted yield` = round(emmean, 1)
  )

kable(top10_table, caption = "Top 10 Predicted Treatment Combinations")

# Best single combination (Rank 1)
best_combo <- top10_table[1, ]
kable(best_combo, caption = "Best Predicted Combination")



# Export Results to CSV                    #


write.csv(tukey_date,      "tukey_date_main.csv",          row.names = FALSE)
write.csv(tukey_var,       "tukey_variety_main.csv",       row.names = FALSE)
write.csv(tukey_space,     "tukey_spacing_main.csv",       row.names = FALSE)
write.csv(tukey_seed,      "tukey_seedlings_main.csv",     row.names = FALSE)

write.csv(tukey_d_v,       "tukey_date_variety.csv",       row.names = FALSE)
write.csv(tukey_d_s,       "tukey_date_spacing.csv",       row.names = FALSE)
write.csv(tukey_d_v_seed,  "tukey_date_variety_seed.csv",  row.names = FALSE)
write.csv(tukey_d_v_space, "tukey_date_variety_spacing.csv", row.names = FALSE)

write.csv(top10_table,     "top10_treatment_combinations.csv", row.names = FALSE)
write.csv(best_combo,      "best_treatment_combination.csv",   row.names = FALSE)



