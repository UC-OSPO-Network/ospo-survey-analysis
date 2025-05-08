# Do statistics for Q6 of the suervey (motivations for contributing to open source)

# TODO: Make the combining of research staff and postdocs into one category and optional command line argument
# TODO: Make output prettier so I can run this from command line
# TODO: Write up a summary of my findings

suppressWarnings(suppressMessages(source("utils.R")))


data <- load_qualtrics_data("deidentified_no_qual.tsv")

codenames <- c(
  "Developing open-source" = "Job",
  "To improve the tools" = "Improve Tools",
  "To customize existing" = "Customize",
  "To build a network" = "Network",
  "To give back to" = "Give back",
  "To improve my skills" = "Skills",
  "Because it's fun" = "Fun",
  "Other " = "Other"
)


############## Data wrangling ##############

motivations_raw <- data %>% select(
  starts_with("motivations")
)
motivations_raw <- shorten_long_responses(motivations_raw, codenames)
motivations_raw <- rename_cols_based_on_entries(motivations_raw)
motivations_raw$Role <- data$job_category
motivations_raw <- shorten_long_responses(motivations_raw, c("Other research staff" = "Other research staff"))

# Remove any rows where they didn't answer the question
motivations_raw <- motivations_raw %>%
  filter(if_any(Job:Other, ~ .x != ""))

motivation_cols <- as.vector(codenames)
motivations_processed <- make_df_binary(motivations_raw, cols = motivation_cols)


# Combine post-docs and other research staff into one category
# The model is a good fit either way
motivations_processed <- motivations_processed %>%
  mutate(
    Role = recode(
      Role,
      `Post-Doc` = "Postdocs and Staff Researchers",
      `Other research staff` = "Postdocs and Staff Researchers"
    )
  )





############## Regression analysis ##############

# Multivariate logistic regression predicting a vector of binary responses from Role

Y <- as.matrix(motivations_processed[, motivation_cols])
X <- motivations_processed$Role
fit <- manyglm(Y ~ X, family = "binomial")
fit

# Assessing goodness of fit

# Residual deviance statistic is close to DoF, which suggests good fit

# Residuals look normally distributed, at a glance
pit_resids <- residuals(fit, type = "pit.trap")
hist(pit_resids[, "Skills"], main = "PIT Residuals: Skills")

# Correlations between variables are near zero (ish),
# suggesting that the model captures the important relationships
corrplot(cor(pit_resids), method = "circle")

# Hypothesis testing: regression model

# anova() summarizes the statistical significance of the fitted model.
# test="LR" is the default, and specifies a likelihood ratio test.
# resamp indicates the method for resampling under the null hypothesis.
# resamp="pit.trap" is the default ("probability integral transform" residuals)
# The anova output starts with a table of the multivariate test statistics.
# This tests for the global effect of Role, by resampling the whole response vector.
# The next part of the table is the univariate test statistics,
# which are separate logistic regressions for each response variable, ignoring the other variables.

anova_result <- anova(fit, resamp = "pit.trap", test = "LR", p.uni = "adjusted")
anova_result
# Results:
# Our Pr(>Dev) is a statistically significant p-value,
# indicating that Role significantly predicts motivation profile.
# Only "Skills" shows a significant univariate effect of Role after
# multiple testing correction (p.uni = "adjusted").
# In other words, when considering whether Role can predict
# a single motivation, it can only predict Skills.







############## Power analysis ##############
# Post-hoc power analysis to determine whether we have enough undergraduates
# https://rpubs.com/sypark0215/223385
# https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html

# What total sample size is needed to achieve 80% power, assuming equal groups?

n_grad <- sum(motivations_processed$Role == "Grad Student")
n_grad_yes <- sum(motivations_processed$Role == "Grad Student" & motivations_processed$Skills == 1)
p_grad_yes <- n_grad_yes / n_grad

n_undergrad <- sum(motivations_processed$Role == "Undergraduate")
n_undergrad_yes <- sum(motivations_processed$Role == "Undergraduate" & motivations_processed$Skills == 1)
p_undergrad_yes <- n_undergrad_yes / n_undergrad


# Cohen's h effect size
h <- pwr::ES.h(p_grad_yes, p_undergrad_yes)

# Calculate sample size, assuming equal group sizes
# Let's be stringent and say two-sided
pwr::pwr.2p.test(
  h = h,
  sig.level = 0.05,
  power = 0.8,
  alternative = "two.sided"
)

# Now, what ratio of N_undergrads vs. N_gradstudents is needed to achieve 80% power?
# (Unequal group sizes)
pwr::pwr.2p2n.test(
  h = h,
  n1 = n_grad,
  sig.level = 0.05,
  power = 0.8,
  alternative = "less"
)

# Bah! For two-sided, we would need several times more undergrads to achieve 80% power.
# For one-sided, we would still need twice as many undergrads.
# So we don't have enough power to test whether undergrads are more likely
# than graduate students to select "skills" as a motivation.

# What if we do undergrads vs. everyone else combined?
n_nonundergrad <- sum(motivations_processed$Role != "Undergraduate")
n_nonundergrad_yes <- sum(motivations_processed$Role != "Undergraduate" & motivations_processed$Skills == 1)
p_nonundergrad_yes <- n_nonundergrad_yes / n_nonundergrad


h <- pwr::ES.h(p_nonundergrad_yes, p_undergrad_yes)

pwr::pwr.2p2n.test(
  h = h,
  n1 = n_nonundergrad,
  sig.level = 0.05,
  power = 0.8,
  alternative = "two.sided"
)

# In this case, we have enough undergraduates.
# So we can proceed with this less-compelling test.

prop.test(
  x = c(n_nonundergrad_yes, n_undergrad_yes),
  n = c(n_nonundergrad, n_undergrad),
  alternative = "less",
  correct = FALSE
)

# So undergraduates were significantly more likely to
# select “skills” as a motivation than non-undergraduates.
