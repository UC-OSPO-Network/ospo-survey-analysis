# Do statistics for Q6 of the suervey (motivations for contributing to open source)

# TODO: Make the combining of research staff and postdocs into one category and optional command line argument
# TODO: Make output prettier so I can run this from command line
# TODO: Write up a summary of my findings

suppressWarnings(suppressMessages(source("utils.R")))


pairwise_z_test_lessthan <- function(
  df,
  outcome_col = "Skills",
  group1,
  group2,
  alternative = "less"
) {
  # Count total and 'yes' outcomes for each group
  n1 <- sum(df[["Role"]] == group1)
  y1 <- sum(df[["Role"]] == group1 & df[[outcome_col]] == 1)

  n2 <- sum(df[["Role"]] == group2)
  y2 <- sum(df[["Role"]] == group2 & df[[outcome_col]] == 1)

  # Perform the one-sided prop test (testing if group1 < group2)
  result <- prop.test(
    x = c(y1, y2),
    n = c(n1, n2),
    alternative = alternative,
  )

  return(result)
}


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

motivations_raw <- data %>%
  select(
    starts_with("motivations")
  )
motivations_raw <- shorten_long_responses(motivations_raw, codenames)
motivations_raw <- rename_cols_based_on_entries(motivations_raw)
motivations_raw$Role <- data$job_category
motivations_raw <- shorten_long_responses(
  motivations_raw,
  c("Other research staff" = "Other research staff")
)

# Remove any rows where they didn't answer the question
motivations_raw <- motivations_raw %>%
  filter(if_any(Job:Other, ~ .x != ""))

motivation_cols <- as.vector(codenames)
motivations_raw <- make_df_binary(motivations_raw, cols = motivation_cols)


# Combine post-docs and other research staff into one category
# The model is a good fit either way
motivations_processed <- motivations_raw %>%
  mutate(
    Role = recode(
      Role,
      "Post-Doc" = "Postdocs and Staff Researchers",
      "Other research staff" = "Postdocs and Staff Researchers"
    )
  )


############## Create the regression model ##############

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

# Residuals vs fitted plot shows residuals are distributed around zero
plot(fit)

# Correlations between variables are near zero (ish),
# suggesting that the model captures the important relationships
corrplot(cor(pit_resids), method = "circle")

# Looking at the coefficients
coefficients(fit)
t <- coefficients(fit)
t["XUndergraduate", "Give back"] <- 0
t["XUndergraduate", "Skills"] <- 0

# What happens if we remove undergrads?
Y2 <- motivations_processed %>%
  filter(Role != "Undergraduate") %>%
  select(-Role) %>%
  as.matrix()
X2 <- motivations_processed$Role[motivations_processed$Role != "Undergraduate"]
fit_no_undergrads <- manyglm(Y2 ~ X2, family = "binomial")


############## Hypothesis testing ##############

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

# Can we use the pairwise.comp argument to do pairwise comparisons?
anova_pw <- anova(
  fit,
  resamp = "pit.trap",
  test = "LR",
  p.uni = "adjusted",
  pairwise.comp = X
)

# Indicates three significant pairwise comparisons:
# Faculty vs Undergraduate, Postdocs and Staff Researchers vs Undergraduate, and Faculty vs Non-research Staff.

############## Power analysis ##############
# Post-hoc power analysis to determine whether we have enough undergraduates
# https://rpubs.com/sypark0215/223385
# https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html

# What total sample size is needed to achieve 80% power, assuming equal groups?

n_grad <- sum(motivations_processed$Role == "Grad Student")
n_grad_yes <- sum(
  motivations_processed$Role == "Grad Student" &
    motivations_processed$Skills == 1
)
p_grad_yes <- n_grad_yes / n_grad

n_undergrad <- sum(motivations_processed$Role == "Undergraduate")
n_undergrad_yes <- sum(
  motivations_processed$Role == "Undergraduate" &
    motivations_processed$Skills == 1
)
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
n_nonundergrad_yes <- sum(
  motivations_processed$Role != "Undergraduate" &
    motivations_processed$Skills == 1
)
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
# So we can proceed with this test.

############## Z-tests for significance of role vs. pskills ##############

# Finishing what we started above
res <- prop.test(
  x = c(n_nonundergrad_yes, n_undergrad_yes),
  n = c(n_nonundergrad, n_undergrad),
  alternative = "less"
)
res
# So undergraduates were significantly more likely to
# select “skills” as a motivation than non-undergraduates.

# Let's proceed with some more pairwise comparisons between groups for skills
pairwise_z_test_lessthan(
  motivations_raw, # Note we are just looking at post docs
  group1 = "Post-Doc",
  group2 = "Grad Student"
)

pairwise_z_test_lessthan(
  motivations_raw, # Note we are just looking at post docs
  group1 = "Faculty",
  group2 = "Post-Doc"
)

# Not very promising.

# This is a little p-hacky, but perhaps we can look at the overall trend (slope).
# Cochran–Armitage test for trend is built into the prop.trend.test function
# Recall "raw" just means I haven't combined post-docs and other research staff
n_postdoc <- sum(motivations_raw$Role == "Post-Doc")
n_postdoc_yes <- sum(
  motivations_raw$Role == "Post-Doc" & motivations_raw$Skills == 1
)
# For the other groups, it doesn't matter if we use the raw or processed data
n_faculty <- sum(motivations_processed$Role == "Faculty")
n_faculty_yes <- sum(
  motivations_processed$Role == "Faculty" & motivations_processed$Skills == 1
)

n_yes <- c(
  n_undergrad_yes,
  n_grad_yes,
  n_postdoc_yes,
  n_faculty_yes
)

n_tot <- c(
  n_undergrad,
  n_grad,
  n_postdoc,
  n_faculty
)

# Assign scores 1,2,3,4 for Undergrad --> Faculty
scores <- 1:4

prop.trend.test(
  x = n_yes,
  n = n_tot,
  score = scores
)
