Overview
Load Packages
Import & Prepare Data
Part 1: Multiple Linear Regression with a Dummy Variable
1a. The Model
1b. Fit the Model
1c. Interpret the Dummy Coefficient
1d. Visualize the Dummy Effect (Parallel Lines)
Part 2: Adding an Interaction Term
Part 3: Checking Regression Assumptions
Part 4: Model Selection — Stepwise
Part 5: Model Selection — LASSO
Part 6: Final Model Comparison
Summary: Key Concepts
Boston Housing — MLR with Dummy Variable, Interaction, Assumptions & Model Selection
YOUR NAME HERE
ADD THE DATE
Overview
In this lab we build a complete multiple linear regression analysis using the Boston housing dataset. We work through five stages in order:

Create a dummy variable from crim (crime rate)
Fit a MLR model with the dummy variable
Add an interaction term
Check all regression assumptions
Use Stepwise and LASSO to select the best model
Response variable: medv — median home value ($1,000s)

Load Packages
# Uncomment to install if needed:
# install.packages("MASS")
# install.packages("glmnet")
# install.packages("MASS")
# install.packages("car")

library(MASS)     # Boston dataset
library(glmnet)   # LASSO
library(car)      # VIF for multicollinearity check
Import & Prepare Data
data(Boston)

# ── Step 1: Create dummy variable from crim ──────────────────────────────────
# Split crime rate at its median: 1 = High Crime, 0 = Low Crime
median_crim <- median(Boston$crim)
cat("Median crime rate:", round(median_crim, 3), "\n")
## Median crime rate: 0.257
Boston$high_crime <- factor(
  ifelse(Boston$crim > median_crim, 1, 0),
  levels = c(0, 1),
  labels = c("Low Crime", "High Crime")
)

# Check the split
table(Boston$high_crime)
## 
##  Low Crime High Crime 
##        253        253
# Quick look at mean home value by crime group
tapply(Boston$medv, Boston$high_crime, mean)
##  Low Crime High Crime 
##   24.94941   20.11621
Why we create a dummy variable from crim:
crim is a continuous variable, but splitting it at the median lets us ask a cleaner question: “Do high-crime neighborhoods have systematically lower home values, even after controlling for other factors?” The dummy variable captures this group difference as a single coefficient.

# Boxplot: home value by crime group
boxplot(medv ~ high_crime, data = Boston,
        col  = c("steelblue", "tomato"),
        main = "Median Home Value by Crime Level",
        xlab = "Crime Level",
        ylab = "Median Home Value ($1000s)")
abline(h = mean(Boston$medv), lty = 2, col = "darkgreen", lwd = 1.5)
legend("topright", legend = "Overall mean", lty = 2, col = "darkgreen")


Question 1: Based on the boxplot, how large is the difference in median home value between Low Crime and High Crime tracts? Do you expect this to remain significant after controlling for other predictors?

Part 1: Multiple Linear Regression with a Dummy Variable
1a. The Model
We use rm (rooms), lstat (% lower status), ptratio (pupil-teacher ratio), and high_crime as predictors.

The model equation is:

medv^=𝛽0+𝛽1⋅rm+𝛽2⋅lstat+𝛽3⋅ptratio+𝛽4⋅1[High Crime]

When high_crime = "Low Crime" (reference): dummy = 0
→ medv^=𝛽0+𝛽1⋅rm+𝛽2⋅lstat+𝛽3⋅ptratio

When high_crime = "High Crime": dummy = 1
→ medv^=(𝛽0+𝛽4)+𝛽1⋅rm+𝛽2⋅lstat+𝛽3⋅ptratio

𝛽4
 is the intercept shift — the estimated difference in home value between high- and low-crime tracts, holding all other variables constant.

1b. Fit the Model
model_dummy <- lm(medv ~ rm + lstat + ptratio + high_crime, data = Boston)
summary(model_dummy)
## 
## Call:
## lm(formula = medv ~ rm + lstat + ptratio + high_crime, data = Boston)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -14.8722  -3.0207  -0.8036   1.8771  28.4840 
## 
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          20.33775    3.98272   5.106 4.67e-07 ***
## rm                    4.33232    0.43256  10.016  < 2e-16 ***
## lstat                -0.61573    0.04666 -13.195  < 2e-16 ***
## ptratio              -0.96560    0.11831  -8.162 2.69e-15 ***
## high_crimeHigh Crime  1.15893    0.53256   2.176     0.03 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.21 on 501 degrees of freedom
## Multiple R-squared:  0.6816, Adjusted R-squared:  0.6791 
## F-statistic: 268.2 on 4 and 501 DF,  p-value: < 2.2e-16
1c. Interpret the Dummy Coefficient
beta_crime <- coef(model_dummy)["high_crimeHigh Crime"]
cat("Estimated penalty for High Crime areas: $",
    round(beta_crime * 1000, 0), "\n")
## Estimated penalty for High Crime areas: $ 1159
# Confidence interval for the crime dummy
confint(model_dummy, "high_crimeHigh Crime", level = 0.95)
##                         2.5 %   97.5 %
## high_crimeHigh Crime 0.112615 2.205255
Question 2: Interpret the dummy coefficient:

What is the estimated difference in home value between High Crime and Low Crime tracts, holding rm, lstat, and ptratio fixed?
Is the coefficient statistically significant? Report the p-value and 95% confidence interval.
Does the sign of the coefficient match your expectation from the boxplot?
1d. Visualize the Dummy Effect (Parallel Lines)
# Predicted medv vs rm, holding lstat and ptratio at their means
lstat_mean   <- mean(Boston$lstat)
ptratio_mean <- mean(Boston$ptratio)
rm_seq       <- seq(min(Boston$rm), max(Boston$rm), length.out = 200)

pred_low  <- predict(model_dummy,
  newdata = data.frame(rm = rm_seq, lstat = lstat_mean, ptratio = ptratio_mean,
                       high_crime = factor("Low Crime",  levels = c("Low Crime","High Crime"))))
pred_high <- predict(model_dummy,
  newdata = data.frame(rm = rm_seq, lstat = lstat_mean, ptratio = ptratio_mean,
                       high_crime = factor("High Crime", levels = c("Low Crime","High Crime"))))

plot(Boston$rm, Boston$medv,
     col  = ifelse(Boston$high_crime == "High Crime", "tomato", "steelblue"),
     pch  = 16, cex = 0.6,
     xlab = "Average Rooms per Dwelling (rm)",
     ylab = "Median Home Value ($1000s)",
     main = "MLR with Crime Dummy — Parallel Regression Lines")
lines(rm_seq, pred_low,  col = "steelblue", lwd = 2.5)
lines(rm_seq, pred_high, col = "tomato",    lwd = 2.5)
legend("topleft",
       legend = c("Low Crime", "High Crime"),
       col    = c("steelblue", "tomato"),
       pch = 16, lwd = 2)


Key observation: The two lines are parallel — same slope, different intercepts. The dummy variable shifts the line down for High Crime areas but assumes the effect of rm on medv is identical in both groups. We test this assumption in Part 2.

Part 2: Adding an Interaction Term
2a. Why Interact?
The parallel-lines model assumes the effect of rm on home value is the same in both High Crime and Low Crime areas. But is that realistic? An extra room might be worth more in a safe neighborhood than a dangerous one. An interaction term rm × high_crime lets the slopes differ.

medv^=𝛽0+𝛽1⋅rm+𝛽2⋅lstat+𝛽3⋅ptratio+𝛽4⋅𝐷+𝛽5⋅(rm×𝐷)

Where 𝐷=1[High Crime]

Low Crime slope: 𝛽1

High Crime slope: 𝛽1+𝛽5

𝛽5
 = how much the slope of rm changes in High Crime areas
2b. Fit the Interaction Model
# rm * high_crime is shorthand for rm + high_crime + rm:high_crime
model_interact <- lm(medv ~ rm * high_crime + lstat + ptratio, data = Boston)
summary(model_interact)
## 
## Call:
## lm(formula = medv ~ rm * high_crime + lstat + ptratio, data = Boston)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.4714  -2.9581  -0.6342   1.8723  27.7870 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              5.97003    5.13473   1.163    0.246    
## rm                       6.48816    0.65527   9.901  < 2e-16 ***
## high_crimeHigh Crime    20.38899    4.47868   4.552 6.67e-06 ***
## lstat                   -0.60643    0.04591 -13.208  < 2e-16 ***
## ptratio                 -0.93797    0.11645  -8.055 5.88e-15 ***
## rm:high_crimeHigh Crime -3.05225    0.70600  -4.323 1.86e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.12 on 500 degrees of freedom
## Multiple R-squared:  0.6931, Adjusted R-squared:   0.69 
## F-statistic: 225.8 on 5 and 500 DF,  p-value: < 2.2e-16
2c. Extract & Compare Slopes
beta_int <- coef(model_interact)

slope_low  <- beta_int["rm"]
slope_high <- beta_int["rm"] + beta_int["rm:high_crimeHigh Crime"]

cat("Slope of rm — Low Crime areas: ", round(slope_low,  3), "\n")
## Slope of rm — Low Crime areas:  6.488
cat("Slope of rm — High Crime areas:", round(slope_high, 3), "\n")
## Slope of rm — High Crime areas: 3.436
cat("Interaction coefficient:        ", round(beta_int["rm:high_crimeHigh Crime"], 3), "\n")
## Interaction coefficient:         -3.052
2d. Formal Test: Does the Interaction Improve the Model?
# F-test: is the interaction term worth adding?
anova(model_dummy, model_interact)
## Analysis of Variance Table
## 
## Model 1: medv ~ rm + lstat + ptratio + high_crime
## Model 2: medv ~ rm * high_crime + lstat + ptratio
##   Res.Df   RSS Df Sum of Sq      F    Pr(>F)    
## 1    501 13599                                  
## 2    500 13109  1    490.05 18.691 1.856e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
Question 3: Interpret the interaction results:

Is the interaction term rm:high_crimeHigh Crime statistically significant?
In which group (Low Crime or High Crime) does each additional room add more value?
What does the ANOVA F-test tell you — does the interaction significantly improve model fit?
2e. Visualize the Interaction (Non-Parallel Lines)
pred_low_int  <- predict(model_interact,
  newdata = data.frame(rm = rm_seq, lstat = lstat_mean, ptratio = ptratio_mean,
                       high_crime = factor("Low Crime",  levels = c("Low Crime","High Crime"))))
pred_high_int <- predict(model_interact,
  newdata = data.frame(rm = rm_seq, lstat = lstat_mean, ptratio = ptratio_mean,
                       high_crime = factor("High Crime", levels = c("Low Crime","High Crime"))))

plot(Boston$rm, Boston$medv,
     col  = ifelse(Boston$high_crime == "High Crime", "tomato", "steelblue"),
     pch  = 16, cex = 0.6,
     xlab = "Average Rooms per Dwelling (rm)",
     ylab = "Median Home Value ($1000s)",
     main = "MLR with Interaction: rm × high_crime\n(Non-parallel lines = different slopes)")
lines(rm_seq, pred_low_int,  col = "steelblue", lwd = 2.5)
lines(rm_seq, pred_high_int, col = "tomato",    lwd = 2.5)
legend("topleft",
       legend = c("Low Crime", "High Crime"),
       col    = c("steelblue", "tomato"),
       pch = 16, lwd = 2)


Compare to Part 1: The lines are now non-parallel — the interaction allows each group to have its own slope. If the lines cross, it means the effect of rm actually reverses direction depending on crime level.

Part 3: Checking Regression Assumptions
Before trusting the model, we must verify the five key assumptions. We use model_interact as our working model.

Assumption 1 — Linearity
Residuals vs Fitted values should show no pattern.

plot(model_interact, which = 1,
     main = "Assumption 1: Linearity — Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)


What to look for: A flat red smoother line with random scatter. A U-shape or curve indicates non-linearity.

Assumption 2 — Independence
Observations should not be related to each other.

# Plot residuals in observation order — look for patterns or trends
plot(residuals(model_interact), type = "l",
     main = "Assumption 2: Independence — Residuals in Order",
     xlab = "Observation Index", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)


What to look for: No systematic trend across observations. Waves or drifts suggest dependence (e.g., spatial or temporal autocorrelation).

Assumption 3 — Normality of Residuals
Residuals should follow a normal (bell-curve) distribution.

# Q-Q plot
plot(model_interact, which = 2,
     main = "Assumption 3: Normality — Q-Q Plot of Residuals")


# Histogram of residuals
hist(residuals(model_interact),
     breaks = 30, col = "steelblue", border = "white",
     main  = "Histogram of Residuals",
     xlab  = "Residuals")
curve(dnorm(x, mean = mean(residuals(model_interact)),
            sd   = sd(residuals(model_interact))) * length(residuals(model_interact)) * diff(hist(residuals(model_interact), plot=FALSE)$breaks)[1],
      add = TRUE, col = "red", lwd = 2)


# Shapiro-Wilk test (formal test; sensitive to large n)
shapiro.test(residuals(model_interact))
## 
##  Shapiro-Wilk normality test
## 
## data:  residuals(model_interact)
## W = 0.90048, p-value < 2.2e-16
What to look for: Points falling close to the diagonal in the Q-Q plot. Heavy tails or S-shapes indicate non-normality.

Assumption 4 — Equal Variance (Homoscedasticity)
Residual spread should be constant across all fitted values.

# Scale-Location plot
plot(model_interact, which = 3,
     main = "Assumption 4: Equal Variance — Scale-Location Plot")


# Breusch-Pagan test (formal test for heteroscedasticity)
# install.packages("lmtest") if needed
# library(lmtest)
# bptest(model_interact)
What to look for: A flat horizontal red line. A funnel shape (spread increasing with fitted values) means heteroscedasticity — standard errors will be unreliable.

Assumption 5 — No Multicollinearity
Predictors should not be highly correlated with each other.

# Variance Inflation Factor (VIF)
# VIF > 5 = concern; VIF > 10 = serious problem
vif(model_interact)
##            rm    high_crime         lstat       ptratio rm:high_crime 
##      4.082841     96.778514      2.070407      1.224183     94.838373
# Correlation matrix of numeric predictors
cor_vars <- Boston[, c("rm", "lstat", "ptratio", "crim")]
round(cor(cor_vars), 3)
##             rm  lstat ptratio   crim
## rm       1.000 -0.614  -0.356 -0.219
## lstat   -0.614  1.000   0.374  0.456
## ptratio -0.356  0.374   1.000  0.290
## crim    -0.219  0.456   0.290  1.000
What to look for: VIF values close to 1 are ideal. Note: interaction terms naturally inflate VIF — this is expected and not a real problem. Focus on the main effect VIFs.

Influential Observations
# Cook's Distance — identifies high-influence observations
plot(model_interact, which = 4,
     main = "Cook's Distance — Influential Observations")


# Flag observations with Cook's D > 4/n
cooksd   <- cooks.distance(model_interact)
n        <- nrow(Boston)
influential <- which(cooksd > 4 / n)
cat("Number of potentially influential observations:", length(influential), "\n")
## Number of potentially influential observations: 40
cat("Row indices:", influential, "\n")
## Row indices: 9 49 99 142 153 157 162 163 164 167 187 196 204 205 215 225 226 229 234 254 258 262 263 268 284 365 366 368 369 370 371 372 373 374 375 376 381 413 415 491
Assumption Summary Table
# Quick summary of all checks
cat("=== Regression Assumption Checks ===\n\n")
## === Regression Assumption Checks ===
cat("1. Linearity:         Check Residuals vs Fitted plot — look for flat red line\n")
## 1. Linearity:         Check Residuals vs Fitted plot — look for flat red line
cat("2. Independence:      Check residual order plot — no systematic trend\n")
## 2. Independence:      Check residual order plot — no systematic trend
cat("3. Normality:         Shapiro-Wilk p-value =",
    round(shapiro.test(residuals(model_interact))$p.value, 4), "\n")
## 3. Normality:         Shapiro-Wilk p-value = 0
cat("   (p < 0.05 suggests departure from normality)\n")
##    (p < 0.05 suggests departure from normality)
cat("4. Homoscedasticity:  Check Scale-Location plot — look for flat band\n")
## 4. Homoscedasticity:  Check Scale-Location plot — look for flat band
cat("5. Multicollinearity: Max VIF =",
    round(max(vif(model_interact)), 2), "\n")
## 5. Multicollinearity: Max VIF = 96.78
cat("   (VIF > 10 is a serious concern)\n")
##    (VIF > 10 is a serious concern)
Question 4: Based on all five diagnostic checks:

Which assumptions appear to be satisfied?
Which assumptions may be violated? What evidence from the plots supports this?
If heteroscedasticity is present, what corrective action could you take?
Part 4: Model Selection — Stepwise
Now that we have a working model, we use automated selection methods to find the best set of predictors from the full Boston dataset. We exclude the original crim (since we replaced it with high_crime) and work with all remaining variables.

4a. Prepare the Full Model
# Build a full model with all available predictors + our dummy + interaction
# Exclude original crim (replaced by high_crime)
model_full <- lm(medv ~ zn + indus + chas + nox + rm + age +
                   dis + rad + tax + ptratio + black + lstat +
                   high_crime + rm:high_crime,
                 data = Boston)
summary(model_full)
## 
## Call:
## lm(formula = medv ~ zn + indus + chas + nox + rm + age + dis + 
##     rad + tax + ptratio + black + lstat + high_crime + rm:high_crime, 
##     data = Boston)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.8501  -2.6727  -0.6174   1.8174  25.8966 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              24.519119   6.190329   3.961 8.57e-05 ***
## zn                        0.026250   0.014373   1.826 0.068396 .  
## indus                     0.044441   0.061082   0.728 0.467231    
## chas                      2.773586   0.850458   3.261 0.001186 ** 
## nox                     -21.286912   3.958836  -5.377 1.17e-07 ***
## rm                        5.716780   0.673907   8.483 2.60e-16 ***
## age                      -0.001745   0.013213  -0.132 0.894988    
## dis                      -1.236054   0.200202  -6.174 1.40e-09 ***
## rad                       0.182726   0.064863   2.817 0.005041 ** 
## tax                      -0.010411   0.003734  -2.788 0.005506 ** 
## ptratio                  -0.976563   0.129461  -7.543 2.24e-13 ***
## black                     0.011005   0.002640   4.169 3.61e-05 ***
## lstat                    -0.541666   0.049563 -10.929  < 2e-16 ***
## high_crimeHigh Crime     18.636574   4.475302   4.164 3.69e-05 ***
## rm:high_crimeHigh Crime  -2.637287   0.704371  -3.744 0.000202 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.69 on 491 degrees of freedom
## Multiple R-squared:  0.7472, Adjusted R-squared:   0.74 
## F-statistic: 103.7 on 14 and 491 DF,  p-value: < 2.2e-16
# Null model (intercept only) — starting point for forward selection
model_null <- lm(medv ~ 1, data = Boston)
4b. Forward Stepwise Selection
Starts with no predictors and adds the one that most improves AIC at each step.

model_forward <- step(model_null,
                      scope     = list(lower = model_null, upper = model_full),
                      direction = "forward",
                      trace     = FALSE)   # set trace=TRUE to see each step
summary(model_forward)
## 
## Call:
## lm(formula = medv ~ lstat + rm + ptratio + dis + nox + chas + 
##     black + high_crime + rm:high_crime, data = Boston)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.7912  -2.7501  -0.8019   1.7720  26.4098 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              19.032128   5.843334   3.257 0.001203 ** 
## lstat                    -0.536201   0.046446 -11.545  < 2e-16 ***
## rm                        6.366589   0.610382  10.430  < 2e-16 ***
## ptratio                  -1.010705   0.110068  -9.183  < 2e-16 ***
## dis                      -1.075467   0.160299  -6.709 5.36e-11 ***
## nox                     -22.147050   3.543335  -6.250 8.83e-10 ***
## chas                      2.988106   0.846694   3.529 0.000456 ***
## black                     0.010572   0.002579   4.099 4.85e-05 ***
## high_crimeHigh Crime     22.552226   4.147465   5.438 8.49e-08 ***
## rm:high_crimeHigh Crime  -3.226123   0.651862  -4.949 1.02e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.717 on 496 degrees of freedom
## Multiple R-squared:  0.7416, Adjusted R-squared:  0.7369 
## F-statistic: 158.2 on 9 and 496 DF,  p-value: < 2.2e-16
cat("\nForward Stepwise — Final predictors:\n")
## 
## Forward Stepwise — Final predictors:
cat(paste(names(coef(model_forward))[-1], collapse = ", "), "\n")
## lstat, rm, ptratio, dis, nox, chas, black, high_crimeHigh Crime, rm:high_crimeHigh Crime
cat("AIC:", round(AIC(model_forward), 2), "\n")
## AIC: 3017.71
cat("Adjusted R²:", round(summary(model_forward)$adj.r.squared, 4), "\n")
## Adjusted R²: 0.7369
4c. Backward Stepwise Selection
Starts with all predictors and removes the one that most improves AIC at each step.

model_backward <- step(model_full,
                       direction = "backward",
                       trace     = FALSE)
summary(model_backward)
## 
## Call:
## lm(formula = medv ~ zn + chas + nox + rm + dis + rad + tax + 
##     ptratio + black + lstat + high_crime + rm:high_crime, data = Boston)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.8351  -2.7379  -0.6455   1.8815  25.8731 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              24.594826   6.180122   3.980 7.94e-05 ***
## zn                        0.025713   0.014270   1.802 0.072179 .  
## chas                      2.832903   0.843490   3.359 0.000844 ***
## nox                     -20.695805   3.743916  -5.528 5.26e-08 ***
## rm                        5.643432   0.665139   8.485 2.55e-16 ***
## dis                      -1.260336   0.185753  -6.785 3.34e-11 ***
## rad                       0.170016   0.061745   2.754 0.006114 ** 
## tax                      -0.009262   0.003368  -2.750 0.006177 ** 
## ptratio                  -0.965282   0.127710  -7.558 2.00e-13 ***
## black                     0.010921   0.002629   4.155 3.84e-05 ***
## lstat                    -0.541561   0.046398 -11.672  < 2e-16 ***
## high_crimeHigh Crime     18.357118   4.443593   4.131 4.24e-05 ***
## rm:high_crimeHigh Crime  -2.592147   0.698091  -3.713 0.000228 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.683 on 493 degrees of freedom
## Multiple R-squared:  0.7469, Adjusted R-squared:  0.7408 
## F-statistic: 121.3 on 12 and 493 DF,  p-value: < 2.2e-16
cat("\nBackward Stepwise — Final predictors:\n")
## 
## Backward Stepwise — Final predictors:
cat(paste(names(coef(model_backward))[-1], collapse = ", "), "\n")
## zn, chas, nox, rm, dis, rad, tax, ptratio, black, lstat, high_crimeHigh Crime, rm:high_crimeHigh Crime
cat("AIC:", round(AIC(model_backward), 2), "\n")
## AIC: 3013.18
cat("Adjusted R²:", round(summary(model_backward)$adj.r.squared, 4), "\n")
## Adjusted R²: 0.7408
4d. Both-Direction Stepwise
At each step, considers both adding AND removing a predictor — picks whichever improves AIC most.

model_both <- step(model_full,
                   direction = "both",
                   trace     = FALSE)
summary(model_both)
## 
## Call:
## lm(formula = medv ~ zn + chas + nox + rm + dis + rad + tax + 
##     ptratio + black + lstat + high_crime + rm:high_crime, data = Boston)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.8351  -2.7379  -0.6455   1.8815  25.8731 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              24.594826   6.180122   3.980 7.94e-05 ***
## zn                        0.025713   0.014270   1.802 0.072179 .  
## chas                      2.832903   0.843490   3.359 0.000844 ***
## nox                     -20.695805   3.743916  -5.528 5.26e-08 ***
## rm                        5.643432   0.665139   8.485 2.55e-16 ***
## dis                      -1.260336   0.185753  -6.785 3.34e-11 ***
## rad                       0.170016   0.061745   2.754 0.006114 ** 
## tax                      -0.009262   0.003368  -2.750 0.006177 ** 
## ptratio                  -0.965282   0.127710  -7.558 2.00e-13 ***
## black                     0.010921   0.002629   4.155 3.84e-05 ***
## lstat                    -0.541561   0.046398 -11.672  < 2e-16 ***
## high_crimeHigh Crime     18.357118   4.443593   4.131 4.24e-05 ***
## rm:high_crimeHigh Crime  -2.592147   0.698091  -3.713 0.000228 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.683 on 493 degrees of freedom
## Multiple R-squared:  0.7469, Adjusted R-squared:  0.7408 
## F-statistic: 121.3 on 12 and 493 DF,  p-value: < 2.2e-16
cat("\nBoth-Direction Stepwise — Final predictors:\n")
## 
## Both-Direction Stepwise — Final predictors:
cat(paste(names(coef(model_both))[-1], collapse = ", "), "\n")
## zn, chas, nox, rm, dis, rad, tax, ptratio, black, lstat, high_crimeHigh Crime, rm:high_crimeHigh Crime
cat("AIC:", round(AIC(model_both), 2), "\n")
## AIC: 3013.18
cat("Adjusted R²:", round(summary(model_both)$adj.r.squared, 4), "\n")
## Adjusted R²: 0.7408
4e. Compare Stepwise Results
stepwise_compare <- data.frame(
  Method    = c("Forward", "Backward", "Both"),
  Adj_R2    = c(summary(model_forward)$adj.r.squared,
                summary(model_backward)$adj.r.squared,
                summary(model_both)$adj.r.squared),
  AIC       = c(AIC(model_forward), AIC(model_backward), AIC(model_both)),
  BIC       = c(BIC(model_forward), BIC(model_backward), BIC(model_both)),
  Num_Coefs = c(length(coef(model_forward)),
                length(coef(model_backward)),
                length(coef(model_both)))
)
stepwise_compare[, 2:4] <- round(stepwise_compare[, 2:4], 3)
print(stepwise_compare)
##     Method Adj_R2      AIC      BIC Num_Coefs
## 1  Forward  0.737 3017.713 3064.205        10
## 2 Backward  0.741 3013.176 3072.348        13
## 3     Both  0.741 3013.176 3072.348        13
Question 5:

Do all three stepwise methods arrive at the same final model?
Which method produces the lowest AIC? Which produces the fewest predictors?
Is the high_crime dummy retained in the final stepwise model? Is the interaction term rm:high_crime retained?
Part 5: Model Selection — LASSO
LASSO adds an ℓ1
 penalty to shrink coefficients — some all the way to exactly zero, automatically removing those predictors.

Minimize: 𝑅𝑆𝑆+𝜆∑𝑗=1𝑝|𝛽𝑗|

5a. Prepare the Model Matrix
# glmnet requires a numeric matrix — no formula interface
# We include the interaction term manually via model.matrix
x <- model.matrix(medv ~ zn + indus + chas + nox + rm + age +
                    dis + rad + tax + ptratio + black + lstat +
                    high_crime + rm:high_crime,
                  data = Boston)[, -1]   # drop intercept column

y <- Boston$medv

cat("Predictor matrix dimensions:", dim(x), "\n")
## Predictor matrix dimensions: 506 14
cat("Column names:\n", colnames(x), "\n")
## Column names:
##  zn indus chas nox rm age dis rad tax ptratio black lstat high_crimeHigh Crime rm:high_crimeHigh Crime
5b. Fit LASSO Over a Grid of Lambda Values
set.seed(42)
lasso_model <- glmnet(x, y, alpha = 1)   # alpha = 1 → LASSO

# Coefficient path: shows how each coefficient shrinks as lambda increases
plot(lasso_model, xvar = "lambda", label = TRUE)
title("LASSO Coefficient Path\n(each line = one predictor)", line = 2.5)


Reading the plot: Each colored line is one predictor. Moving right (larger 𝜆
) = stronger penalty = more coefficients forced to zero. The numbers at the top show how many predictors remain at each 𝜆
 value.

5c. Select Lambda via Cross-Validation
set.seed(42)
cv_lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 10)

# CV error curve
plot(cv_lasso)
title("LASSO: 10-Fold CV Mean Squared Error", line = 2.5)


lambda_min <- cv_lasso$lambda.min
lambda_1se <- cv_lasso$lambda.1se

cat("lambda.min (lowest CV error):       ", round(lambda_min, 4), "\n")
## lambda.min (lowest CV error):        0.0021
cat("lambda.1se (simplest within 1 SE):  ", round(lambda_1se, 4), "\n")
## lambda.1se (simplest within 1 SE):   0.3146
Lambda choice	Philosophy	Result
lambda.min	Best prediction accuracy	More predictors retained
lambda.1se	Simplest model within 1 SE of best	Fewer predictors — more parsimonious
5d. Inspect Selected Coefficients
cat("=== LASSO Coefficients at lambda.min ===\n")
## === LASSO Coefficients at lambda.min ===
coef_min <- coef(cv_lasso, s = "lambda.min")
print(coef_min)
## 15 x 1 sparse Matrix of class "dgCMatrix"
##                            lambda.min
## (Intercept)              25.632694443
## zn                        0.027620558
## indus                     0.039065107
## chas                      2.778087186
## nox                     -21.087555236
## rm                        5.527182164
## age                      -0.001927723
## dis                      -1.246546915
## rad                       0.181342484
## tax                      -0.010296212
## ptratio                  -0.972072068
## black                     0.010936286
## lstat                    -0.541939465
## high_crimeHigh Crime     17.010450312
## rm:high_crimeHigh Crime  -2.381998665
cat("\n=== LASSO Coefficients at lambda.1se ===\n")
## 
## === LASSO Coefficients at lambda.1se ===
coef_1se <- coef(cv_lasso, s = "lambda.1se")
print(coef_1se)
## 15 x 1 sparse Matrix of class "dgCMatrix"
##                           lambda.1se
## (Intercept)             19.903958970
## zn                       .          
## indus                    .          
## chas                     2.119732473
## nox                     -5.783777183
## rm                       4.255371981
## age                      .          
## dis                     -0.448074339
## rad                      .          
## tax                      .          
## ptratio                 -0.828684616
## black                    0.007312624
## lstat                   -0.526903561
## high_crimeHigh Crime     .          
## rm:high_crimeHigh Crime  .
# Which predictors survived at lambda.1se?
survived <- rownames(coef_1se)[which(coef_1se != 0)]
survived <- survived[survived != "(Intercept)"]
cat("\nPredictors retained at lambda.1se:", paste(survived, collapse = ", "), "\n")
## 
## Predictors retained at lambda.1se: chas, nox, rm, dis, ptratio, black, lstat
5e. Compare LASSO vs OLS on Test Data
set.seed(42)

# 70/30 train-test split
train_idx <- sample(1:nrow(Boston), size = floor(0.7 * nrow(Boston)))
x_train <- x[train_idx, ];   y_train <- y[train_idx]
x_test  <- x[-train_idx, ];  y_test  <- y[-train_idx]

# OLS on training data
ols_train  <- lm(medv ~ zn + indus + chas + nox + rm + age +
                   dis + rad + tax + ptratio + black + lstat +
                   high_crime + rm:high_crime,
                 data = Boston[train_idx, ])
ols_pred   <- predict(ols_train, newdata = Boston[-train_idx, ])
ols_mse    <- mean((y_test - ols_pred)^2)

# LASSO (refit CV on training data to prevent data leakage)
cv_lasso_train <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)
lasso_pred_min <- predict(cv_lasso_train, newx = x_test, s = "lambda.min")
lasso_pred_1se <- predict(cv_lasso_train, newx = x_test, s = "lambda.1se")
lasso_mse_min  <- mean((y_test - lasso_pred_min)^2)
lasso_mse_1se  <- mean((y_test - lasso_pred_1se)^2)

cat("=== Test Set MSE Comparison ===\n")
## === Test Set MSE Comparison ===
cat("OLS (full model):       ", round(ols_mse,       3), "\n")
## OLS (full model):        19.015
cat("LASSO (lambda.min):     ", round(lasso_mse_min, 3), "\n")
## LASSO (lambda.min):      20.271
cat("LASSO (lambda.1se):     ", round(lasso_mse_1se, 3), "\n")
## LASSO (lambda.1se):      26.395
Question 6:

Which predictors does LASSO eliminate at lambda.1se? Are high_crime and the interaction term retained?
Does LASSO achieve lower test MSE than OLS? Why might regularization help even when we have more observations than predictors?
Between lambda.min and lambda.1se, which would you prefer and why?
Part 6: Final Model Comparison
# Collect all models
all_models <- list(
  "Dummy only"          = model_dummy,
  "Dummy + Interaction" = model_interact,
  "Full model (OLS)"    = model_full,
  "Stepwise (backward)" = model_backward
)

comparison <- data.frame(
  Model     = names(all_models),
  Adj_R2    = sapply(all_models, function(m) round(summary(m)$adj.r.squared, 4)),
  AIC       = sapply(all_models, function(m) round(AIC(m), 2)),
  BIC       = sapply(all_models, function(m) round(BIC(m), 2)),
  Num_Coefs = sapply(all_models, function(m) length(coef(m)))
)

print(comparison)
##                                   Model Adj_R2     AIC     BIC Num_Coefs
## Dummy only                   Dummy only 0.6791 3113.34 3138.70         5
## Dummy + Interaction Dummy + Interaction 0.6900 3096.77 3126.35         6
## Full model (OLS)       Full model (OLS) 0.7400 3016.61 3084.24        15
## Stepwise (backward) Stepwise (backward) 0.7408 3013.18 3072.35        13
# Add LASSO test MSE to context
cat("\nLASSO test MSE (lambda.min):", round(lasso_mse_min, 3))
## 
## LASSO test MSE (lambda.min): 20.271
cat("\nLASSO test MSE (lambda.1se):", round(lasso_mse_1se, 3))
## 
## LASSO test MSE (lambda.1se): 26.395
cat("\nOLS  test MSE (full model): ", round(ols_mse, 3), "\n")
## 
## OLS  test MSE (full model):  19.015
Question 7 — Final Reflection:

Which model has the best balance of fit and simplicity (consider Adjusted R², AIC, BIC together)?
Does the stepwise-selected model retain the high_crime dummy and interaction term? What does that tell you about their practical importance?
LASSO and stepwise are both selection methods — when would you prefer one over the other?
If you had to recommend one final model to a policy maker, which would you choose and why?
Summary: Key Concepts
Concept	What it does	R syntax
Dummy variable	Converts categorical → 0/1 for regression	factor(ifelse(...)) then use in lm()
Dummy coefficient	Intercept shift between groups	Interpreted as $ mean difference, all else equal
Interaction term	Allows different slopes per group	lm(y ~ x * dummy)
Interaction coefficient	How much the slope changes across groups	𝛽interaction=slope difference
Assumption checks	Validate model reliability	plot(model), vif(), shapiro.test()
Forward stepwise	Build up from null model using AIC	step(..., direction="forward")
Backward stepwise	Trim down from full model using AIC	step(..., direction="backward")
LASSO	Shrink + zero out coefficients via 𝜆
cv.glmnet(x, y, alpha=1)
lambda.min	Best prediction accuracy	s = "lambda.min"
lambda.1se	Simpler, more parsimonious model	s = "lambda.1se"
Workflow to remember:

1. Create dummy variable → explore with boxplot
2. Fit MLR with dummy → interpret intercept shift
3. Add interaction → check if slopes differ across groups
4. Run ANOVA F-test → is interaction worth adding?
5. Check 5 assumptions → fix violations if needed
6. Apply stepwise → find best AIC-based subset
7. Apply LASSO → shrink and select simultaneously
8. Compare all models → choose based on Adj R², AIC, BIC, test MSE
