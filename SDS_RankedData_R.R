# Setup 
library(readxl)
library(pmr)
library(PlackettLuce)
library(prefmod)
library(qvcalc)
library(smacof)

# Load data 
faculty.survey <- read_excel("pare-1331-finch.xlsx")
head(faculty.survey)
str(faculty.survey)

# Descriptive Statistics 
# Prepare rankings columns only (columns 1 to 6)
faculty.rankings <- data.frame(faculty.survey[, 1:6])
# Aggregate into ranking format for pmr library
faculty.rankings.agg <- rankagg(faculty.rankings)
# Descriptive statistics
faculty.desc <- destat(faculty.rankings.agg)
faculty.desc

# Standard deviations
sd(faculty.survey$contract)
sd(faculty.survey$salary)
sd(faculty.survey$health_care)
sd(faculty.survey$workload)
sd(faculty.survey$chair_support)
sd(faculty.survey$travel_budget)

# Chi-square test for non-random ranking
null_mean <- rep(3.5, 6)
A <- ((12 * 41) / (6 * (6 + 1)))
chi <- A * sum((faculty.desc$mean.rank - null_mean)^2)
chi                                            # chi-square statistic
dchisq(chi, 5)                                 # density (NOT p-value)
pchisq(chi, df = 5, lower.tail = FALSE)        # correct p-value
qchisq(0.95, df = 5)                           # critical value at p=0.05

# Compare Rankings Across Degree Groups
bachelors.rankings <- faculty.survey[which(faculty.survey$degree == 1), 1:6]
graduates.rankings <- faculty.survey[which(faculty.survey$degree > 1), 1:6]

bachelors.rankings.agg <- rankagg(bachelors.rankings)
graduates.rankings.agg <- rankagg(graduates.rankings)

bach.ranks <- destat(bachelors.rankings.agg)
grad.ranks <- destat(graduates.rankings.agg)

chisq.test(cbind(as.vector(bach.ranks$mar), as.vector(grad.ranks$mar)))
fisher.test(cbind(as.vector(bach.ranks$mar), as.vector(grad.ranks$mar)), simulate.p.value = TRUE)

t.test(bachelors.rankings[, 1], graduates.rankings[, 1])  # contract
t.test(bachelors.rankings[, 2], graduates.rankings[, 2])  # salary

# UMDS 
faculty.smacof <- smacofRect(faculty.survey[, 1:6], itmax = 1000)

# Joint configuration plot (items + respondents)
par(mar = c(2, 2, 2, 2))
plot(faculty.smacof, plot.type = "confplot", what = "both")

# Shepard plot (model fit check)
plot(faculty.smacof, plot.type = "Shepard")

# Stress value
faculty.smacof$stress

# Variance explained 
1 - faculty.smacof$stress^2


# Plackett-Luce Model 
# Convert rankings to PLM format
faculty.rankings2 <- as.rankings(faculty.rankings)

# Fit PLM with contract as reference (npseudo = 0 as in our analysis)
faculty.mod_mle <- PlackettLuce(faculty.rankings2, npseudo = 0)

# Summary with contract as reference
summary(faculty.mod_mle)

# Summary with mean worth as reference
summary(faculty.mod_mle, ref = NULL)

# Deviance, df and ratio — extracted directly from model
deviance_val   <- deviance(faculty.mod_mle)
df_val         <- faculty.mod_mle$df.residual
deviance_ratio <- deviance_val / df_val
cat("Deviance:", deviance_val, "\n")
cat("Degrees of freedom:", df_val, "\n")
cat("Deviance/df ratio:", deviance_ratio, "\n")

# Log-likelihood
logLik(faculty.mod_mle)

# Probability of each item being ranked first
itempar(faculty.mod_mle, ref = 1:6)

# Pairwise Z-test: Salary vs Health Care 
# Get full covariance matrix from model
vcov_plm <- vcov(faculty.mod_mle)
vcov_plm  # print full matrix

# Extract all values directly from model
alpha_salary     <- coef(faculty.mod_mle)["salary"]
alpha_healthcare <- coef(faculty.mod_mle)["health_care"]
se_salary        <- sqrt(vcov_plm["salary", "salary"])
se_healthcare    <- sqrt(vcov_plm["health_care", "health_care"])
cov_sal_hc       <- vcov_plm["salary", "health_care"]

# Z-test formula 
z_test <- (alpha_salary - alpha_healthcare) /
  sqrt(se_salary^2 + se_healthcare^2 - 2 * cov_sal_hc)
z_test

# Quasi-Standard Errors 
# Compute quasi-variances
faculty.qv <- qvcalc(faculty.mod_mle)

# Summary of quasi-variances
summary(faculty.qv)

# Worth plot with error bars
plot(faculty.qv,
     xlab = "Job qualities",
     ylab = "Log of worth",
     main = "Log worth of job qualities for contract faculty")

# PLM with Covariates 
# Create grouped rankings object
faculty.n <- nrow(faculty.survey)
faculty.g <- group(faculty.rankings2,
                   index = rep(seq_len(faculty.n), 1))

# Fit PLMC with experience
faculty.plmc.exp <- pltree(faculty.g ~ experience,
                           data = faculty.survey,
                           minsize = 2)
summary(faculty.plmc.exp)
plot(faculty.plmc.exp)
grid.text("PLT — Experience Only", x = 0.5, y = 0.98, gp = gpar(fontsize = 14))

# Fit PLMC with degree
faculty.plmc.deg <- pltree(faculty.g ~ degree,
                           data = faculty.survey,
                           minsize = 2)
summary(faculty.plmc.deg)
plot(faculty.plmc.deg)
grid.text("PLT — Degree Only", x = 0.5, y = 0.98, gp = gpar(fontsize = 14))

# AIC comparison
AIC(faculty.mod_mle)
AIC(faculty.plmc.exp)
AIC(faculty.plmc.deg)

# Plackett-Luce Tree
# Create binary grad variable 
faculty.survey$grad <- ifelse(faculty.survey$degree > 1, 1, 0)

# Fit PLT with grad + experience together
faculty.tree <- pltree(faculty.g ~ grad + experience,
                       data = faculty.survey,
                       minsize = 2,
                       maxdepth = 3)
faculty.tree
summary(faculty.tree)
plot(faculty.tree)
grid.text("PLT — Grad + Experience Together", x = 0.5, y = 0.98, gp = gpar(fontsize = 14))

