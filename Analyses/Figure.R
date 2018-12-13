## ART-Net Study 2018   ##
## Figure Script        ##
## 2018-09-25           ##
##
## https://stats.idre.ucla.edu/r/dae/poisson-regression/
## https://www.theanalysisfactor.com/generalized-linear-models-in-r-part-6-poisson-regression-count-variables/

# Read in data and packages ---------------------------------------
# Data
rm(list = ls())
p <- readRDS("Shiny Output/artnet4shiny.rda")
pshort <- subset(p, select = c("age", "totdegree", "maintotdegree", "castotdegree"))

# Packages
library(ggplot2)
library(tidyverse)
library(MASS)
library(sandwich)
library(data.table)

# NetParams version -------------------------------------
# acts/per week/per partnership for main and casual partnerships
# fx of: partnership duration,
#        age of each partner (might take sqrt of sum)
#        race combo (in two-category set up, 3 values)
#        HIV combo (same as race)
#        partnership type
#        geography main effect


l <- select(l, ptype, p_duration, age, p_age = p_AGE, city = city2,
            race = race.cat, p_race = p_race.cat, rai = p_RAI, iai = p_IAI,
            acts = p_anal.acts.week) %>%
  filter(ptype %in% 1:2) %>%
  filter(rai == 1 | iai == 1)
head(l, 25)

table(l$age, useNA = "always")
table(l$p_age, useNA = "always")
l$p_age[l$p_age %in% c("", "00")] <- NA
l$p_age <- as.numeric(l$p_age)

l$comb.age <- l$age + l$p_age
l$diff.age <- abs(l$age - l$p_age)

l$race2 <- ifelse(l$race %in% c("white", "other"), 1, 0)
l$p_race2 <- ifelse(l$p_race %in% c("white", "other"), 1, 0)
l$race.combo <- rep(NA, nrow(l))
l$race.combo[l$race2 == 0 & l$p_race2 == 0] <- 0
l$race.combo[l$race2 == 0 & l$p_race2 == 1] <- 1
l$race.combo[l$race2 == 1 & l$p_race2 == 0] <- 1
l$race.combo[l$race2 == 1 & l$p_race2 == 1] <- 2
table(l$race2, l$p_race2)
table(l$race.combo)
l <- select(l, -c(race, p_race, race2, p_race2))

l$p_duration[which(l$p_duration == 0)] <- sample(1:4, length(l$p_duration[which(l$p_duration == 0)]), TRUE)

head(l, 25)
sum(is.na(l$acts))

# with duration
mod <- glm(floor(acts*52) ~ p_duration + I(p_duration^2) + as.factor(race.combo) +
             as.factor(ptype) + comb.age + city, family = poisson(), data = l)
summary(mod)

b <- coef(mod)
x <- expand.grid(p_duration = seq(0, 1000, 100),
                 ptype = 1:2,
                 race.combo = 0:2,
                 comb.age = seq(30, 120, 30),
                 city = "Atlanta")
pred <- predict(mod, newdata = x, type = "response", se.fit = TRUE)
pred <- cbind(x,
              est = pred$fit/52,
              lcl = (pred$fit - 1.96*pred$se.fit)/52,
              ucl = (pred$fit + 1.96*pred$se.fit)/52)
pred

ggplot(pred, aes(p_duration, est, color = as.factor(comb.age), lty = as.factor(ptype))) +
  geom_line() +
  facet_wrap(~as.factor(race.combo)) +
  scale_color_viridis_d() +
  theme_minimal()

# no duration

# with duration
mod <- glm(floor(acts*52) ~ as.factor(race.combo) +
             as.factor(ptype) + comb.age + city, family = poisson(), data = l)
summary(mod)

b <- coef(mod)
x <- expand.grid(ptype = 1:2,
                 race.combo = 0:2,
                 comb.age = seq(30, 120, 30),
                 city = city_name)
pred <- predict(mod, newdata = x, type = "response", se.fit = TRUE)
pred <- cbind(x,
              est = pred$fit/52,
              lcl = (pred$fit - 1.96*pred$se.fit)/52,
              ucl = (pred$fit + 1.96*pred$se.fit)/52)
pred

# Updated version of Sam's ----------------------------------------------

## All testing ##

# Y axis: Degree (0-5)
# X axis: Age (continuous, 15-65)
# Lines: Partnership type + CI
# Model: Age x sqrt(Age)

# If using pnum as a predictor, adjust the 3 NA values to 0
#p$cuml.pnum[which(is.na(p$cuml.pnum))] <- 0

# Run models (add partner number as offset term?)
total <- glm(totdegree ~ age + sqrt(age), # + cuml.pnum,
             family = "poisson", data = p)
main <- glm(maintotdegree ~ age + sqrt(age), # + cuml.pnum,
            family = "poisson", data = p)
cas <- glm(castotdegree ~ age + sqrt(age) ,# + cuml.pnum,
           family = "poisson", data = p)
# Do these coefficient values need to be exponentiated?
# dftot <- as.data.frame(round(cbind(exp(coef(total)), rbind(exp(confint(total)))), 3))
# dfmain <- as.data.frame(round(cbind(exp(coef(main)), rbind(exp(confint(main)))), 3))
# dfcas <- as.data.frame(round(cbind(exp(coef(cas)), rbind(exp(confint(cas)))), 3))

pred <- data.frame(pshort, predtot = total$fitted.values,
                      predmain = main$fitted.values,
                      predcas = cas$fitted.values)

dftot <- as.data.frame(round(cbind(coef(total), rbind(confint(total))), 3))
dfmain <- as.data.frame(round(cbind(coef(main), rbind(confint(main))), 3))
dfcas <- as.data.frame(round(cbind(coef(cas), rbind(confint(cas))), 3))
colnames(dftot) <- c("Coef", "Lower", "Upper")
colnames(dfmain) <- c("Coef", "Lower", "Upper")
colnames(dfcas) <- c("Coef", "Lower", "Upper")

# Ages for plot
ages <- 15:65

## Total degree ##
pred <- matrix(NA, ncol = length(ages), nrow = 1)
for (ii in 1:ncol(pred)) {
  pred[, ii] <- exp(dftot$Coef[1] - log(2) + dftot$Coef[2]*ages[ii] + dftot$Coef[3]*(ages[ii]^2))
}
pred <- as.data.frame(pred)
names(pred) <- paste0("age", ages)
head(pred)

est <- apply(pred, 2, median)
lwr <- apply(pred, 2, quantile, 0.025)
upr <- apply(pred, 2, quantile, 0.975)

pred.all <- as.data.frame(cbind(est, lwr, upr))
pred.all$age <- ages


## Main degree ##
pred <- matrix(NA, ncol = length(ages), nrow = nrow(dfmain))
for (ii in 1:ncol(pred)) {
  pred[, ii] <- exp(dfmain$Coef[1] - log(2) + dfmain$Coef[2]*ages[ii] + dfmain$Coef[3]*(ages[ii]^2))
}
pred <- as.data.frame(pred)
names(pred) <- paste0("age", ages)
head(pred)

est <- apply(pred, 2, median)
lwr <- apply(pred, 2, quantile, 0.025)
upr <- apply(pred, 2, quantile, 0.975)

pred.main <- as.data.frame(cbind(est, lwr, upr))
pred.main$age <- ages


## Casual degree ##
pred <- matrix(NA, ncol = length(ages), nrow = nrow(dfcas))
for (ii in 1:ncol(pred)) {
  pred[, ii] <- exp(dfcas$Coef[1] - log(2) + dfcas$Coef[2]*ages[ii] + dfcas$Coef[3]*(ages[ii]^2))
}
pred <- as.data.frame(pred)
names(pred) <- paste0("age", ages)
head(pred)

est <- apply(pred, 2, median)
lwr <- apply(pred, 2, quantile, 0.025)
upr <- apply(pred, 2, quantile, 0.975)

pred.cas <- as.data.frame(cbind(est, lwr, upr))
pred.cas$age <- ages

# plot
# pdf(file = "analyses/Fig.pdf", h = 5, w = 9)
pal <- RColorBrewer::brewer.pal(3, "Set1")
pal.a <- adjustcolor(pal, alpha.f = 0.3)
par(mar = c(3,3,1,1), mgp = c(2,1,0))
plot(ages, pred.all$est, type = "n", ylim = c(0, 1.3), ylab = "Mean Degree", xlab = "Ages")
grid()
lines(ages, pred.all$est, col = pal[1], lwd = 1.3)
polygon(x = c(ages, rev(ages)), y = c(pred.all$lwr, rev(pred.all$upr)), col = pal.a[1], border = NA)
lines(ages, pred.main$est, col = pal[2], lwd = 1.3)
polygon(x = c(ages, rev(ages)), y = c(pred.main$lwr, rev(pred.main$upr)), col = pal.a[2], border = NA)
lines(ages, pred.cas$est, col = pal[3], lwd = 1.3)
polygon(x = c(ages, rev(ages)), y = c(pred.cas$lwr, rev(pred.cas$upr)), col = pal.a[3], border = NA)
legend("topright", legend = c("Total", "Main", "Casual"), col = pal, lwd = 2, cex = 0.9, bty = "n")
# dev.off()


# PSU example -------------
# https://onlinecourses.science.psu.edu/stat504/node/169/
crab <- fread('https://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson07/crab/index.txt')
colnames(crab) <- c("Obs","C","S","W","Wt","Sa")

#### to remove the column labeled "Obs"
crab <- crab[,-1]


model <- glm(crab$Sa ~ 1 + crab$W, family = poisson(link = log))

print <- data.frame(crab, pred = model$fitted)
print

#### note the linear predictor values
#### e.g., for the first observation, exp(1.3378)=3.810

model$linear.predictors
exp(model$linear.predictors)



# Online methods for further testing ------------------------













ggplot(p, aes(totdegree, fill = age.cat)) +
  geom_histogram(binwidth = 1) +
  facet_grid(race.cat ~ ., margins = TRUE, scales = "free")

# Neg bin
totalnb <- glm.nb(totdegree ~ age + sqrt(age), data = p) # Significant - NB fits

# LR test for NB vs Poisson
pchisq(2 * (logLik(totalnb) - logLik(total)), df = 1, lower.tail = FALSE)


# Predict values
newdata1 <- data.frame(math = mean(dat$math), prog = factor(1:3, levels = 1:3,
                                                            labels = levels(dat$prog)))
newdata1$phat <- predict(total, newdata1, type = "response")
newdata1


newdata2 <- data.frame(
  math = rep(seq(from = min(dat$math), to = max(dat$math), length.out = 100), 3),
  prog = factor(rep(1:3, each = 100), levels = 1:3, labels =
                  levels(dat$prog)))

newdata2 <- cbind(newdata2, predict(m1, newdata2, type = "link", se.fit=TRUE))
newdata2 <- within(newdata2, {
  DaysAbsent <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

ggplot(newdata2, aes(math, DaysAbsent)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = prog), alpha = .25) +
  geom_line(aes(colour = prog), size = 2) +
  labs(x = "Math Score", y = "Predicted Days Absent")

p <- within(p, {
  prog <- factor(prog, levels = 1:3, labels = c("General", "Academic",
                                            "Vocational"))
  id <- factor(id)
})
summary(p)

with(p, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth = .5, position = "dodge")

summary(m1 <- glm(num_awards ~ prog + math, family = "poisson", data=p))

cov.m1 <- vcovHC(m1, type = "HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate = coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail = FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

r.est

with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail = FALSE)))

s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)),
                 coef(m1), cov.m1)

s1 <- data.frame(math = mean(p$math),
                 prog = factor(1:3, levels = 1:3, labels = levels(p$prog))))

predict(m1, s1, type = "response", se.fit = TRUE)

## calculate and store predicted values
p$phat <- predict(m1, type="response")

## order by program and then by math
p <- p[with(p, order(prog, math)), ]

## create the plot
ggplot(p, aes(x = math, y = phat, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Math Score", y = "Expected number of awards")


# Method 2 - not quite there
ages <- 15:65
p$totdegree
plot(p$totdegree, x = p$age, axes = F)
axis(1, at = 15:65)
axis(side = 2)
y = c(1:5)
total <- glm(totdegree ~ age + sqrt(age), family = "poisson", data = p)
abline(total)

predProbs <- predict(total, data.frame(y = seq(min(ages), max(ages),
                                               length.out = 100)),
                     type = "response")
lines(seq(min(y), max(y), length.out = 100), predProbs, col = 2, lwd = 2)
