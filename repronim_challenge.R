# 2017-11-11
# Repronim Data Challenge
# Statistical analysis script for BET brain volumes
# ---------------------

# ---------------------
# Import the BET brainmask data
# ... colClasses{} needed to keep "F"-emale from being interpretted as logical "FALSE"
sub1 <- read.csv("female.csv", as.is = TRUE, header = FALSE, colClasses = c("character", "character", "numeric", "character", "character", "numeric"))
sub2 <- read.csv("male.csv", as.is = TRUE, header = FALSE, colClasses = c("character", "character", "numeric", "character", "character", "numeric"))

names(sub1) <- c("subjid", "sex", "age", "location", "set", "brainmask")
names(sub2) <- c("subjid", "sex", "age", "location", "set", "brainmask")

# combine the datasets
mydat <- data.frame(rbind(sub1, sub2), sub.set = c(rep("sub1", nrow(sub1)), rep("sub2", nrow(sub2))))
# ---------------------

# ---------------------
# run a stat model
lm1 <- lm(brainmask/1000 ~ sex, data = mydat)
sink(file="results.txt")
summary(lm1)
sink()

# Call:
# lm(formula = brainmask ~ sex, data = mydat)

# Residuals:
    # Min      1Q  Median      3Q     Max 
# -142916  -37315    -870   43524  185699 

# Coefficients:
             # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.453e+06  2.004e+04   72.48   <2e-16 ***
# sexM        8.635e-11  2.835e+04    0.00        1    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 77630 on 28 degrees of freedom
# Multiple R-squared:  8.032e-30,	Adjusted R-squared:  -0.03571 
# F-statistic: 2.249e-28 on 1 and 28 DF,  p-value: 1

# ---------------------


# ---------------------
# A plot to go with the data
mymeans <- with(mydat, aggregate(brainmask ~ sex, FUN = mean))
pdf(file="plot.pdf", height=5, width=5)
boxplot(brainmask/1000 ~ sex, data = mydat, horizontal = TRUE, notch = TRUE, xlab = "brainmask volume (cm3)", ylab = "sex", col = "skyblue")
with(mydat, points(x = mymeans[,2]/1000, y = c(1, 2), pch = 21, bg = "orange", cex = 3))
dev.off()
# ---------------------

