# 2017-11-11
# Repronim Data Challenge
# Statistical analysis script for BET brain volumes
# ---------------------

# ---------------------
# import the spreadsheet of source information
subdat <- read.csv("~/downloads/SFN-17ReproData - guest_11_11_2017_5_28_8.csv", as.is = TRUE)
# ---------------------

# ---------------------
# Some data exploration
names(subdat)
# looks oike "Label" codes a geographical place as a prefix.
# Get a table of the various prefixes:
# data.frame(table(gsub("(^.*)(_.*)", "\\1", subdat$Label)))
        # Var1 Freq
# 1        202    1
# 2        262    1
# 3        276    1
# 4        278    1
# 5        425    1
# 6         64    1
# 7         70    1
# 8   AnnArbor   13
# 9  Baltimore    1
# 10    Bangor    4
# 11   Beijing   79
# 12 Cambridge   96
# 13    Leiden    9
# 14   NewYork    2
# ... suggest sampling from Beijing and Cambridge!

table(subdat$Project)
# fcon_1000       ixi 
      # 204         7 
# ... and fcon_1000 as well?
# ... check this:
data.frame(
	table(
		gsub("(^.*)(_.*)", "\\1", subdat$Label[subdat$Project %in% "fcon_1000"])
	)
)
       # Var1 Freq
# 1  AnnArbor   13
# 2 Baltimore    1
# 3    Bangor    4
# 4   Beijing   79
# 5 Cambridge   96
# 6    Leiden    9
# 7   NewYork    2
# YES, all are in fcon_1000

# Could also do it this way:
# get a location variable
locate <- gsub("(^.*)(_.*)", "\\1", subdat$Label)
data.frame(table(locate))
# ... then the 'locate' var can be used later
# ---------------------


# ---------------------
# get subsets of the data
sub1 <- subdat[sample(which(locate %in% "Beijing"), 15), ]
sub2 <- subdat[sample(which(locate %in% "Cambridge"), 15), ]
# ---------------------


# ---------------------
# Import the BET brainmask data
# ... colClasses{} needed to keep "F"-emale from being interpretted as logical "FALSE"
bet <- read.csv("~/Downloads/sample.csv", as.is = TRUE, header = FALSE, colClasses = c("character", "character", "numeric", "character", "character", "numeric"))

# Assign names to the data
names(bet) <- c("subjid", "sex", "age", "location", "set", "brainmask")
# ---------------------


# ---------------------
# Use this as a subset to start the analysis
sub1 <- bet
sub2 <- bet

# change sex to M for sub2
sub2$sex <- "M"

# combine the datasets
mydat <- data.frame(rbind(sub1, sub2), sub.set = c(rep("sub1", nrow(sub1)), rep("sub2", nrow(sub2))))
# ---------------------

# ---------------------
# run a stat model
lm1 <- lm(brainmask/1000 ~ sex, data = mydat)
summary(lm1)

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
boxplot(brainmask/1000 ~ sex, data = mydat, horizontal = TRUE, notch = TRUE, xlab = "brainmask volume (cm3)", ylab = "sex", col = "skyblue")
mymeans <- with(mydat, aggregate(brainmask ~ sex, FUN = mean))
with(mydat, points(x = mymeans[,2]/1000, y = c(1, 2), pch = 21, bg = "orange", cex = 3))
# ---------------------

