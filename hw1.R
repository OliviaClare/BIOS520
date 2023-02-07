dat <- readxl::read_xls("captopril.xls")
dat$SBP_change <- dat$SBP_WK1 - dat$SBP_WK0

boxplot(SBP_change ~ GROUP, data=dat,ylab = "Change in SBP", xlab="Group")
wilcox.test(SBP_change ~ GROUP, data=dat, exact=F, alternative="less")
mean(dat$SBP_change[dat$GROUP=="placebo"])
mean(dat$SBP_change[dat$GROUP=="captopril"])
