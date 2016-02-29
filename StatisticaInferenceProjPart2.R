##Quick exploration of the data set

#For the purposes of this analysis, we will convert the dose variable into a factor.

data(ToothGrowth)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

library(pander)
ts <- lapply(c(.5, 1, 2), function(x) {
  t.test(len ~ supp, data=subset(ToothGrowth, dose==x), paired=FALSE, var.equal=FALSE)
})
pvals <- c(ts[[1]]$p.value, ts[[2]]$p.value, ts[[3]]$p.value)
stats <- c(ts[[1]]$statistic, ts[[2]]$statistic, ts[[3]]$statistic)
adjp <- p.adjust(pvals, method = "bonferroni")
lls <- sapply(c(ts[[1]]$conf.int[1], ts[[2]]$conf.int[1], ts[[3]]$conf.int[1]), round, 3)
uls <- sapply(c(ts[[1]]$conf.int[2], ts[[2]]$conf.int[2], ts[[3]]$conf.int[2]), round, 3)
df <- data.frame(dose=c(0.5, 1, 2), t=stats, p=pvals, adj=adjp,
                 ci=paste0("[",paste(lls, uls, sep=", "), "]"))
colnames(df) <- c("Dose", "t", "p-value", "adj. p-value", "conf. int.")
pander(df, round=3, split.tables=120,
       caption="*Two-sided comparison of delivery methods by dose*")

library(ggplot2)
ggplot(data=ToothGrowth, aes(y=len, x=supp, fill=supp)) + geom_boxplot() +
  facet_wrap(~ dose, ncol=3) + ylab("Tooth length") + xlab("Delivery method") + 
  ggtitle("Tooth growth by delivery and dose") + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=2) +
  theme_bw()+ theme(legend.position = "none")

s <- subset(ToothGrowth, dose==2)
d <- split(s, s$supp)
n1 <- 10; m1 <- mean(d$OJ$len); s1 <- sd(d$OJ$len)
n2 <- 10; m2 <- mean(d$VC$len); s2 <- sd(d$VC$len)
pooled_sd <-  sqrt( ((n1 - 1) * s1^2 + (n2-1) * s1^2) / (n1 + n2-2))
cat("effect size:", round((m2 - m1)/pooled_sd, 3), "\nestimated sample size:",
    round(power.t.test(power=0.9, delta = m1 - m2, sd=pooled_sd)$n,0))