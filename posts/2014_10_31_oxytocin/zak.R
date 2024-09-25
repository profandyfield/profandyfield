
library(pastecs)
library(BayesFactor)


placebo<-c(3, 3, 4, 4, 4, 5, 6, 6, 6, 6, 6, 7, 7, 8, 8, 9, 9, 11, 11, 11, 12, 12, 12, 12, 12, 12)
oxy<-c(3, 4, 4, 6, 6, 7, 8, 8, 8, 8, 9, 9, 10, 10, 10, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12)

round(stat.desc(placebo, basic = F), 1)
round(stat.desc(oxy, basic = F), 1)

placebo<-c(3, 3, 4, 4, 4, 5, 6, 6, 6, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 10, 11, 11, 11, 12, 12, 12, 12, 12, 12)

round(stat.desc(placebo, basic = F), 1)

zak<-data.frame(gl(2, 29), c(placebo, oxy))
names(zak)<-c("Group", "MU")
zak$Group<-factor(zak$Group, labels = c("Placebo", "Oxytocin"))

wilcox.test(MU~Group, zak)

yuenbt(placebo, oxy, tr=.2, alpha=.05, nboot = 2000, side = T)
pb2gen(placebo, oxy, alpha=.05, nboot=2000, est=median)

d<-(mean(oxy)-mean(placebo))/sd(placebo)
ttestBF(oxy, y = placebo)


