#library
library(tibble)
library(reshape2)
library(vtable)
library(kableExtra)
library(rstatix)

#讀資料
Data <-read.csv("csv(revised)/1-1.csv")

#改格式
Data1 <-subset(Data, select = c(Im,Im.1))
Data1 <-add_column(Data1, id=c(1:nrow(Data1)), .after = 0)
Data1m <- melt(Data1, id=c("id"), measured=c("Im","Im.1"))
names(Data1m) <-c('id','question','score')

#平均數&標準差&boxplot
st(Data1m, group = 'question')
boxplot(score~question, data = Data1m)

#factorized
Data1m$id <-factor(Data1m$id)

#one way repeated anova(If satisfied the assumption of sphericity)
model <- aov(score~factor(question)+Error(factor(id)/factor(question)), data = Data1m)
summary(model)

#事後檢定(pairwise paired t test with bonferroni corection)
# ppt <- with(Data1m,
#  pairwise.t.test(
#    x = score, g = question, paired = TRUE,
#    p.adjust.method = "bonferroni")
# )

#one way repeated anova(Checking the assumption of sphericity)
#model <- anova_test(data = Data1m,dv = score, wid = id, within = question)
#get_anova_table(model)
