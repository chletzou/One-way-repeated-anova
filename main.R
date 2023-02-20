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

Data2 <-subset(Data, select = c(Cos,Cos.1))
Data2 <-add_column(Data2, id=c(1:nrow(Data2)), .after = 0)
Data2m <- melt(Data2, id=c("id"), measured=c("Cos","Cos.1"))
names(Data2m) <-c('id','question','score')

Data3 <-subset(Data, select = c(En,En.1))
Data3 <-add_column(Data3, id=c(1:nrow(Data3)), .after = 0)
Data3m <- melt(Data3, id=c("id"), measured=c("En","En.1"))
names(Data3m) <-c('id','question','score')

Data4 <-subset(Data, select = c(Com,Com.1))
Data4 <-add_column(Data4, id=c(1:nrow(Data4)), .after = 0)
Data4m <- melt(Data4, id=c("id"), measured=c("Com","Com.1"))
names(Data4m) <-c('id','question','score')

#平均數&標準差&boxplot
st(Data1m, group = 'question')
boxplot(score~question, data = Data1m)

st(Data2m, group = 'question')
boxplot(score~question, data = Data2m)

st(Data3m, group = 'question')
boxplot(score~question, data = Data3m)

st(Data4m, group = 'question')
boxplot(score~question, data = Data4m)

# #factorized
Data1m$id <-factor(Data1m$id)
# Data2m$id <-factor(Data2m$id)
# Data3m$id <-factor(Data3m$id)
# Data4m$id <-factor(Data4m$id)

#one way repeated anova(If satisfied the assumption of sphericity)
model1 <- aov(score~factor(question)+Error(factor(id)/factor(question)), data = Data1m)
summary(model1)

model2 <- aov(score~factor(question)+Error(factor(id)/factor(question)), data = Data2m)
summary(model2)

model3 <- aov(score~factor(question)+Error(factor(id)/factor(question)), data = Data3m)
summary(model3)

model4 <- aov(score~factor(question)+Error(factor(id)/factor(question)), data = Data4m)
summary(model4)

#事後檢定(pairwise paired t test with bonferroni corection)
# ppt <- with(Data1m,
#  pairwise.t.test(
#    x = score, g = question, paired = TRUE,
#    p.adjust.method = "bonferroni")
# )

#one way repeated anova(Checking the assumption of sphericity)
#model <- anova_test(data = Data1m,dv = score, wid = id, within = question)
#get_anova_table(model)
