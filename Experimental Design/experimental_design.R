#AR_paper <- read.csv("~/Desktop/AR_paper.csv")
AR_paper <- read.csv("C:/Users/Tongfang/Desktop/HCDE 516 Final Project/data.csv")
ar.t1 <- na.omit(AR_paper$AR.Task.1)
ar.task1 <- as.numeric(na.omit(AR_paper$AR.Task.1))
paper.task1 <- as.numeric(na.omit(AR_paper$Paper.Task.1))

ar.task2 <- as.numeric(na.omit(AR_paper$AR.Task.2))
paper.task2 <- as.numeric(na.omit(AR_paper$Paper.Task.2))

gender <- as.factor((AR_paper$What.is.your.sex.))
experience <- as.factor(AR_paper$Experiene.with.VR.or.AR)



# remove outlier
boxplot(ar.task1, paper.task1)
new_ar.task1 <- sort(ar.task1)
ar.task1 <- new_ar.task1[1:(length(new_ar.task1) - 1)]
new_paper.task1 <- sort(paper.task1)
paper.task1 <- new_paper.task1[1:(length(new_paper.task1) - 1)]
boxplot(ar.task1, paper.task1)

dat <- data.frame(xx = c(ar.task1, paper.task1),yy = c(rep(0, length(ar.task1)), rep(1, length(paper.task1))))

library(ggplot2)

par(mfrow=c(1,2))

ggplot(dat,aes(x=xx)) + 
  geom_histogram(data=subset(dat,yy == 0),fill = "red", alpha = 0.4, binwidth = 3) +
  geom_histogram(data=subset(dat,yy == 1),fill = "#00bfc4" , alpha = 0.4, binwidth = 3) + theme_bw()


p <- ggplot(data = dat, aes(factor(yy), xx))
p + geom_boxplot(aes(fill = factor(yy))) + theme_bw()


boxplot(ar.task2, paper.task2)
new_ar.task2 <- sort(ar.task2)
ar.task2 <- new_ar.task2[1:(length(new_ar.task2) - 3)]
boxplot(ar.task2, paper.task2)


dat2 <- data.frame(xx = c(ar.task2, paper.task2),yy = c(rep(0, length(ar.task2)), rep(1, length(paper.task2))))

ggplot(dat2,aes(x=xx)) + 
  geom_histogram(data=subset(dat2,yy == 0),fill = "red", alpha = 0.4, binwidth = 10) +
  geom_histogram(data=subset(dat2,yy == 1),fill = "#00bfc4" , alpha = 0.4, binwidth = 10) + theme_bw()


p <- ggplot(data = dat2, aes(factor(yy), xx))
p + geom_boxplot(aes(fill = factor(yy))) + theme_bw()

## Arrange
media.task1 <- as.factor(c(rep(0, length(ar.task1)), rep(1, length(paper.task1))))
time.task1 <- c(ar.task1, paper.task1)

boxplot(ar.task1, paper.task1)

#Task 1 (paper vs AR)
mean(ar.task1)
sd(ar.task1)

lm.1 <- kruskal.test(time.task1~media.task1)
lm.1
lm.n1 <- lm(time.task1~media.task1)
summary(lm.n1)
anova(lm.n1)


#Task2 (paper vs AR)
media.task2 <- as.factor(c(rep(0, length(ar.task2)), rep(1, length(paper.task2))))
time.task2 <- c(ar.task2, paper.task2)

media.task2 <- as.factor(c(rep(0, length(ar.task2)), rep(1, length(paper.task2))))
time.task2 <- c(ar.task2, paper.task2)

lm.2 <- lm(time.task2~media.task2)
anova(lm.2)

lm.2n <- kruskal.test(time.task2~media.task2)
lm.2n

boxplot(ar.task2, paper.task2)


# Total
## Media
ar_all <- c(ar.task1, ar.task2)
paper_all <- c(paper.task1, paper.task2)
#categorical

time <- c(ar_all, paper_all)
time

media <- as.factor(c(rep(0, length(ar_all)), c(rep(1, length(paper_all)))))
task <- as.factor(c(rep(1, length(ar.task1)), c(rep(2, length(ar.task2))), c(rep(1, length(paper.task1))), c(rep(2, length(paper.task2)))))

lm.all <- lm(time~media+task)
anova(lm.all)

lm.3 <- lm(time~media+task+media:task)
anova(lm.3)

# lm.4 <- lm(time~media:task)
# summary(lm.4)


# Experience with AR show on task 1, 2
ar.task1 <- as.numeric((AR_paper$AR.Task.1))
expAR1 <- data.frame(ar.task1, experience)
exoAR1 <- na.omit((expAR1))
boxplot(ar.task1~experience, data = exoAR1)

ar.task2 <- as.numeric((AR_paper$AR.Task.2))
expAR2 <- data.frame(ar.task2, experience)
exoAR2 <- na.omit((expAR2))
boxplot(ar.task2~experience, data = exoAR2)

arrange_task <- as.factor(c(rep(0, length(exoAR1$ar.task1)), rep(1, length(exoAR2$ar.task2))))
time_ar <- c(exoAR1$ar.task1, exoAR2$ar.task2)
exp_all <- as.factor(c(exoAR1$experience, exoAR2$experience))

ar_exp_t <- data.frame(time_ar, arrange_task, exp_all)

#task 1 AR
temp1 <- ar_exp_t$time_ar[1:18]
mean(temp1)
sd(temp1)



#task 1 AR
temp2 <- ar_exp_t$time_ar[19:35]
mean(temp2)
sd(temp2)


lm.4 <- lm(time_ar~arrange_task+exp_all)
anova(lm.4)


# Gender Influence on AR time

ar.task1 <- as.numeric((AR_paper$AR.Task.1))
genAR1 <- data.frame(ar.task1, gender)
genoAR1 <- na.omit((genAR1))
boxplot(ar.task1~gender, data = genoAR1)

ar.task2 <- as.numeric((AR_paper$AR.Task.2))
genAR2 <- data.frame(ar.task2, gender)
genoAR2 <- na.omit((genAR2))
boxplot(ar.task2~gender, data = genoAR2)

arrange_task <- as.factor(c(rep(0, length(genoAR1$ar.task1)), rep(1, length(genoAR2$ar.task2))))
time_ar <- c(genoAR1$ar.task1, genoAR2$ar.task2)
gen_all <- as.factor(c(genoAR1$gender, genoAR2$gender))
gender_table <- data.frame(time_ar, arrange_task, gen_all)

lm.5 <- lm(time_ar~arrange_task+gen_all, data = gender_table)
anova(lm.5)


library(ggplot2)
boxExp <- ggplot(data = gender_table, aes(x=arrange_task, y=time_ar)) + geom_boxplot(aes(fill=gen_all)) + ylab("time consuming")
boxExp + theme_bw()

# library(PWR) effect size
library(pwr)
data = c(ar.task2, paper.task2)
SD = sd(data)
data1 = c(ar.task1, paper.task1)
SD1 = sd(data1)
pwr.t.test(d=(mean(ar.task2)- mean(paper.task2))/SD, power=.8, sig.level=.05, type = "two.sample", alternative = "two.sided")
pwr.t.test(d=(mean(ar.task1)- mean(paper.task1))/SD1, power=.8, sig.level=.05, type = "two.sample", alternative = "two.sided")

