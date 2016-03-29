# 1. explore differences in voter attitudes in two regions

# read data into R
voter <- read.csv("VoterValues2015.csv", header = T, stringsAsFactors = F)
attach(voter)

# load required packages to perform PCA
library(psych)
library(GPArotation)

# 1A. perform a principle component analysis with varimax rotation
compout1 <- princomp(~PrivOwn+GayMarriage+Abortion+GovResp+Compete+AssistSuicide)
summary(compout1)

# create a data frame containing the 6 variables
vars1 <- data.frame(PrivOwn, GayMarriage, Abortion, GovResp, Compete, AssistSuicide)

# rotate components to find more interpretable summary measures
principal(vars1, nfactors = 4, rotate = 'varimax')

# 1B. create subscales
liberal1 <- GayMarriage + Abortion
liberal2 <- Abortion + AssistSuicide
conservative <- GovResp
individual <- PrivOwn + Compete

# Cronbach's alpha
qvars1 <- data.frame(GayMarriage, Abortion)
alpha(qvars1)

qvars2 <- data.frame(Abortion, AssistSuicide)
alpha(qvars2)

qvars3 <- data.frame(PrivOwn, Compete)
alpha(qvars3)

# 1C. comopare voters from two regions
# run a set of simple regressions
voter$Region = voter$Region - 1

regout1 <- lm(liberal1 ~ factor(Region))
summary(regout1)

regout2 <- lm(liberal2 ~ factor(Region))
summary(regout2)

regout3 <- lm(conservative ~ factor(Region))
summary(regout3)

regout4 <- lm(individual ~ factor(Region))
summary(regout4)



# 2 lead study
leadstudy <- read.csv("LeadStudy2015(1).csv", header = T, stringsAsFactors = F)
attach(leadstudy)

# low blood lead levels
summary(leadstudy$lead=="0") 

# subset low and high lead levels into two data frames
lowlead <- subset(leadstudy, lead=="0")
highlead <- subset(leadstudy, lead=="1")

# in low lead levels column
mean(lowlead$age)
sd(lowlead$age)

# sex
summary(lowlead$sexF=="1")
176/373
197/373

# race
table(lowlead$race)
198/373
65/373
50/373
60/373


# in high lead levels column
mean(highlead$age)
sd(highlead$age)

# sex
table(highlead$sexF)
58/127 # male
69/127 # female

# race
table(highlead$race)
54/127
26/127
26/127
21/127

# calculate p-value for age
t.test(age ~ lead, data = leadstudy)

# calculate p-value for sex
mat <- matrix(c(176,197,58,69), ncol = 2)
mat
chisq.test(mat)

# calculate p-value for race
t.test(race ~ lead, data = leadstudy)


# run a multiple logistic regression
logout2 <- glm(lead ~ age + factor(race) + factor(sexF), 
               family = binomial(link = 'logit'))
summary(logout2)


# 2B.
# percentage of children with ADHD
table(ADHD)
102/500

# mean IQ for children in this sample
mean(leadstudy$iq)
sd(leadstudy$iq)


# 2C. mean and sd of IQ

# for high lead levels
mean(highlead$iq)
sd(highlead$iq)

# for low lead levels
mean(lowlead$iq)
sd(lowlead$iq)

# comparing two means
t.test(highlead$iq, lowlead$iq, var.equal = T)


# 2D. conduct a multiple linear regression
reg3 <- lm(iq ~ age + factor(sexF) + factor(race) + factor(lead), data = leadstudy)
summary(reg3)

# give a type I break down ANOVA
summary.aov(reg3)

# calculate the p-value
1-pf(2.713, 6, 499)


# 2F. 
# percent with ADHD in high lead levels 
table(highlead$ADHD)
40/127
# percent with ADHD in low lead levels
table(lowlead$ADHD)
62/373

# conduct a chi-square test
attach(leadstudy)
chisq.test(table(lead, ADHD), correct = F)
1-pchisq(12.908, 1)


# 2G. model ADHD from age, sex, race, and lead exposure 

# run a multiple logistic regression
logout.g <- glm(ADHD ~ age + factor(sexF) + factor(race) + factor(lead), 
                 family = binomial(link = "logit"))
summary(logout.g)

# convert the slopes to odds ratios
exp(coef(logout.g))

# 95% confidence interval
exp(confint(logout.g))

# calculate the p-value
1 - pchisq(36.88, 6)

# calculate the C-statistic
library(epicalc)
lroc(logout.g)$auc

