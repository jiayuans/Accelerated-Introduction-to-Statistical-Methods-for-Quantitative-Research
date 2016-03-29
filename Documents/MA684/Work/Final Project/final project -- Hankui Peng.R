# 1. explore differences in voter attitudes in two regions

# read data into R
voter <- read.csv("VoterValues2015.csv", header = T, stringsAsFactors = F)
attach(voter)

# load required packages to perform PCA
library(psych)
library(GPArotation)


# 1A. perform a principle component analysis with varimax rotation
compout1 <- princomp(~PrivOwn+GayMarriage+Abortion+GovResp+Compete+AssistSuicide, 
                     cor=T)
summary(compout1)

# scree plot 
screeplot(compout1, type = "line")

# create a data frame containing the 6 variables
vars1 <- data.frame(PrivOwn, GayMarriage, Abortion, 
                    GovResp, Compete, AssistSuicide)

# rotate components to find more interpretable summary measures

# principle component analysis
principal(vars1, nfactors = 2, rotate = 'varimax')

# factor analysis
faout <- fa(vars1, nfactors = 2, rotate = 'varimax')
summary(faout)


# 1B. create subscales
moral <- GayMarriage + Abortion + AssistSuicide
political <- PrivOwn + Compete + GovResp

# Cronbach's alpha
qvars1 <- data.frame(GayMarriage, Abortion, AssistSuicide)
alpha(qvars1)

qvars2 <- data.frame(PrivOwn, Compete, GovResp)
alpha(qvars2)

qvars2 <- data.frame(PrivOwn, Compete)
alpha(qvars2)


# 1C. compare the political viewpoints of voters from the two regions 
library(dplyr)
region1 <- subset(voter, Region==1)
region2 <- subset(voter, Region==2)

r1.moral <- with(region1, GayMarriage + Abortion + AssistSuicide)
r1.political <- with(region1, PrivOwn + Compete)

r2.moral <- with(region2, GayMarriage + Abortion + AssistSuicide)
r2.political <- with(region2, PrivOwn + Compete)

# conduct 2 two sample t-tests
t.test(r1.moral, r2.moral, var.equal = T) # test moral
t.test(r1.political, r2.political, var.equal = T) # test political

#---------------------------------------------------------------------#

# 2 lead study
LeadStudy <- read.csv("LeadStudy2015(1).csv",header=T)
attach(LeadStudy)

# low blood lead levels
summary(LeadStudy$lead=="0") 

# subset low and high lead levels into two data frames
lowlead <- subset(LeadStudy, lead=="0")
highlead <- subset(LeadStudy, lead=="1")

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
t.test(lowlead$age, highlead$age, var.equal = T)

# calculate p-value for sex
chisq.test(table(LeadStudy$sexF, LeadStudy$lead), correct = F)
1-pchisq(0.08742, 1)

# calculate p-value for race
chisq.test(table(LeadStudy$race, LeadStudy$lead), correct = F)
1-pchisq(5.7059, 3)

# 2B.
# percentage of children with ADHD
table(leadstudy$ADHD)
prop.test(table(leadstudy$ADHD), correct = F)

# mean IQ for children in this sample
mean(leadstudy$iq)
sd(leadstudy$iq)


# 2C. mean and sd of IQ for high and low lead levels

# for high lead levels
mean(highlead$iq)
sd(highlead$iq)

# for low lead levels
mean(lowlead$iq)
sd(lowlead$iq)

# comparing two means
t.test(highlead$iq, lowlead$iq, var.equal = T)


# 2D. conduct a multiple linear regression
reg3 <- lm(iq ~ age + factor(sexF) + factor(race) + factor(lead), 
           data = leadstudy)
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
                data = leadstudy, family = binomial(link = "logit"))
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


# 2H. calculate the predicted probability

# without lead exposure
-0.80657-0.02484*10
exp(-1.05497)/(1+exp(-1.05497))

# with lead exposure
-0.80657-0.02484*10+0.97409
exp(-0.08088)/(1+exp(-0.08088))
