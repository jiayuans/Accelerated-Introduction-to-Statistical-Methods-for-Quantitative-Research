library(psych)
library(GPArotation)
dm <- read.csv("drinkingmotives2007.csv",header=T)
attach(dm)
head(dm)
compout <- princomp(~D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+
                      D11+D12+D13+D14+D15+D16+D17+D18+D19+D20,
                    cor=TRUE)
summary(compout)
vars <- data.frame(D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, 
                   D11, D12, D13, D14, D15, D16, D17, D18, D19, D20)
principal(vars,nfactors=3,rotate="varimax")


havafun <- D3+D5+D7+D9+D10+D11+D13+D14+D15+D16+D18
besocial <- D2+D8+D12+D19+D20
cheerup <- D1+D4+D6+D17

mean(havafun)
mean(besocial)
mean(cheerup)

sd(havafun)
sd(besocial)
sd(cheerup)

max(havafun)-min(havafun)
max(besocial)-min(besocial)
max(cheerup)-min(besocial)

lm.havefun <- lm(havafun~AgeDrink)
summary(lm.havefun)
lm.besocial <- lm(besocial~AgeDrink)
summary(lm.besocial)
lm.cheerup <- lm(cheerup~AgeDrink)
summary(lm.cheerup)

