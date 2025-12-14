#### Leíró ####

# Momentumok
library(moments)
skewness() # Ferdeség - 0: szimmetrikus, <0: balra elnyúló
kurtosis(rt(1000, df=1)) # Csúcsosság - 3: normálissal azonos csúcsosságú, 3<: csúcsosabb 

# Leíró
library(psych)
describe() # Sok leíró

library(Hmisc)
smean.cl.normal() # Átlag konfidenciaintervallum

library(gmodels)
CrossTable() # Kereszttábla, minőségi ismérvek kapcsolatához

prop.table(table(Town, Cond)) # Keresztábbla (megoszlás)

by(data[, 2:3], data$Cond, summary) # Leíró adatok bizonyos változóra egy nominális változó mentén

sapply(data[, 3:5], quantile) # Kvantilisek

#### Tesztek ####
t.test(rnorm(1000)-3) # H0: mu=3
t.test(rnorm(1000), rnorm(900, 2, 2), var.equal = FALSE) # H0: mu1 = mu2 (nem egyenlő variancia)

aov(price~condition, data) # Vegyes kapcsolat vizsgálat (normális eloszlások + azonos szórás)

cov() # Kovariancia
cor() # Korreláció

ks.test(data, "pnorm") # Kolmog.-Smornov normalitás teszt

#### Modellekhez kapcsolódó tesztek ####
library(lmtest)
coeftest(model) # Együtthatók tesztelése
waldtest(model1, model2, test="F") # Wald F próba
waldtest(model1, model2, test="Chisq") # LM próba

anova(model1, model2) # Wald F próba

confint(model, level=0.95) # Együtthatók tesztelése

library(broom)
glance(model) # Info kritériumok

library(car)
linearHypothesis(model, c("A=B")) # Modellben szereplő két változó együtthatója megegyezik-e

library(margins)
dydx(data, model, "variable") # Marginális hatás kiszámolása adott variable-re

library(lmtest)
resettest(model) # Ramsey-Resetteszt, H0: a modell jól specifikált

vcov(model) # A modell együtthatóinak kovariancia mátrixa

#### Multikollinearitás ####
library(corrplot)
corrplot(cor(data), method = "color")

library(car)
vif(model) # VIF mutató

diag(solve(cor(data))) # VIF mutató máshogy (korr. mtrx inverzének diag. elemei)

# PCA
scale(data) # Standaradizálás

fokomp = prcomp(data, center = T, scale = T)
cbind(data, fokomp$x[, 1:3])

#### Heteroszkedaszticitás ####
library(skedastic)
white(model, interactions = T)$p.value # White teszt, H0: Homoszkedasztikus

library(lmtest)
bptest(model, studentize = T) # Breusch-Pagan teszt, H0: Homoszkedasztikus

library(car)
library(lmtest)
coeftest(model, vcov = hccm(model)) # Korrigált standard hibákkal számolt p-értékek (White féle korrigált stand. hibák)

library(nlme)
gls(formula, data) # Generalized Least Squares (jó p-értékek)

#### Logisztikus ####
glm(formula, data, family = binomial(link = "logit"))

(summarize(model)$null.deviance - summarize(model)$deviance) / summarize(model)$null.deviance
# McFadden R^2 (milyen messze van a nullmodelltől a modell - nagyobb = jobb)

library(DescTools)
PseudoR2(model, "McFadden") # Máshogy

library(pROC)
csod$valosz = predict(model, data, type="response") # Valószínűségek -> új oszlop 

plot(roc(data$csod, csod$valosz), col="red", main="Csőd modell ROC görbe",
     xlim=c(1,0), ylim=c(0,1), type="l")

auc(roc(data$csod, csod$valosz)) # Terület a ROC görbe alatt (0.8<: jó)

csod$becsles = ifelse(csod$valosz > 0.5, 1, 0) # 0.5-ös cut value

klassz_matrix = table(csod[, c("csod", "becsult")]) # Confusion matrix

precision_1 = klassz_matrix[2,2]/(klassz_matrix[2,2] + klassz_matrix[1,2])
precision_0 = klassz_matrix[1,1]/(klassz_matrix[1,1] + klassz_matrix[2,1])

recall_1 = klassz_matrix[2,2]/(klassz_matrix[2,2] + klassz_matrix[2,1])
recall_0 = klassz_matrix[1,1]/(klassz_matrix[1,1] + klassz_matrix[1,2])

ROC_adatok = roc(csod$csod, csod$valosz)
coords(ROC_adatok, "best", ret = "threshold", best.method = "closest.topleft") # Cut value választása


#### Multinomiális ####
library(nnet)
multinom(formula, data)

# Zolival
setwd("S:/Bevoko")

library(readxl)
ProgData <- read_excel("StackOverflowHungary2020.xlsx")
summary(ProgData)
unique(ProgData$Gender)

#faktorrá kell alakítani
ProgData[,5:9] <- lapply(ProgData[,5:9], as.factor)

#modell az elégedettségre
#multinomiális logisztikus regresszió
levels(ProgData$JobSat)
#itt a semleges lesz a referencia
#átszintezzük a változót
ProgData$JobSat <- relevel(ProgData$JobSat, ref="Very dissatisfied")
install.packages("nnet")
library(nnet)
multimodell <- multinom(JobSat~Age+YearsCodePro+MonthlyHuf, data=ProgData)
summary(multimodell)
#olyan mintha 4 bináris logitunk lenne
#béta értelmezés itt egyszerű
exp(coef(multimodell))
#változók szeparált tesztelése
#Likelihood ratio teszt
library(lmtest)
lrtest(multimodell,"Age")
#H0: nem releváns a változó
#p-érték alapján tényleg nem az
lrtest(multimodell,"YearsCodePro")
#csak 10%-os szignifikanciszint mellett szignifikáns
lrtest(multimodell, "MonthlyHuf")
#ez se szignifikáns

#nem acélos ez a modell
summary(ProgData)
#nem kivétel próbáljuk meg felvenni a többi változót

multimodell <- multinom(JobSat~.-Gender, data = ProgData)
#ha megnéznénk a változókra az lrtest-et, akkor egyik se lenne szignifikáns
#megnézzük, hogy van-e értelme a modellnek

#csinálunk egy nullmodellt
multi_logit_null <- multinom(JobSat~1, data=ProgData)
anova(multimodell,multi_logit_null)
#10% feletti p-értéket kapunk
#a mi modellünk nem jobb, mint az üres modell
#ha 5-ös szorzóval számolunk, akkor max 42 paramétert becsülhetünk
length(coef(multimodell))
#túl sok paramétert becsülünk, ezért nem jó ez a modell