library(plm)
library(lmtest)
library(sandwich)
library(car)
library(dplyr)
library(readxl)
setwd("")
adat = read_excel("")
# adat <- read.csv("sajat_adat.csv")

# DEMÓ: feltételezzük, hogy már van egy 'adat' nevű data frame:
# Változók:
# id   = egyed (pl. ország, cég, személy)
# time = idő (év)
# y    = függő változó
# x1,x2,x3 = magyarázó változók

adat$year   <- as.factor(adat$year)
adat$JARAS_NEV <- as.factor(adat$JARAS_NEV)

#Panel adatszerkezet létrehozása
panel_adat <- pdata.frame(adat, index = c("JARAS_NEV", "year"))

# Fix hatású modell
mod_fe <- plm(szja ~ kozut + gepkocsi + tanar + lakasar + internet + vallalkozas,
              data = panel_adat,
              model = "within")
summary(mod_fe)
##within csak a belso variancia
##pld:Ha egy adott járásban egyik évről a másikra nő az autók száma,
#akkor ugyanabban a járásban az SZJA alap éven belül átlagosan 8.38 egységgel nő.

# Random hatású modell
mod_re <- plm(szja ~ kozut + gepkocsi + tanar + lakasar + internet + vallalkozas,
              data = panel_adat,
              model = "random")
summary(mod_re)
###belso es kulso variancia
###Azok a járások, ahol nagyobb az autók száma,
#általában magasabb SZJA értékeket mutatnak, és
#egy járáson belül az autók számának növekedése is együtt jár az SZJA növekedésével.”

###ha az egyedhatások és a magyarázóváltozók között van korreláció akkor fixhatású
# Hausman-teszt: FE vs RE
hausman <- phtest(mod_fe, mod_re)
print(hausman)

###H0: random hatás van a modellben
### p <5%, akkor H0_t elvetem
### p>5%, nem vetem el, és a random a modell
## 3. MULTIKOLLINEARITÁS VIZSGÁLATA ÉS KEZELÉSE ----
# Panelnél a VIF-et általában egy pooled OLS modellen nézik
# itt add meg, mely változókat szeretnéd vizsgálni
valtozok <- c("szja", "kozut" , "gepkocsi" , "tanar", "lakasar" , "internet" , "vallalkozas")

ev <- 2018

adat_ev <- adat %>% 
  filter(year == ev) %>% 
  select(all_of(valtozok))

corr_mat <- cor(adat_ev, use = "complete.obs")
corr_mat
library(reshape2)

corr_long <- melt(corr_mat)

head(corr_long)
library(ggplot2)

ggplot(corr_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "lightblue",
    mid = "white",
    high = "salmon",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = paste0(ev, " évi korrelációs mátrix"),
    x = "",
    y = "",
    fill = "Korreláció"
  ) +
  theme_minimal(base_family = "TNR") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold")
  )


mod_ols <- lm(szja ~ kozut + gepkocsi + tanar + lakasar + internet + vallalkozas
  ,   # ugyanaz a specifikáció
  data = adat
)


vif_ertekek <- vif(mod_ols)
print(vif_ertekek)


## 4. HETEROSZKEDASZTICITÁS VIZSGÁLATA ----

# Klasszikus Breusch–Pagan-teszt (pooled OLS modellen)
bp_teszt <- bptest(
  szja ~ kozut + gepkocsi + tanar + lakasar + internet + vallalkozas,
  data = adat
)
print(bp_teszt)
#nullhip: homo

## 5. HETEROSZKEDASZTICITÁS KEZELÉSE: ROBUSZTUS HIBÁK ----

# (a) FE modell robusztus (panel HC1, csoportonként klaszterezett) hibákkal
fe_robust <- coeftest(
  mod_fe,
  vcov = vcovHC(mod_fe, type = "HC1", cluster = "group")
)
print(fe_robust)

# (b) Ha autokorreláció is gyanús, használhatunk Driscoll–Kraay hibákat:
# Ehhez a panel 'time' dimenzió is kellően nagy legyen

fe_driscoll <- coeftest(
  mod_fe,
  vcov = vcovSCC(mod_fe, type = "HC1", maxlag = 2)  # maxlag ízlés szerint
)
print(fe_driscoll)

## 6. AUTOKORRELÁCIÓ TESZT (EXTRA, DE HASZNOS) ----

# Panel Breusch–Godfrey teszt (Wooldridge-féle) a plm-ből
pbg <- pbgtest(mod_fe)
print(pbg)

#############################################
# A LEGJOBB EREDMÉNYEKHEZ:
# - Válaszd ki a megfelelő modellt (Hausman-teszt alapján)
# - Kezeld a multikollinearitást (VIF alapján változó elhagyás / transzformáció)
# - Használj robusztus hibákat heteroszkedaszticitás/autokorreláció esetén
#############################################


####ábra:
# Telepítés egyszer kell
# install.packages("showtext")

library(ggplot2)
library(showtext)

# Times New Roman betöltése
font_add(family = "TNR", regular = "times.ttf")
showtext_auto()
###pontdiagram
ggplot(adat, aes(x = szja, y = kozut)) +
  geom_point(color = "steelblue3", alpha = 0.6, size = 3) +
  labs(title = "x1 és y kapcsolata",
       x = "szja", y = "kozut") +
  theme_minimal(base_family = "TNR") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
###
ggplot(adat, aes(x = year, y = szja, group = JARAS_NEV, color = JARAS_NEV)) +
  geom_line(alpha = 0.7, size = 1) +
  scale_color_brewer(palette = "Pastel1") +
  labs(title = "Y időbeli alakulása országonként",
       x = "ÉV", y = "Y") +
  theme_minimal(base_family = "TNR") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = "bottom",
    axis.text = element_text(size = 12)
  )
##hiszt
ggplot(adat, aes(x = szja)) +
  geom_histogram(bins = 30, fill = "thistle3", color = "white", alpha = 0.8) +
  labs(title = "x1 eloszlása") +
  theme_minimal(base_family = "TNR") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 12)
  )
ggplot(adat, aes(x = id, y = y)) +
  geom_boxplot(fill = "lightblue2", color = "grey40", alpha = 0.8) +
  labs(title = "Y eloszlása országonként",
       x = "Ország", y = "Y") +
  theme_minimal(base_family = "TNR") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  )
library(tidyr)

adat_long <- pivot_longer(
  adat,
  cols = c(x1, x2, x3),
  names_to = "Valtozo",
  values_to = "Ertek"
)

ggplot(adat_long, aes(x = time, y = Ertek, color = Valtozo)) +
  geom_line(size = 1, alpha = 0.8) +
  scale_color_brewer(palette = "Pastel2") +
  labs(title = "Több változó időbeli alakulása",
       x = "Év", y = "Érték") +
  theme_minimal(base_family = "TNR") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = "bottom"
  )

