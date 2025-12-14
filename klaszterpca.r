############################################################
# K-MEANS + OPTIMÁLIS K + PCA-KLASZTEREZÉS
# ADAT: FAT_Data.xlsx (BodyFat jellegű adatok)
############################################################

### 0. CSOMAGOK BETÖLTÉSE ----
# install.packages(c("readxl", "dplyr", "ggplot2",
#                    "factoextra", "cluster", "showtext"))

library(readxl)
library(dplyr)
library(ggplot2)
library(factoextra)   # fviz_nbclust, fviz_cluster, fviz_eig
library(cluster)      # silhouette
library(showtext)     # Times New Roman

### 0.1. BETŰTÍPUS (TIMES NEW ROMAN) ----
# Ha hiba van az elérési úttal, a base_family-t kommentezd ki a ggplot-oknál.
font_add(family = "TNR", regular = "times.ttf")
showtext_auto()

############################################################
# 1. ADAT BEOLVASÁSA
############################################################
adat <- read_excel("FAT_Data.xlsx")

# Klaszterezéshez használt változók
valtozok_klaszter <- c(
  "Fat", "Age", "Weight", "Height", "BMI",
  "Neck", "Chest", "Abdomen", "Hip", "Thigh",
  "Knee", "Ankle", "Biceps", "Forearm", "Wrist"
)

# Ellenőrzés: minden oszlop létezik-e
stopifnot(all(valtozok_klaszter %in% names(adat)))

# Csak ezeket a változókat vesszük ki, és kidobjuk az NA-s sorokat
adat_num <- adat %>%
  select(all_of(valtozok_klaszter)) %>%
  na.omit()

# Standardizálás (nagyon fontos k-meansnél)
adat_scaled <- scale(adat_num)

############################################################
# 2. OPTIMÁLIS KLASZTERSZÁM (ELBOW + SILHOUETTE)
############################################################

## 2.1. Elbow (WSS) módszer, itt a klaszterátlagoktól vett távolságok minimalizálása
fviz_nbclust(adat_scaled, kmeans, method = "wss") +
  labs(
    title = "Optimális klaszterszám – Elbow (BodyFat adatok)",
    x     = "Klaszterek száma (k)",
    y     = "Összes within-cluster szórásnégyzet (WSS)"
  ) +
  theme_minimal(base_family = "TNR") +
  theme(plot.title = element_text(size = 18, face = "bold"))

## 2.2. Silhouette módszer, klasztereken belüli átlagos euklidészi távolságot minimalizál
fviz_nbclust(adat_scaled, kmeans, method = "silhouette") +
  labs(
    title = "Optimális klaszterszám – Silhouette (BodyFat adatok)",
    x     = "Klaszterek száma (k)",
    y     = "Átlagos sziluett érték"
  ) +
  theme_minimal(base_family = "TNR") +
  theme(plot.title = element_text(size = 18, face = "bold"))

############################################################
# 3. K-MEANS KLASZTEREZÉS AZ EREDETI (STANDARDIZÁLT) VÁLTOZÓKON
############################################################

# Itt kézzel választod a k-t az előző ábrák alapján (pl. 3 vagy 4)
k_raw <- 3   # ← ezt írd át arra, ami neked optimálisnak tűnik

set.seed(123)
kmeans_raw <- kmeans(adat_scaled, centers = k_raw, nstart = 25)

# Klasztercímkék hozzáadása az adathoz
adat_num$cluster_raw <- factor(kmeans_raw$cluster)

# Klaszterek vizualizálása (fviz_cluster PCA-alapú 2D vetítést használ)
fviz_cluster(kmeans_raw, data = adat_scaled,
             geom = "point", repel = TRUE) +
  labs(
    title = paste0("K-means klaszterek (eredeti mutatók, k = ", k_raw, ")")
  ) +
  theme_minimal(base_family = "TNR") +
  theme(plot.title = element_text(size = 18, face = "bold"))

############################################################
# 4. PCA (FŐKOMPONENS-ELEMZÉS)
############################################################

pca_mod <- prcomp(adat_scaled, center = TRUE, scale. = TRUE)

###
pca_mod$rotation
round(pca_mod$rotation[, 1:3], 3)
fviz_pca_var(pca_mod,
             col.var = "contrib",
             gradient.cols = c("lightblue", "yellow", "red"),
             repel = TRUE) +
  theme_minimal(base_family = "TNR") +
  theme(plot.title = element_text(size = 18, face = "bold"))

# Magyarázott variancia komponensenként
fviz_eig(pca_mod) +
  labs(
    title = "PCA komponensek magyarázott varianciája (BodyFat)",
    x = "Főkomponens (PC)",
    y = "Magyarázott variancia (%)"
  ) +
  theme_minimal(base_family = "TNR") +
  theme(plot.title = element_text(size = 18, face = "bold"))

# PCA-score-ok (PC1, PC2, PC3, ...)
pca_data <- as.data.frame(pca_mod$x)
pca_data
# Klaszterezéshez az első 3 főkomponenst használjuk
pca_data_sub <- pca_data[, 1:3]

############################################################
# 5. OPTIMÁLIS KLASZTERSZÁM PCA-ADATOKON (ELBOW + SILHOUETTE)
############################################################

fviz_nbclust(pca_data_sub, kmeans, method = "wss") +
  labs(
    title = "Optimális klaszterszám PCA-adatokon – Elbow",
    x = "Klaszterek száma (k)",
    y = "WSS"
  ) +
  theme_minimal(base_family = "TNR") +
  theme(plot.title = element_text(size = 18, face = "bold"))

fviz_nbclust(pca_data_sub, kmeans, method = "silhouette") +
  labs(
    title = "Optimális klaszterszám PCA-adatokon – Silhouette",
    x = "Klaszterek száma (k)",
    y = "Átlagos sziluett"
  ) +
  theme_minimal(base_family = "TNR") +
  theme(plot.title = element_text(size = 18, face = "bold"))

############################################################
# 6. K-MEANS KLASZTEREZÉS A PCA-KOMPONENSEKEN
############################################################

# Itt is választasz k-t a PCA-s elbow/silhouette alapján
k_pca <- 4   # ← ezt is nyugodtan módosíthatod

set.seed(123)
kmeans_pca <- kmeans(pca_data_sub, centers = k_pca, nstart = 25)

# Klasztercímkék a PCA-adatokhoz
pca_data_sub$cluster_pca <- factor(kmeans_pca$cluster)

# Klaszterek ábrázolása a PCA-térben (PC1–PC2)
fviz_cluster(kmeans_pca, data = pca_data_sub,
             geom = "point", repel = TRUE) +
  labs(
    title = paste0("K-means klaszterek PCA-adatokon (k = ", k_pca, ")"),
    x = "PC1",
    y = "PC2"
  ) +
  theme_minimal(base_family = "TNR") +
  theme(plot.title = element_text(size = 18, face = "bold"))

############################################################
# A script végére:
#  - adat_num$cluster_raw       → klaszter az eredeti változókon
#  - pca_data_sub$cluster_pca   → klaszter az első 3 főkomponens alapján
############################################################
