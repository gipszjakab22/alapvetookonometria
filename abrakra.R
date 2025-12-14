library(ggplot2)
theme_set(
  theme_bw() + 
    theme(
      text = element_text(family = "serif"), 
      plot.title = element_text(face = "bold", hjust = 0.5), 
      legend.position = "bottom"
    )
)
#### Egy, minőségi változó #####################################################
ggplot(data, aes(x = race)) + 
  geom_bar(fill = "steelblue", color = "black") +
  labs(y = "Gyakoriság",x="Rassz", title = "Kategória eloszlás")


data$race  <- factor(data$race)
data$smoke <- factor(data$smoke)

mosaicplot(
  table(data$race, data$smoke),
  main = "Rassz és dohányzás kapcsolata",
  xlab = "Rassz",
  ylab = "Dohányzás",
  color = TRUE
)





#### Egy, mennyiségi változó ###################################################
ggplot(data, aes(x = bwt)) +
  # Hisztogram density skálán, hogy rámehessen a görbe
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 500, color = "black", fill = "white") +
  # Sűrűségfüggvény (vastag vonallal)
  geom_density(aes(y = after_stat(density)),alpha = 0.2, fill = "red") + 
  labs(title = "Hisztogram és sűrűségfüggvény")

#felosztása
geom_histogram(bins = 10, breaks = seq(500, 5500, 1000), binwidth = 500)

ggplot(data, aes(x = race, y = bwt)) +
  geom_boxplot(outlier.shape = NA, fill = "lightgray") +
  geom_jitter(width = 0.15, alpha = 0.5) +  
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3,
    color = "red"
  )


#################timeseries
install.packages("ggfortify")
library(ggfortify)
# ARIMA vagy Time Series objektumra:
autoplot(adat)

ggplot(adat, aes(x = date, y = Fidesz)) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_line(aes(y=Uzleti), color = "red", linewidth = 1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Idősor alakulása", x = "Dátum", y = "Érték")

library(ggfortify)

pca <- prcomp(scale(data[,c(2:3,9:10)]), center = TRUE)
autoplot(pca, data = data, loadings = TRUE, loadings.label = TRUE)

fit <- lm(lwt ~ bwt
          + race, data = data)
autoplot(fit)
checkresiduals(fit)

#arimara
library(forecast)
fc <- forecast(fit, h = 12)
autoplot(fc)

#### Két mennyiségi ############################################################
ggplot(data, aes(x = lwt, y = bwt)) + 
  geom_point(alpha = 0.6) +        
  geom_smooth(method = "lm", se = TRUE, color = "red") 



install.packages("corrplot")
library(corrplot) 
korr_matrix <- cor(data[,c(2:3,10)]) 
corrplot(korr_matrix, method="color")

corrplot(
  korr_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.9,
  diag = FALSE
)

#### Plusszok ##################################################################


#hline vline
+
  geom_hline(yintercept = 50, color = "blue") +
  geom_vline(xintercept = mean(data$race), color = "red", size=2, linetype="dashed") +
  annotate("text", x = 4000, y = 0.0006, label = "Kritikus zóna", color = "red")

+
  labs(
    title = "",
    subtitle = "",
    x = "(%)",
    y = "(%)",
    color = ""
  )

#bontás
facet_wrap(~ race)
facet_grid(var1 ~ var2)




####  Modellekre ###############################################################
hibak <- data.frame(resid = residuals(model))
ggplot(hibak, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title = "QQ-plot (Normál eloszlás ellenőrzése)")

diagnosztika <- data.frame(fitted = fitted(model), resid = residuals(model))

ggplot(diagnosztika, aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Reziduumok a becsült értékek függvényében", 
       x = "Becsült érték (Fitted)", y = "Hiba (Residual)")


install.packages("modelsummary")
library(modelsummary)

modelsummary(
  list(
    "OLS" = lm(mpg ~ wt + hp, mtcars),
    "Logit" = glm(am ~ wt + hp, mtcars, family = binomial)
  ),
  statistic = "({std.error})",
  stars = TRUE
)

##############################################x
library(NbClust)
nb <- NbClust(pc_scores[, c("PC1","PC2","PC3")], distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
opt_k <- nb$Best.nc[1]


###############################xxx###
tab <- table(data$race, data$smoke)

chi <- chisq.test(tab)

# Pearson-reziduumok
res <- chi$residuals
library(tidyr)
library(dplyr)

df_plot <- as.data.frame(tab) |>
  rename(
    race  = Var1,
    smoke = Var2,
    n     = Freq
  ) |>
  mutate(
    expected = as.vector(chi$expected),
    pearson  = as.vector(res)
  )
library(ggplot2)

ggplot(df_plot, aes(x = race, y = smoke)) +
  geom_tile(aes(fill = pearson), color = "white") +
  geom_text(aes(label = round(pearson, 2)), size = 3) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    limits = c(-4, 4),
    name = "Pearson\nreziduum"
  ) +
  labs(
    title = "Rassz és dohányzás kapcsolata",
    subtitle = "Cellánkénti Pearson-féle χ²-reziduumok",
    x = "Rassz",
    y = "Dohányzás"
  ) 