### Libraries used in project
install.packages(c("dplyr","ggplot2","tidyverse","nnet","caret",
                   "readr","stringr","kableExtra","lme4","lmerTest",
                   "car","performance","devtools","MuMIn"))
library(dplyr)
library(ggplot2)
library(tidyverse)
library(nnet)
library(caret)
library(readr)
library(stringr)
library(kableExtra)
library(lme4)
library(lmerTest)
library(car) 
library(MuMIn)
library(performance)



### Läsa in data set.
setwd("C:/Users/timce/OneDrive/Dokument/Rcode_2024_2025/732G56/datamaterial")
data = read.csv("utespelare.csv", sep=";")
data$average.rating <- as.numeric(gsub(",",".",data$average.rating))
only_GK <- data[data$position == "GK",]
only_GK$matchday <- str_extract(only_GK$matchday,"S\\d+")
only_GK$matchday <- as.numeric(str_remove(only_GK$matchday,"S"))
only_GK <- only_GK[,c("matchday","average.rating","name","club","opponent","acc","aer","agg","agi","ant",
                      "bal","bra","cmd","com","cmp","cnt","cor","cro","dec","det","dri","ecc","fin","fir",
                      "fla","fre","han","hea","jum","kic","ldr","lon","l.th","mar","nat","otb","pac","pas",
                      "pen","pos","pun","ref","tro","sta","str","tck","tea","tec","thr","vis","wor")]


# NA värder
na_rows <- only_GK[rowSums(is.na(only_GK[,6:ncol(only_GK)])) == (ncol(only_GK) - 5),]
# filtrera bort alla observationer som innehålla na värder
only_GK <- only_GK[!(rowSums(is.na(only_GK[, 6:ncol(only_GK)])) == (ncol(only_GK) - 5)), ]

# Identifiera numeriska kolumner
numeric_cols <- sapply(only_GK, is.numeric)
# Uteslut 'matchday' från de numeriska kolumnerna
numeric_cols["matchday"] <- FALSE
# Filtrera bort rader där något av de utvalda numeriska värdena är < 5
df_filtered <- only_GK[!apply(only_GK[ , numeric_cols] < 5, 1, any), ]
# Spara alla observationer som innehålla värdena som mindre än 5
df_removed <- only_GK[apply(only_GK[ , numeric_cols] < 5, 1, any), ]


beskrivning_data <- df_filtered[,-1]
# Välj bara numeriska kolumner
numeric_df <- beskrivning_data[sapply(beskrivning_data, is.numeric)]
numeric_df <- numeric_df %>%
  select(-c("cor", "cro", "dri", "fin", "hea", "lon", "l.th", "mar", "nat", "sta", "tck"))


# Skapa en data.frame med medelvärde, min och max för varje variabel
summary_stats <- data.frame(
  Medelvärde = sapply(numeric_df, mean, na.rm = TRUE),
  StdAvvikelse = sapply(numeric_df, sd,na.rm = TRUE),
  Minimum    = sapply(numeric_df, min, na.rm = TRUE),
  Maximum    = sapply(numeric_df, max, na.rm = TRUE)
)

# Visa resultatet
summary_stats %>%
  round(3)%>%
  kable()

# hist() för målvakters medelmatchbetyg
ggplot(data = df_filtered, aes(x = average.rating)) + 
  geom_histogram(bins = 15, color = "black", fill = "blue") +
  labs(title = "Histogram över average.rating",
       x = "average.rating",
       y = "Frekvens") + theme_bw()

### fullständig modell
fullständig_model <- lmer(average.rating~acc+aer+agg+agi+ant+bal+bra+cmd+com+cmp+cnt+
                     dec+det+ecc+fir+fla+fre+han+jum+kic+
                     ldr+otb+pac+pas+pen+pos+pun+ref+tro+
                     str+tea+tec+thr+vis+wor+(1|name),
                   data=df_filtered, control = lmerControl(optCtrl = list(maxfun = 100000)),
                   REML = FALSE,
                   na.action = na.fail)

r2(fullständig_model)
AIC(fullständig_model)
summary(fullständig_model)
# Beräkna GVIF och justerad GVIF värder
vif(fullständig_model) %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() %>% 
  kable(
    digits = 4
  )

# båkateliminering
lmerTest::step(fullständig_model)
get_model(step(fullständig_model))


### Funktionen för att skapa en kombination av 4 olika plot för residualanalys
### Funktionen kräver endast ett argument, modellen som anpassats
residualPlots <- function(model) {
  residualData <- 
    data.frame(
      residuals = residuals(model),
      # Responsvariabeln finns som första kolumn i modellens model-objekt
      
      yHat = fitted(model)
    )
  p1 <- ggplot(residualData) + 
    aes(x = residuals, y = after_stat(density)) +
    geom_histogram(bins = 20, fill = "steelblue", color = "black") + 
    theme_bw() + 
    labs(x = "Residualer", y = "Densitet")
  p2 <- ggplot(residualData) + 
    aes(x = yHat, y = residuals) + 
    geom_hline(aes(yintercept = 0)) + 
    geom_point(color = "steelblue") + 
    theme_bw() +
    labs(x = "Anpassade värden", y = "Residualer")
  p3 <- ggplot(residualData) + 
    # Använder standardiserade residualer
    aes(sample = scale(residuals)) + 
    geom_qq_line() + 
    geom_qq(color = "steelblue") +
    theme_bw() + 
    labs(x= "Teoretiska kvantiler", y = "Observerade kvantiler")
  p4 <- ggplot(residualData) + 
    aes(x = 1:nrow(residualData), y = residuals) + 
    geom_line(color = "steelblue") + 
    theme_bw() +
    labs(x = "Obs. index", y = "Residualer") + 
    geom_hline(
      aes(yintercept = 0),
      color = "black")
  cowplot::plot_grid(p1, p2, p3,p4, nrow = 2)
}









### reducerad modell1
reducerad_model1 <- lm(average.rating ~ acc + agg + agi + bra + cmd + cnt + det + 
                        ecc + fir + fre + han + otb + pac + ref,
                      data=df_filtered)

summary(reducerad_model1)
r2(reducerad_model1)
AIC(reducerad_model1)

# residual plot
residualPlots(reducerad_model1)



### reducerad modell2
reducerad_model2 <- lmer(average.rating ~ acc + agg + agi + bra + cmd + cnt + det + 
                          ecc + fir + fre + han + otb + pac + ref + (1|name),
                        data=df_filtered,
                        REML = FALSE,
                        na.action = na.fail)

summary(reducerad_model2)
AIC(reducerad_model2)
r2(reducerad_model2)

# Residual plot
residualPlots(reducerad_model2)





























