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
require(devtools)
# install the stable version
devtools::install_github("dustinfife/flexplot")
# install the development version
devtools::install_github("dustinfife/flexplot", ref="development")
library(flexplot)



### Läsa in data set.
setwd("X:/732G56_project/datamaterial")
data = read.csv("utespelare.csv", sep=";")
data$average.rating <- as.numeric(gsub(",",".",data$average.rating))
only_GK <- data[data$position == "GK",]
only_GK$matchday <- str_extract(only_GK$matchday,"S\\d+")
only_GK$matchday <- as.numeric(str_remove(only_GK$matchday,"S"))
only_GK <- only_GK[,c("matchday","average.rating","name","club","opponent","acc","aer","agg","agi","ant",
                      "bal","bra","cmd","com","cmp","cnt","cor","cro","dec","det","dri","ecc","fin","fir",
                      "fla","fre","han","hea","jum","kic","ldr","lon","l.th","mar","nat","otb","pac","pas",
                      "pen","pos","pun","ref","tro","sta","str","tck","tea","tec","thr","vis","wor")]



na_rows <- only_GK[rowSums(is.na(only_GK[,6:ncol(only_GK)])) == (ncol(only_GK) - 5),]
View(na_rows)

only_GK <- only_GK[!(rowSums(is.na(only_GK[, 6:ncol(only_GK)])) == (ncol(only_GK) - 5)), ]


# Identifiera numeriska kolumner
numeric_cols <- sapply(only_GK, is.numeric)

# Uteslut 'matchday' från de numeriska kolumnerna
numeric_cols["matchday"] <- FALSE

# Filtrera bort rader där något av de utvalda numeriska värdena är < 5
df_filtered <- only_GK[!apply(only_GK[ , numeric_cols] < 5, 1, any), ]



beskrivning_data <- df_filtered[,-1]
# Välj bara numeriska kolumner
numeric_df <- beskrivning_data[sapply(beskrivning_data, is.numeric)]

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
  kable() %>%
  kable_styling("striped")






table(df_filtered$acc)
table(df_filtered$mar)
table(df_filtered$tck)
table(df_filtered$aer)
table(df_filtered$name)
table(df_filtered$wor)

"cor,cro,dri,fin,hea,lon,l.th,mar,nat,sta,tck"




sd(df_filtered$hea)


model_test <- lmer(average.rating~agg+(1|name),
                   data=df_filtered, control = lmerControl(optCtrl = list(maxfun = 100000)),
                   REML = FALSE,
                   na.action = na.fail)
visualize(model_test, plot="model", sample = 53, formula = average.rating ~  + name)


### fullständig modell
model_test <- lmer(average.rating~acc+aer+agg+agi+ant+bal+bra+cmd+com+cmp+cnt+
                     dec+det+dri+ecc+fir+fla+fre+han+hea+jum+kic+
                     ldr+mar+otb+pac+pas+pen+pos+pun+ref+tro+sta+
                     str+tck+tea+tec+thr+vis+wor+(1|name),
                   data=df_filtered, control = lmerControl(optCtrl = list(maxfun = 100000)),
                   REML = FALSE,
                   na.action = na.fail)

model_test <- lmer(average.rating~acc+aer+agg+agi+ant+bal+bra+cmd+com+cmp+cnt+
                     dec+det+ecc+fir+fla+fre+han+jum+kic+
                     ldr+otb+pac+pas+pen+pos+pun+ref+tro+
                     str+tea+tec+thr+vis+wor+(1|name),
                   data=df_filtered, control = lmerControl(optCtrl = list(maxfun = 100000)),
                   REML = FALSE,
                   na.action = na.fail)

vif(model_test)

r2(model_test)
AIC(model_test)
summary(model_test)

write.csv(df_filtered, file = "df_filtered_export.csv", row.names = FALSE)

###Model1
model1 <- lmer(average.rating~acc+aer+agg+agi+ant+bal+bra+cmd+com+cmp+cnt+
                 dec+det+dri+ecc+fir+fla+fre+han+jum+kic+
                 ldr+otb+pac+pas+pen+pos+pun+ref+tro+sta+
                 str+tea+tec+thr+vis+wor+(1|name),
               data=df_filtered,
               REML = FALSE,
               na.action = na.fail)

model1 <- lmer(average.rating~acc+aer+agg+agi+ant+bal+bra+cmd+com+cmp+cnt+
                 dec+det+ecc+fir+fla+fre+han+jum+kic+
                 ldr+otb+pac+pas+pen+pos+pun+ref+tro+
                 str+tea+tec+thr+vis+wor+(1|name),
               data=df_filtered,
               REML = FALSE,
               na.action = na.fail)

test_model1 <- get_model(step(model1))
summary(test_model1)
test <- lmer(formula = average.rating ~ acc + agg + agi + bra + cmd + cnt + 
     det + ecc + fir + fre + han + otb + pac + ref+(1|name), data = df_filtered, REML = F,
   na.action = na.fail)
summary(test)

table(df_filtered$sta)

summary(model1)
AIC(model1)
r2(model1)
vif(model1) %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() %>% 
  kable(
    digits = 4
  )


###model2
model2 <- lmer(average.rating~acc+aer+agg+agi+ant+bal+bra+cmd+com+cmp+cnt+
                 dec+det+dri+ecc+fir+fla+fre+han+jum+kic+
                 ldr+otb+pac+pas+pen+pos+pun+ref+tro+sta+
                 str+tea+tec+thr+vis+wor+(1|name),
               data=df_filtered,
               REML = FALSE,
               na.action = na.fail)



summary(model2)
vif(model2)
AIC(model2)
summary(model2) %>% 
  coef() %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() %>% 
  rename(
    ` ` = rowname,
    Skattning = Estimate,
    Medelfel = `Std. Error`,
    `t-värde` = `t value`,
    `p-värde` = `Pr(>|t|)`
  ) %>% 
  kable(
    digits = 4
  ) %>% 
  kable_styling("striped")


###step() för att hitta den bäst modellen
step_model <- lmerTest::step(model1)
test <- get_model(step_model)


summary(test)

###model3
model3 <- lmer(average.rating ~ acc + agg + agi + cmd + cnt + 
                 ecc + fir + fre + han + otb + pac + ref + (1|name),
               data=df_filtered,
               REML = FALSE,
               na.action = na.fail)
AIC(model3)
r2(model3)
summary(model3)
summary(model3) %>% 
  coef() %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() %>% 
  rename(
    ` ` = rowname,
    Skattning = Estimate,
    Medelfel = `Std. Error`,
    `t-värde` = `t value`,
    `p-värde` = `Pr(>|t|)`
  ) %>% 
  kable(
    digits = 4
  ) %>% 
  kable_styling("striped")



# Funktionen kräver endast ett argument, modellen som anpassats
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
residualPlots(model3)

###model4
model4 <- lmer(average.rating ~ acc + agg + agi + cmd + cnt + 
                 ecc + fir + fre + han + otb + pac + ref + (1|name),
               data=df_filtered,
               REML = FALSE,
               na.action = na.fail)
AIC(model4)
summary(model4)
r2(model4)
residualPlots(model4)















