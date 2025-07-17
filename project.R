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
setwd("C:/Users/timce/OneDrive/Dokument/Rcode_2024_2025/732G56/datamaterial")
data = read.csv("utespelare.csv", sep=";")
data$average.rating <- as.numeric(gsub(",",".",data$average.rating))
only_GK <- data[data$position == "GK",]
only_GK$matchday <- str_extract(only_GK$matchday,"S\\d+")
only_GK$matchday <- as.numeric(str_remove(only_GK$matchday,"S"))


#######################################################################


### test data
only_GK <- only_GK[,c("matchday","average.rating","name","club","opponent","acc","aer","agg","agi","ant","bal","bra","cmd","com","cmp","cnt","cor","cro","dec","det","dri","ecc",
                      "fin","fir","fla","fre","han","hea","jum","kic","ldr","lon","l.th","mar","nat","otb","pac","pas","pen","pos","pun","ref",
                      "tro","sta","str","tck","tea","tec","thr","vis","wor")]

### hantera med 0
only_GK[ , sapply(only_GK, is.numeric)] <- lapply(only_GK[ , sapply(only_GK, is.numeric)], function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
})




### corelation matrix
numeric_vars <- only_GK %>% select(where(is.numeric))
cor_matrix <- cor(numeric_vars,use="complete.obs")
kable(cor_matrix, caption = "Correlation Matrix of Numeric Variables") %>%
  kable_styling()

ggplot(data_5_first_seasons, aes(x=dec,y=average.rating)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE)






### fullständig data
model_test <- lmer(average.rating~acc+aer+agg+agi+ant+bal+bra+cmd+com+cmp+cnt+
                     cor+cro+dec+det+dri+ecc+fin+fir+fla+fre+han+hea+jum+kic+
                     ldr+lon+otb+pac+pas+pen+pos+pun+ref+tro+sta+
                     str+tea+tec+thr+vis+wor+(1|name),
                   data=only_GK, control = lmerControl(optCtrl = list(maxfun = 100000)),
                   REML = FALSE,
                   na.action = na.fail)
anova(model_test)
r2(model_test)
AIC(model_test2)

### Undersöka potentiella förklarande variabler
unique(only_GK$wor)

# Lista på variabelnamn som har värden lägre än 5
vars_to_adjust <- c("agg", "ant", "bal", "cmp", "cor", "cro", "dec", "det", "dri", 
                    "fin", "fir", "bra", "fla", "fre", "han", "hea", "jum", "ldr", 
                    "lon", "l.th", "mar", "nat", "otb", "pos", "pun", "ref", "sta", 
                    "str", "tck", "tea", "tec", "vis", "wor", "acc")

# Sätt alla värden < 5 till 5
only_GK_over5 <- only_GK %>%
  mutate(across(all_of(vars_to_adjust), ~ ifelse(. < 5, 5, .)))

#Kontroll
unique(only_GK_over5$nat)

#Model
model1 <- lmer(average.rating~acc+aer+agg+agi+ant+bal+bra+cmd+com+cmp+cnt+
                     cor+cro+dec+det+dri+ecc+fin+fir+fla+fre+han+hea+jum+kic+
                     ldr+lon+otb+pac+pas+pen+pos+pun+ref+tro+sta+
                     str+tea+tec+thr+vis+wor+(1|name),
                   data=only_GK_over5,
                   REML = FALSE,
                   na.action = na.fail)
library(car)
vif(model1)

model2 <- lmer(average.rating~acc+aer+agg+agi+ant+bal+bra+cmd+com+cmp+cnt+
                 dec+det+dri+ecc+fir+fla+fre+han+jum+kic+
                 ldr+otb+pac+pas+pen+pos+pun+ref+tro+sta+
                 str+tea+tec+thr+vis+wor+(1|name),
               data=only_GK_over5,
               REML = FALSE,
               na.action = na.fail)
summary(model2)
step_model <- step(model2)
summary(step_model)

model3 <- lmer(average.rating ~ acc + agg + agi + bra + cmd + cnt + 
                 ecc + fir + fre + han + otb + pac + ref + (1|name),
               data=only_GK_over5,
               REML = FALSE,
               na.action = na.fail)
summary(model3)

model4 <- lmer(formula = average.rating ~ acc + agg + agi + cmd + cnt + 
                 ecc + fir + fre + han + otb + pac + ref + (1 | name),
               data=only_GK_over5,
               REML = FALSE,
               na.action = na.fail)
summary(model4)
visualize(model4, formula = average.rating ~ acc | name, sample = 5)
residualPlots(model_test)
vcov(model4)

model.comparison(model3, model4)
?estimates
compare.fits(model3, model4)
visualize(model4)

estimates(model1)

#Test
data(relationship_satisfaction)

### modeling + graphics

full.mod = lm(satisfaction~communication * separated , data=relationship_satisfaction)
reduced.mod = lm(satisfaction~communication + separated , data=relationship_satisfaction)
visualize(full.mod)
estimates(full.mod)
model.comparison(full.mod, reduced.mod)


############## l.th############## 6 egenskaper
data1 <- only_GK %>%
  select(average.rating,name,cmd,com,ecc,han,pun,thr)
data1

model_test <- lmer(average.rating ~ cmd+com+ecc+han+pun+thr+(cmd+com+ecc+han+pun+thr|name),
                   data=data1, control = lmerControl(optCtrl = list(maxfun = 100000)),
                   REML = FALSE,
                   na.action = na.fail)

visualize(model_test,plot="model",
          formula=average.rating ~ com|cmd,
          sample=20)
visualize(model_test,plot="model",
          formula=average.rating ~ com|ecc,
          sample=20)


summary(model_test) %>% 
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



ranef(model_test)$name
summary(model_test)$coefficients
summary(model_test)
resid(model_test)
hist(resid(model_test))


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
  cowplot::plot_grid(p1, p2, p3, nrow = 2)
  
}
residualPlots(model_test)



summary(model_test)
anova(model_test)
dredge(model_test)



######### reducerade modell
model_test2 <- lmer(average.rating ~ cmd+com+ecc+pun+(1|name),
                    data=only_GK,
                    REML = FALSE,
                    na.action = na.fail)
summary(model_test2)
anova(model_test2)

r2(model_test2)
plot(residuals(model_test2))
qqnorm(resid(model_test2))
qqline(resid(model_test2))



baseline <- lmer(average.rating ~ 1 + (1 | name), data=only_GK)

test_mod <- lmer()

model_test <- lmer(average.rating~acc+aer+agg+agi+ant+bal+bra+cmd+com+cmp+cnt+
                     cor+cro+dec+det+dri+ecc+fin+fir+fla+fre+han+hea+jum+kic+
                     ldr+lon+otb+pac+pas+pen+pos+pun+ref+tro+sta+
                     str+tea+tec+thr+vis+wor+(1|name),
                   data=only_GK,
                   REML = FALSE,
                   na.action = na.fail)
library(flexplot)
summary(model_test)
estimates(model_test)

r2(model_test)r2(model_test)model_test

plot(residuals(model_test))
qqnorm(resid(model_test))
qqline(resid(model_test))


dredge(model_test)


model_test2 <- lmer(average.rating~bal+fir+ldr+ref+(1|name),data=only_GK)
estimates(model_test)
test <- lmer(average.rating ~ 1 + (1|name), data=only_GK)
visualize(model_test, formula=average.rating~name)
require(lme4)

summary(model_test2)

r2(model_test2)

plot(residuals(model_test2))
qqnorm(resid(model_test2))
qqline(resid(model_test2))


model_test3 <- glmmTMB(average.rating~acc+aer+agg+agi+ant+bal+bra+cmd+com+cmp+cnt+cor+cro+dec+det+dri+ecc+fin+fir+fla+fre+han+hea+jum+kic+ldr+lon+l.th+mar+nat+otb+pac+pas+pen+pos+pun+ref+tro+sta+str+tck+tea+tec+thr+vis+wor+(1|opponent)+(1|club),data=dataTrain,family=gaussian)
model_test3 <- glmmTMB(average.rating~acc+aer+agg+agi+ant+bal+bra+cmd+com+cmp+cnt+cor+cro+dec+det+dri+ecc+fin+fir+fla+fre+han+hea+jum+kic+ldr+lon+l.th+mar+nat+otb+pac+pas+pen+pos+pun+ref+tro+sta+str+tck+tea+tec+thr+vis+wor+(1|club),data=dataTrain,family=gaussian)

summary(model_test)
options(na.action = "na.fail")
model_selected <- dredge(model_test,na.action=TRUE)


###
hist(only_GK$average.rating,breaks = 30)

modell_test3 <- glmmTMB(average.rating ~ aer+cmd+com+ecc+han+jum+kic+pun+ref+tro+thr+(1|name)+(1|opponent)+(1|club),data=dataTrain,family = gaussian)
summary(modell_test3)

modell_test3 <- glm(average.rating ~ aer+cmd+com+ecc+han+jum+kic+pun+ref+tro+thr,data=dataTrain,family = gaussian)
summary(modell_test3)

modell_test3_re <- glmmTMB(average.rating ~ aer+pun+ref+(1|name)+(1|opponent)+(1|club),data=dataTrain,family = gaussian)
summary(modell_test3_re)

modell_test3_re <- glm(average.rating ~ aer+com+ecc+jum+kic+tro+thr, family = gaussian, data = dataTrain)
summary(modell_test3_re)





# Prediktera antal räddningar på testdata
pred_matchbetyg <- predict(modell_test3_re, newdata = dataValid, allow.new.levels = TRUE)

# Lägg till prediktionerna i testdata
dataValid$predicted_matchbetyg <- pred_matchbetyg

colors <- ifelse(abs(dataValid$average.rating - dataValid$predicted_matchbetyg) > 0.5,
                 "red",  # stor skillnad
                 "green") # liten skillnad
plot(dataValid$average.rating, dataValid$predicted_matchbetyg,
     xlab = "Verkliga räddningar",
     ylab = "Predikterade räddningar",
     main = "Prediktion vs Verklighet",
     pch = 19, col = colors)
abline(0, 1, col = "red", lty = 2)
# RMSE: ju lägre, desto bättre
rmse <- sqrt(mean((dataValid$total.saves - dataValid$predicted_saves)^2))
print(paste("RMSE:", round(rmse, 2)))


resid <- residuals(modell_test3_re,type = "pearson")
plot(predict(modell_test), resid,
     xlab = "Predikterade värden",
     ylab = "Residualer",
     main = "Residualplot")
abline(h = 0, col = "red", lty = 2)

####

table(data$penalties.faced)

data_test <- data2 %>%
  group_by(name) %>%
  summarise(
    Avg_rating = mean(average.rating, na.rm=TRUE),
    total.saves = mean(total.saves, na.rm=TRUE),
    save. = mean(save., na.rm=TRUE),
    conceded = mean(conceded, na.rm=TRUE),
    saves.parried = mean(saves.parried, na.rm=TRUE),
    saves.held = mean(saves.held, na.rm=TRUE),
    saves.tipped = mean(saves.tipped, na.rm=TRUE),
    faced = mean(as.numeric(data$penalties.faced),na.rm = TRUE),
    saved = mean(as.numeric(data$penalties.saved),na.rm = TRUE)
  )

####################################################################
### "average.rating","acc","aer","agg","agi","ant","bal","bra","cmd","com","cmp","cnt","cor",
### "cro","dec","det","dri","ecc","fin","fir","fla","fre","han","hea","jum","kic","ldr","lon",
### "l.th","mar","nat","otb","pac","pas","pen","pos","pun","ref","tro","sta","str","tck","tea",
### "tec","thr","vis","wor"
data_test2 <- only_GK %>%
  group_by(name) %>%
  summarise(
    average.rating = mean(average.rating),
    acc = mean(acc),
    aer = mean(aer),
    agg = mean(agg),
    agi = mean(agi),
    ant = mean(ant),
    bal = mean(bal),
    bra = mean(bra),
    cmd = mean(cmd),
    com = mean(com),
    cmp = mean(cmp),
    cnt = mean(cnt),
    cor = mean(cor),
    cro = mean(cro),
    dec = mean(dec),
    det = mean(det),
    dri = mean(dri),
    ecc = mean(ecc),
    fin = mean(fin),
    fir = mean(fir),
    fla = mean(fla),
    fre = mean(fre),
    han = mean(han),
    hea = mean(hea),
    jum = mean(jum),
    kic = mean(kic),
    ldr = mean(ldr),
    lon = mean(lon),
    l.th = mean(l.th),
    mar = mean(mar),
    nat = mean(nat),
    otb = mean(otb),
    pac = mean(pac),
    pas = mean(pas),
    pen = mean(pen),
    pos = mean(pos),
    pun = mean(pun),
    ref = mean(ref),
    tro = mean(tro),
    sta = mean(sta),
    str = mean(str),
    tck = mean(tck),
    tea = mean(tea),
    tec = mean(tec),
    thr = mean(thr),
    vis = mean(vis),
    wor = mean(wor)
  )

###
only_GK[ , sapply(only_GK, is.numeric)] <- lapply(only_GK[ , sapply(only_GK, is.numeric)], function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
})

require(olsrr)

modell_test4 <- lm(average.rating ~ . - name,data=data_test2)
summary(modell_test4)

# Lägg till argumentet details = TRUE för att få mer detaljer i utskriften
stegModell <- ols_step_both_aic(modell_test4)

stegModell$metrics %>% 
  select(step, variable, r2, adj_r2, aic) %>% 
  kable(digits = 3, 
        col.names = c("Steg", "Variabel", "$R^2$", "$R^2_{adj}$", "AIC"),
        escape = FALSE)

result <- ols_step_best_subset(modell_test4)
result[[1]] %>% 
  head(n = 5) %>% 
  kable(digits = 3)




# Antag att din data heter df och du har variabler:
# id = person-id
# time = tidpunkt
# y = respons
# x1, x2 = prediktorer

# Standardisera inom varje individ
df_within <- only_GK %>%
  group_by(name) %>%
  mutate(
    y_z = scale(average.rating, center = TRUE, scale = TRUE),
    x1_z = scale(acc, center = TRUE, scale = TRUE),
    x2_z = scale(aer, center = TRUE, scale = TRUE)
  ) %>%
  ungroup()

# Kolla korrelation mellan standardiserade värden
cor(df_within %>% select(y_z, x1_z, x2_z))
library(psych)
psych::corr.test(df_within %>% select(y_z, x1_z, x2_z))




