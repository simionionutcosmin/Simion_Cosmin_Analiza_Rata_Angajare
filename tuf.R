  install.packages("readxl")
  install.packages("ggplot2")
  library(readxl)
  library(ggplot2)
  
  setwd("D:/econometrie/date econometrie")
  
  # Citire fisier din csv
  employment<- read.csv("Employment_rate.csv")
  HICP<- read.csv("HICP.csv")
  Labor<- read.csv("Labor_force.csv")
  People<- read.csv("People_with_gradute_degree.csv")
  
  View(employment)
  View(HICP)
  View(Labor)
  View(People)
  
  #########################################################################################################################
  
  # Citirea datelor din fișierul Excel
  file_path <- "D:/econometrie/date econometrie/date_regresie_liniara_simpla.xlsx"
  data <- read_excel(file_path)
  
  # Explorarea datelor pentru a identifica variabilele
  print(head(data))
  str(data)
  View(data)
  
  #########################################################################################################################
  
  # Statistici descriptive employment rate 2021
  summary(employment$X2021)
  sd(employment$X2021)
  length(employment$X2021)
  
  # !!! Statistici descriptive Labor 2021
  summary(Labor$X2021)
  sd(Labor$X2021)
  length(Labor$X2021)
  
  # Tabele de frecventa
  table(employment$X2016)
  table (employment$X2016, employment$X2021)
  
  # Tabele de frecventa
  table(Labor$X2016)
  table (Labor$X2016, Labor$X2021)
  
  #########################################################################################################################
  #Coeficient corelatie
  cor_employment_Labor<-cor(data$employment, data$Labor, method = c("kendall"))
  cor(data$employment, data$Labor, method = c("kendall"))
  cor(data$employment, data$People, method = c("kendall"))
  cor(data$employment, data$HICP, method = c("kendall"))
  
  # Selectează doar coloanele numerice
  numeric_data <- data[, sapply(data, is.numeric)]
  cor_matrix <- cor(numeric_data)
  
  #Grafic corelatie
  install.packages("corrplot")
  library(corrplot)
  
  # Diagrama de corelație
  corrplot(cor_matrix, method = "circle", type = "upper", 
           tl.col = "black", tl.srt = 45)
  
  #########################################################################################################################
  
  # Citirea datelor din fișierul Excel
  file_path <- "D:\econometrie\date econometrie\date_regresie_liniara_simpla.xlsx"
  data <- read_excel(file_path)
  
  # Explorarea datelor pentru a identifica variabilele
  print(head(data))
  str(data)
  View(data)
  
  #########################################################################################################################
  # Regresie unifactorială (min_wage ~ cpi/GDP/Unemployment rate)
  
  #Multiple R-squared:  
  unifactorial_model1 <- lm(data$employment ~ data$HICP, data = data)
  summary(unifactorial_model1)
  
  #!!! CEL MAI BUN !!!
  #Multiple R-squared:  
  unifactorial_model2 <- lm(data$employment ~ data$Labor, data = data)
  summary(unifactorial_model2)
  
  #Multiple R-squared: 
  unifactorial_model3 <- lm(data$employment ~ data$People, data = data)
  summary(unifactorial_model3)
  
  # Graficul modelului de regresie unifactorial
  plot (data$employment ~ data$Labor)
  olsreg1 <- lm(data$employment ~ data$Labor, data = data)
  abline(olsreg1)
  
  install.packages("ggplot2")
  library(ggplot2)
  
  
  
  #########################################################################################################################
  #Incercam sa logaritmam 
  #1. Modelul log-log
  
  #Multiple R-squared:
  log_log_unifactorial_model1 <- lm(log(data$employment) ~ log(data$HICP), data = data)
  summary(log_log_unifactorial_model1)
  
  #Multiple R-squared: 
  log_log_unifactorial_model2 <- lm(log(data$employment) ~ log(data$Labor), data = data)
  summary(log_log_unifactorial_model2)
  
  #Multiple R-squared:  
  log_log_unifactorial_model3 <- lm(log(data$employment) ~ log(data$People), data = data)
  summary(log_log_unifactorial_model3)
  
  #########################################################################################################################
  #2. Modelul log-lin
  
  #Multiple R-squared:  
  log_lin_unifactorial_model1 <- lm(log(data$employment) ~ data$HICP, data = data)
  summary(log_lin_unifactorial_model1)
  
  #Multiple R-squared: 
  log_lin_unifactorial_model2 <- lm(log(data$employment) ~ data$Labor, data = data)
  summary(log_lin_unifactorial_model2)
  
  #Multiple R-squared:  
  log_lin_unifactorial_model3 <- lm(log(data$employment) ~ data$People, data = data)
  summary(log_lin_unifactorial_model3)
  
  #########################################################################################################################
  #3. Modelul lin-log
  
  #Multiple R-squared:  
  lin_log_unifactorial_model1 <- lm(data$employment ~ log(data$HICP), data = data)
  summary(lin_log_unifactorial_model1)
  
  #!!! CEL MAI BUN !!!

  lin_log_unifactorial_model2 <- lm(data$employment~ log(data$Labor), data = data)
  summary(lin_log_unifactorial_model2)
  
  #R-squared: din variația employment_rate logaritmat este explicată de variația Labor
  #p-value:  Testul F este extrem de semnificativ, ceea ce confirmă că modelul global este valid.
  
  #Multiple R-squared:  
  lin_log_unifactorial_model3 <- lm(data$employment ~ log(data$People), data = data)
  summary(lin_log_unifactorial_model3)
  
  #grafic log-log
  ggplot(data, aes(log(employment), log(Labor))) +
    geom_point() +
    geom_smooth(method = lm, se = F)
  
  #grafic log-lin
  ggplot(data, aes(log(employment), Labor)) +
    geom_point() +
    geom_smooth(method = lm, se = F)
  
  #grafic lin-log
  ggplot(data, aes(employment, log(Labor))) +
    geom_point() +
    geom_smooth(method = lm, se = F)
  
  #########################################################################################################################
  
  # Regresie multifactoriala
  reg <- lm(data$employment ~ data$HICP + data$Labor + data$People)
  summary(reg)
  

  
  
  # Instalare și încărcare pachete necesare
  install.packages(c("tidyverse", "stargazer", "magrittr", "car"))
  library(tidyverse)
  library(stargazer)
  library(magrittr)
  library(car)
  
  # Definire employment1 (dacă nu există deja)
  employment1 <- data
  
  # Crearea modelului de regresie
  reg <- lm(employment ~ HICP + Labor + People, data = employment1)
  
  # Calcularea valorilor previzionate și reziduurilor
  employment1 <- employment1 %>% 
    mutate(employmenthat = fitted(reg),
           uhat = residuals(reg))
  
  # Verificare
  head(employment1)
  view(employment1)
  
  ########################################################################################################################
  
  
  # Asumăm că datele sunt deja citite dintr-un fișier Excel
  # și modelul de regresie este construit pe baza acestora.
  
  # Modelul de regresie multiplă
  reg <- lm(employment ~ HICP + Labor + People, data = data)
  
  # Rezumatul modelului
  summary(reg)
  
  # Testul Shapiro-Wilk pentru reziduuri
  shapiro_result <- shapiro.test(residuals(reg))
  print(shapiro_result)
  
  # Interpretare:
  # Dacă p-value < 0.1/0.05/0.01, atunci reziduurile nu sunt distribuite normal.
  
  # Testul Jarque-Bera pentru reziduuri
  if (!require("tseries")) install.packages("tseries")
  library(tseries)
  
  jarque_bera_result <- jarque.bera.test(residuals(reg))
  print(jarque_bera_result)
  
  # Histograma reziduurilor
  residuals_model <- residuals(reg) %>% as.data.frame()
  colnames(residuals_model) <- "Residuals"
  
  ggplot(data = residuals_model, aes(x = Residuals)) +
    theme_bw() +
    geom_histogram(col = 'grey', bins = 10) +
    xlab('Reziduurile') +
    ylab('Frecvența') +
    ggtitle('Histograma reziduurilor modelului de regresie') +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  ############################################################
  #Prognoza 
  # Calcularea valorilor estimate și a reziduurilor
  # Asigurarea că pachetele necesare sunt instalate și încărcate
  PackageNames <- c("tidyverse", "ggplot2", "magrittr")
  for (i in PackageNames) {
    if (!require(i, character.only = TRUE)) {
      install.packages(i, dependencies = TRUE)
      require(i, character.only = TRUE)
    }
  }
  
  library(tidyverse)
  library(ggplot2)
  library(magrittr)
  
  # Adăugarea predicțiilor și a reziduurilor în cadrul setului de date folosind modelul multifactorial
  data <- data %>%
    mutate(employment_hat = fitted(reg),  # Valorile estimate pentru rata de ocupare
           residuals = resid(reg))       # Reziduurile modelului
  
  # Verificarea primelor rânduri din setul de date actualizat
  head(data)
  
  # Graficul valorilor observate, estimate și reziduurilor
  ggplot(data, aes(x = Labor)) +
    geom_point(aes(y = employment, color = "Rata de ocupare - Valoare observată")) +
    geom_point(aes(y = employment_hat, color = "Rata de ocupare - Valoare estimată")) +
    geom_point(aes(y = residuals, color = "Reziduurile")) +
    geom_smooth(aes(y = employment, color = "Linie ajustată"), 
                method = "lm", se = FALSE) +
    labs(x = "Forța de muncă (Labor)", 
         y = "Rata de ocupare",
         title = "Graficul valorilor observate, estimate și reziduurilor") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Grafic suplimentar pentru a arăta reziduurile față de o altă variabilă explicativă
  ggplot(data, aes(x = HICP)) +
    geom_point(aes(y = residuals, color = "Reziduurile")) +
    geom_smooth(aes(y = residuals, color = "Tendința reziduurilor"), 
                method = "lm", se = FALSE) +
    labs(x = "Indicele Armonizat al Prețurilor de Consum (HICP)", 
         y = "Reziduuri",
         title = "Graficul reziduurilor față de HICP") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Explorarea statisticilor descriptive pentru rata_absolvire și rata_ocupare în 2021
  summary(People$X2021)  # Statistici descriptive pentru rata_absolvire
  sd(People$X2021)       # Devierea standard pentru rata_absolvire
  length(People$X2021)   # Numărul de observații pentru rata_absolvire
  
  summary(HICP$X2021)  # Statistici descriptive pentru indicele armonizat al prețurilor de consum
  sd(HICP$X2021)       # Devierea standard pentru indicele armonizat al prețurilor de consum
  length(HICP$X2021)   # Numărul de observații pentru indicele armonizat al prețurilor de consum
  
  # Regresie multifactorială
  reg <- lm(data$employment ~ data$HICP + data$People + data$Labor)
  summary(reg)
  
  
  
  
  # Statistici descriptive pentru variabilele People (rata_absolvire) și HICP
  
  summary(People$X2021)  # Statistici descriptive pentru rata_absolvire
  sd(People$X2021)       # Devierea standard pentru rata_absolvire
  length(People$X2021)   # Numărul de observații pentru rata_absolvire
  
  summary(HICP$X2021)  # Statistici descriptive pentru hicp
  sd(HICP$X2021)       # Devierea standard pentru hicp
  length(HICP$X2021)   # Numărul de observații pentru hicp
  
  # Regresie multifactorială
  reg <- lm(employment ~ HICP + People + Labor, data = data)
  summary(reg)
  
  reg_log <- lm(log(data$employment) ~ log(data$HICP) + log(data$People) + log(data$Labor), data = data)
  summary(reg_log)
  

  
  ############################################################
  # Cerința 3: Testarea normalității reziduurilor
  ############################################################
  
  # Graficul Residuals vs Fitted
  plot(reg, which = 1)
  
  # Graficul Q-Q Plot
  plot(reg, which = 2)
  
  # Histograma reziduurilor
  install.packages("olsrr")
  
  library(olsrr)
  ols_plot_resid_hist(reg)
  
  ggplot(data = data) +
    theme_bw() +
    geom_histogram(mapping = aes(x = residuals(reg)), col = 'blue') +
    xlab('Reziduuri') + 
    ylab('Count') +
    ggtitle('Histograma reziduurilor') + 
    theme(plot.title = element_text(hjust = 0.5))
  
  # Asimetria și curtosisul reziduurilor
  install.packages("moments")
  library(moments)
  
  skewness(residuals(reg))  # Asimetria reziduurilor
  kurtosis(residuals(reg))  # Curtosisul reziduurilor
  
  # Testul Shapiro-Wilk pentru reziduuri
  shapiro.test(residuals(reg))
  # Dacă p-value < 0.05, reziduurile nu sunt distribuite normal.
  
  # Distanța Cook
  ols_plot_cooksd_bar(reg)
  
  ############################################################
  # Testarea homoscedasticității
  ############################################################
  
  # Graficul reziduurilor față de valorile estimate de model
  data <- data %>% mutate(yhat = fitted(reg), uhat = residuals(reg))
  ggplot(data = data, mapping = aes(x = yhat, y = uhat)) +
    theme_bw() +
    geom_point() +
    geom_hline(yintercept = 0, col = 'red') +
    labs(y = 'Reziduuri', x = 'Valori estimate')
  
  # Generarea interacțiunilor și pătratelor variabilelor independente
  data <- data %>%
    mutate(HICP_sq = HICP^2,
           Labor_sq = Labor^2,
           People_sq = People^2,
           HICP_Labor = HICP * Labor,
           HICP_People = HICP * People,
           Labor_People = Labor * People)
  
  # Testul Breusch-Pagan
  install.packages("lmtest")
  library(lmtest)
  bptest(reg)
  
  bptest(reg_log)
  
  install.packages("dplyr") # Instalează pachetul
  library(dplyr)            # Încarcă pachetul
  
  
  # Testul White
  data <- data %>% mutate(uhatsq = uhat^2)
  model_White <- lm(uhatsq ~ HICP + Labor + People + HICP_sq + Labor_sq + People_sq +
                      HICP_Labor + HICP_People + Labor_People, data = data)
  summary(model_White)
  
  # Calcul manual pentru testul White
  k <- model_White$rank - 1
  r2 <- summary(model_White)$r.squared
  n <- nrow(data)
  
  F_stat <- (r2 / k) / ((1 - r2) / (n - k - 1))
  F_pval <- pf(F_stat, k, n - k - 1, lower.tail = FALSE)
  
  LM_stat <- n * r2
  LM_pval <- pchisq(LM_stat, df = k, lower.tail = FALSE)
  
  cat("F-statistic:", F_stat, "\nF p-value:", F_pval, "\n")
  cat("LM-statistic:", LM_stat, "\nLM p-value:", LM_pval, "\n")
  
  # Dacă p-value < 0.05, reziduurile sunt heteroscedastice.
  
  ############################################################
  # Grafice suplimentare pentru analiza reziduurilor
  ############################################################
  
  # Graficul reziduurilor față de o variabilă independentă (HICP)
  install.packages("ggplot2")
  library(ggplot2)
  
  
  
  ggplot(data, aes(x = HICP, y = uhat)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = "Reziduuri vs HICP", x = "HICP", y = "Reziduuri") +
    theme_minimal()
  
  # Graficul reziduurilor față de valorile estimate
  ggplot(data, aes(x = yhat, y = uhat)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = "Reziduuri vs Valori estimate", x = "Valori estimate", y = "Reziduuri") +
    theme_minimal()
  
  
  
  
  
  # Setul de date cu privire la variabilele tale
  # Asigura-te ca "data" este setul de date utilizat
  
  install.packages("dplyr")
  library(dplyr)
  
  # Model de regresie pentru variabilele tale
  model_0 <- lm(employment ~ HICP + Labor + People, data = data) 
  summary(model_0)
  data <- data %>% mutate(uhat = resid(model_0)) # extragem reziduurile din model 
  
  # Ipoteza 1 - Este modelul liniar in parametri? 
  # Da, deoarece poate fi scris ca o functie liniara de forma: 
  # employment = intercept + coef1 * HICP + coef2 * Labor + coef3 * People + epsilon
  
  # Ipoteza 2 - Nr de observatii > nr variabile independente
  nobs(model_0) > (model_0$rank - 1)
  
  # Ipoteza 3 - Modelul de regresie este corect specificat
  # Presupunem ca modelul este specificat corect, deoarece variabilele utilizate sunt relevante pentru dependentă
  
  # Ipoteza 4 - Variabilitatea in X este pozitiva
  var(data$HICP)
  var(data$Labor)
  var(data$People) # toate valorile > 0 => ipoteza acceptata
  
  # Ipoteza 5 - Media reziduurilor este 0
  mean(model_0$residuals) # medie aproape de 0 => ipoteza acceptata
  
  library(car)
  # Ipoteza 6 - Testare multicoliniaritate
  vif(model_0) # nu avem valori pentru VIF > 10 => ipoteza acceptata
  
  # Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
  cor.test(data$HICP, model_0$residuals) # p-value > 0.1 => nu sunt corelate
  cor.test(data$Labor, model_0$residuals) # p-value > 0.1 => nu sunt corelate
  cor.test(data$People, model_0$residuals) # p-value > 0.1 => nu sunt corelate
  # => ipoteza acceptata
  
  install.packages("sandwich")
  
  # Ipoteza 8 - Reziduurile sunt homoscedastice
  library(lmtest)
  bptest(model_0) # verificare homoscedasticitate
  library(sandwich)
  coeftest(model_0, vcov. = vcovHC(model_0, type = "HC1")) # hetero
  
  # Corectie prin WLS (Weighted Least Squares)
  model_WLS1 <- lm(employment ~ HICP + Labor + People, 
                   data = data, weights = 1/Labor)
  
  # Transformarea variabilelor pentru WLS
  w <- sqrt(1 / data$Labor)
  data <- data %>% mutate(
    employment_star = employment * w,
    HICP_star = HICP * w,
    Labor_star = Labor * w,
    People_star = People * w,
    constant_star = w
  )
  
  model_WLS2 <- lm(employment_star ~ 0 + constant_star + HICP_star + Labor_star + People_star, 
                   data = data)
  
  # Teste dupa corectie
  bptest(model_WLS2) # homo
  coeftest(model_WLS2, vcov. = vcovHC(model_WLS2, type = "HC1")) # hetero
  summary(model_WLS2)
  
  # Ipoteza 9 - Reziduurile nu sunt autocorelate
  acf(model_WLS2$residuals) # nu sunt autocorelate
  library(lmtest)
  dwtest(model_WLS2) # p-value > 0.1 => reziduuri nonautocorelate 
  bgtest(model_WLS2, order = 2) # p-value > 0.1 => reziduuri nonautocorelate
  
  # Ipoteza 10 - Reziduurile sunt normal distribuite
  library(tseries)
  jarque.bera.test(model_WLS2$residuals) # test pentru normalitate
  
  # Detectie puncte influente
  library(car)
  influencePlot(model_WLS2)
  
  # Corectie pentru puncte influente
  # Eliminam observatiile influente (exemplu 42, 63, etc. adaptat pentru datele tale)
  data_clean <- data[-c(42, 63), ]
  model_WLS3 <- lm(employment_star ~ 0 + constant_star + HICP_star + Labor_star + People_star, 
                   data = data_clean)
  jarque.bera.test(model_WLS3$residuals) # normal distribuite
  
  # MODEL FINAL PENTRU PROGNOZE
  summary(model_WLS3)
  
  
  #################################################################################################
  
  
  # Instalare și încărcare pachete necesare
  if (!require("caret")) install.packages("caret")
  library(caret)
  if (!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  
  # Crearea variabilelor dummy pentru analiza ta
  data$UE <- ifelse(data$Countries %in% c("Belgium", "Bulgaria", "Croatia", "Czechia", "Estonia",
                                          "France", "Germany", "Greece", "Hungary", "Ireland",
                                          "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
                                          "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain"), 1, 0)
  
  # Crearea unei variabile dummy pentru Labor peste mediana
  median_labor <- median(data$Labor, na.rm = TRUE)
  data$High_Labor <- ifelse(data$Labor > median_labor, 1, 0)
  
  # Model de regresie cu variabile dummy
  reg_with_dummies <- lm(employment ~ Labor + HICP + People + UE + High_Labor, data = data)
  summary(reg_with_dummies)
  
  # Model log-log cu variabile dummy
  reg_log_with_dummies <- lm(log(employment) ~ log(Labor) + log(HICP) + log(People) + UE + High_Labor, data = data)
  summary(reg_log_with_dummies)
  
  # Regresie log-log fără variabila High_Labor
  reg_log_with_dummies <- lm(log(employment) ~ log(Labor) + log(HICP) + log(People) + UE, data = data)
  summary(reg_log_with_dummies)
  
  ###############################################################################################
  # Împărțirea datelor în set de antrenare (80%) și set de testare (20%)
  set.seed(123)  # Asigură reproductibilitatea
  training.samples <- data$employment %>%
    createDataPartition(p = 0.8, list = FALSE)
  train.data <- data[training.samples, ]
  test.data <- data[-training.samples, ]
  
  # Model log-log cu variabile dummy pe setul de antrenare
  model <- lm(log(employment) ~ log(Labor) + log(HICP) + log(People) + UE, data = train.data)
  summary(model)
  
  # Predicție pentru setul de testare
  test.data$Predicted_Log_Employment <- predict(model, newdata = test.data)
  test.data$Predicted_Employment <- exp(test.data$Predicted_Log_Employment)
  
  # Calcularea diferenței relative între valorile reale și cele prognozate
  test.data$Difference_Percentage <- ((test.data$Predicted_Employment - test.data$employment) / test.data$employment) * 100
  
  # Vizualizarea primelor rezultate
  head(test.data[, c("employment", "Predicted_Employment", "Difference_Percentage")])
  
  # Calcularea indicatorilor de performanță
  rmse <- sqrt(mean((test.data$Predicted_Employment - test.data$employment)^2))
  mae <- mean(abs(test.data$Predicted_Employment - test.data$employment))
  mape <- mean(abs((test.data$Predicted_Employment - test.data$employment) / test.data$employment)) * 100
  
  # Afișarea rezultatelor
  cat("RMSE:", rmse, "\n")
  cat("MAE:", mae, "\n")
  cat("MAPE:", mape, "%\n")
  
  # Grafic pentru comparație
  library(ggplot2)
  
  ggplot(test.data, aes(x = employment, y = Predicted_Employment)) +
    geom_point(color = "blue", size = 2) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(
      title = "Comparație între valorile reale și cele prognozate (set de testare)",
      x = "Rata de ocupare reală",
      y = "Rata de ocupare prognozată"
    ) +
    theme_minimal()
  
  ####################################################################################################################
  ###Aplicatia 3######################################################################################################
  
  
  
  # Instalarea și încărcarea pachetelor necesare
  PackageNames <- c("tidyverse", "gplots", "plm", "readxl", "lmtest")
  for (i in PackageNames) {
    if (!require(i, character.only = TRUE)) {
      install.packages(i, dependencies = TRUE)
      require(i, character.only = TRUE)
    }
  }
  
  # Citirea datelor din fișier
  file_path <- "D:/econometrie/date econometrie/data_pannel.xlsx"
  data <- read_excel(file_path)
  
  # Statistici descriptive
  summary(data)
  
  # Declararea setului de date de tip panel
  pdata <- pdata.frame(data, index = c("Country", "Year"), drop.index = TRUE)
  
  
  
  # Explorarea heterogeneității
  plotmeans(Employment_rate ~ Country, main = "Heterogeneitate între țări", data = data)
  plotmeans(Employment_rate ~ Year, main = "Heterogeneitate în timp", data = data)
  
  # Modelul Pooled OLS
  pooled <- plm(Employment_rate ~ Labor_force + HICP + People_with_gradute_degree, data = pdata, model = "pooling")
  summary(pooled)
  
  # Model OLS (regresie liniară)
  ols <- lm(Employment_rate ~ Labor_force + HICP + People_with_gradute_degree, data = data)
  summary(ols)
  
  # Model cu efecte fixe (FE)
  fe <- plm(Employment_rate ~ Labor_force + HICP + People_with_gradute_degree, data = pdata, model = "within")
  summary(fe)
  
  # Model cu efecte aleatoare (RE)
  re <- plm(Employment_rate ~ Labor_force + HICP + People_with_gradute_degree, data = pdata, model = "random")
  summary(re)
  
  # Compararea modelelor
  # Testul F - Semnificația efectelor fixe (OLS vs FE)
  pFtest(fe, ols) # Dacă p-value < 0.05 => modelul FE este preferat
  
  # Testul Breusch-Pagan (LM test) - Pooled OLS vs RE
  plmtest(pooled, type = "bp") # Dacă p-value < 0.05 => se preferă modelul RE
  
  # Testul Hausman - RE vs FE
  phtest(fe, re) # Dacă p-value < 0.05 => se preferă modelul FE
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  