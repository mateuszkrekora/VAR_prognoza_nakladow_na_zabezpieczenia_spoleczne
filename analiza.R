#Instalacja i załadowanie niezbędnych pakietów
install.packages("urca")
library(urca)

install.packages("lmtest")
library(lmtest)

install.packages("serial")
library(serial)

install.packages("vars")
library(vars)


getwd()
#setwd("YOUR/LOCAL/FILE")
#Załadowanie danych
dane <- read.csv2("dane_final.csv", sep = ";")

#Sprawdzenie, czy dane zostały poprawnie załadowane
head(dane)
str(dane)

#Jeśli wszystko jest w porządku, kontynuuj z testowaniem stacjonarności
adf_demografia <- ur.df(dane$demografia, type = "none")
adf_pkb_per_capita <- ur.df(dane$pkb_per_capita, type = "none")
adf_naklady_zabezp <- ur.df(dane$naklady_zabezp, type = "none")

summary(adf_demografia)
summary(adf_pkb_per_capita)
summary(adf_naklady_zabezp)

plot(adf_demografia)
plot(adf_pkb_per_capita)
plot(adf_naklady_zabezp)
#Jeśli serie nie są stacjonarne, różnicowanie
demografia_diff <- diff(dane$demografia, differences = 1)
pkb_per_capita_diff <- diff(dane$pkb_per_capita, differences = 1)
naklady_zabezp_diff <- diff(dane$naklady_zabezp, differences = 1)

dane_diff <- data.frame(cbind(demografia_diff, naklady_zabezp_diff, pkb_per_capita_diff))
dane_diff

adf_demografia_diff <- ur.df(demografia_diff, type = "none")
adf_pkb_per_capita_diff <- ur.df(pkb_per_capita_diff, type = "none")
adf_naklady_zabezp_diff <- ur.df(naklady_zabezp_diff, type = "none")

summary(adf_demografia_diff)
summary(adf_pkb_per_capita_diff)
summary(adf_naklady_zabezp_diff)

plot(adf_demografia_diff)
plot(adf_naklady_zabezp_diff)
plot(adf_pkb_per_capita_diff)
#Wybór liczby opóźnień
lag_selection <- VARselect(dane_diff, lag.max = 3, type = "both")
#lag_selection <- VARselect(dane_diff[, c("demografia", "pkb_per_capita", "naklady_zabezp")], lag.max = 15, type = "both")
lag_selection$selection

#Estymacja modelu VAR z optymalną liczbą opóźnień
var_model <- VAR(dane_diff, p = lag_selection$selection["AIC(n)"], type = "both")

#Testowanie autokorelacji reszt - Test Boxa-Ljunga
box_ljung_test <- Box.test(as.vector(residuals(var_model)), lag = lag_selection$selection["AIC(n)"], type = "Ljung-Box")
box_ljung_test

#brak autokorelacji reszt

#Testowanie na normalność reszt - Test Jarque-Bera
normality_test <- normality.test(var_model)
normality_test
#Testowanie heteroskedastyczności - Test White'a
residuals_var <- residuals(var_model)

#Przeprowadzenie testu White'a na resztach
white_test <- bptest(residuals_var ~ lag(residuals_var, k = 1) + lag(residuals_var^2, k = 1))
white_test

#jest homoskedastyczność

summary(var_model)
var_model
#Prognoza
predykcje <- predict(var_model, n.ahead = 5)  #Prognoza na 5 kolejnych okresów
predykcje

#Wykres prognozy
plot(predykcje)

library(ggplot2)
library(scales)

#Zakładając, że 'dane_diff' i 'predykcje' już istnieją w środowisku R

#Tworzenie sekwencji czasowej dla danych historycznych i prognoz
data_czas <- seq(from = as.Date("2014-01-01"), by = "year", length.out = nrow(dane_diff))
prognoza_czas <- seq(from = as.Date("2023-01-01"), by = "year", length.out = 5)

#Utworzenie jednego wektora czasowego
czas <- c(data_czas, prognoza_czas)

#Utworzenie jednej ramki danych z wartościami i prognozami
demografia_wszystko <- c(dane_diff$naklady_zabezp_diff, predykcje$fcst$naklady_zabezp_diff[, "fcst"])
demografia_lower <- c(rep(NA, length(dane_diff$naklady_zabezp_diff)), predykcje$fcst$naklady_zabezp_diff[, "lower"])
demografia_upper <- c(rep(NA, length(dane_diff$naklady_zabezp_diff)), predykcje$fcst$naklady_zabezp_diff[, "upper"])

df_wszystko <- data.frame(czas, demografia_wszystko, demografia_lower, demografia_upper)

#Tworzenie wykresu
ggplot(df_wszystko, aes(x = czas)) +
  geom_line(aes(y = demografia_wszystko), color = "blue") +
  geom_ribbon(aes(ymin = demografia_lower, ymax = demografia_upper), fill = "blue", alpha = 0.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Rok", y = "Wartość", title = "Przyrosty nakładów na zabezpieczenia społeczne w relacji do PKB z prognozami") +
  theme_minimal()

#Tworzenie wykresu
ggplot(df_wszystko, aes(x = czas)) +
  geom_line(aes(y = demografia_wszystko), color = "blue") +
  geom_ribbon(aes(ymin = demografia_lower, ymax = demografia_upper), fill = "blue", alpha = 0.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = "Value", title = "Increases in Social Security Expenditures Relative to GDP with Forecasts") +
  theme_minimal()


labs(x = "Year", y = "Value", title = "Increases in Social Security Expenditures Relative to GDP with Forecasts")

