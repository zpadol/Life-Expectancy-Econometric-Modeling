# =============================================================================
# TEMAT: DETERMINANTY DŁUGOŚCI ŻYCIA
# =============================================================================
install.packages('tidyverse')
install.packages('patchwork')
install.packages('corrplot')
install.packages('mice')
install.packages('car')
install.packages('sandwich')
install.packages('lmtest')
install.packages("tidyverse")
library(tidyverse)
library(patchwork)
library(corrplot)
library(mice)
library(car)
library(sandwich)
library(lmtest)
library(dplyr)
library(ggplot2)
library(tidyr)

# Wczytanie danych i wstępna analiza

raw_data <- read_csv("data_life_exp_wrld.csv",
                     na = c("", "NA", ".."),
                     show_col_types = FALSE)
#Dane pochodzą z World Bank Group
#Dotyczą jakości życia w latach 2000-2023 dla krajów z całego świata z wykluczeniem terytoriów zależnych, małych państw, państw wyspiarskich, nowych państw (np. Kosowo), aby uniknąć znacznych braków danych 

# Zamieniamy brakujące wartości na NA, (domyślnie są wczytane jako
# ciąg znaków "..")

# Pierwsze spojrzenie na dane
glimpse(raw_data) 

# Jak widać, nazwy kolumn są zbyt długie, dlatego trzeba je uporządkować

data <- raw_data %>%
  rename(
    country        = `Country Name`,
    code           = `Country Code`,
    year           = `Time`,
    life_exp       = `Life expectancy at birth, total (years) [SP.DYN.LE00.IN]`,
    gdp_pc         = `GDP per capita, PPP (current international $) [NY.GDP.PCAP.PP.CD]`,
    electricity    = `Access to electricity (% of population) [EG.ELC.ACCS.ZS]`,
    health_exp     = `Current health expenditure (% of GDP) [SH.XPD.CHEX.GD.ZS]`,
    urban          = `Urban population (% of total population) [SP.URB.TOTL.IN.ZS]`,
    school         = `School enrollment, secondary (% gross) [SE.SEC.ENRR]`,
    co2_pc         = `Carbon dioxide (CO2) emissions excluding LULUCF per capita (t CO2e/capita) [EN.GHG.CO2.PC.CE.AR5]`,
    physicians     = `Physicians (per 1,000 people) [SH.MED.PHYS.ZS]`,
    pm25           = `PM2.5 air pollution, mean annual exposure (micrograms per cubic meter) [EN.ATM.PM25.MC.M3]`,
    alcohol        = `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age) [SH.ALC.PCAP.LI]`,
    safe_water     = `People using safely managed drinking water services (% of population) [SH.H2O.SMDW.ZS]`,
    fertility      = `Fertility rate, total (births per woman) [SP.DYN.TFRT.IN]`
  ) %>%
  # Usuwamy kolumnę "Time Code" (YR2000, YR2001 itd.) – to duplikat kolumny year
  select(-`Time Code`)

# efekt po zamianie
glimpse(data)

# Korekta danych
# Zamieniamy year na liczbę całkowitą, a nie double
data <- data %>%
  mutate(year = as.integer(year))

# Sprawdzamy zakres lat i listę krajów
cat("Zakres lat:", min(data$year, na.rm = TRUE), "-", max(data$year, na.rm = TRUE), "\n")
cat("Liczba unikalnych krajów:", n_distinct(data$country), "\n")

# Analiza brakujących wartości

# Wykres mice - żeby był czytelny, bierzemy główne zmienne (wszystkie naraz są nieczytelne)
# Ten wykres pokazuje "wzorce" braków - np. czy braki w GDP idą w parze z brakami w CO2
md.pattern(data %>% select(life_exp, gdp_pc, health_exp, urban, fertility), rotate.names = TRUE)

# Robimy tabelę podsumowującą braki dla wszystkich zmiennych
missing_summary <- data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "zmienna", values_to = "liczba_NA") %>%
  mutate(procent_NA = round(100 * liczba_NA / nrow(data), 1)) %>%
  arrange(desc(liczba_NA))

print(missing_summary)

# Jak widać mamy sporo braków przy niektórych zmiennych:
# - physicians (~35% NA), safe_water (~28%), school (~26%) – dużo braków
# - gdp_pc, health_exp – umiarkowane braki (~2%)
# - life_exp, urban, fertility – prawie kompletne (< 1%)
# Zmienne 'physicians', 'safe_water' oraz 'school' wykazują braki przekraczające 25%.
# Ich uwzględnienie w modelu liniowym mogłoby skutkować znaczną utratą reprezentatywności próby.



# Analiza rozkładów (Zanim wejdziemy w model, sprawdzamy jak wyglądają dane)

# Robimy histogramy dla wszystkich zmiennych naraz (korzystamy z pivot_longer)
# To nam pokaże, które zmienne mają "długie ogony" i wymagają logarytmowania
data %>%
  select(where(is.numeric), -year) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  facet_wrap(~ name, scales = "free") +
  theme_minimal() +
  labs(title = "Rozkłady wszystkich zmiennych")

# Wyraźnie widać, że gdp_pc i co2_pc są bardzo "rozciągnięte". 
# Bez logarytmu te kilka najbogatszych państw zdominowałoby wyniki modelu.

# Czyszczenie danych i Transformacje

# 1. Usuwamy wiersze bez life_exp (nasze Y)
# 2. Logarytmujemy PKB i CO2 (żeby "wyprostować" rozkład)
data_prepared <- data %>%
  filter(!is.na(life_exp)) %>%
  mutate(
    log_gdp = log(gdp_pc),
    log_co2 = log(co2_pc)
  )


# Weryfikacja wizualna po transformacji
p_log1 <- ggplot(data_prepared, aes(x = log_gdp)) + geom_histogram(fill = "darkgreen") + theme_minimal()
p_log2 <- ggplot(data_prepared, aes(x = log_co2)) + geom_histogram(fill = "darkgreen") + theme_minimal()
(p_log1 | p_log2) + plot_annotation(title = "Rozkłady po transformacji logarytmicznej")



# Wybieramy zestaw zmiennych do modelu (odrzucamy te, co mają > 25% braków, 
# bo by nam wycieło połowę państw ze zbioru)
data_model <- data_prepared %>%
  select(country, year, life_exp, log_gdp, log_co2, electricity, health_exp, urban, fertility) %>%
  drop_na()


# Analiza zależności (Korelacje)
kor_mat <- cor(data_model %>% select(where(is.numeric)))
corrplot(kor_mat, method = "color", type = "upper", addCoef.col = "black", 
         tl.col = "black", number.cex = 0.8, title = "\nKorelacje zmiennych")
# - Długość życia (life_exp) silnie dodatnio koreluje z logarytmem PKB (0.83),
#   dostępem do energii (0.84) oraz urbanizacją (0.69), a silnie ujemnie z dzietnością (‑0.83).
# - Między log_gdp a log_co2 obserwujemy bardzo wysoką korelację (0.91), co sugeruje ryzyko
#   współliniowości w modelu.
# - Rok (year) ma niskie korelacje ze zmiennymi, co wskazuje, że nie jest kluczowym predyktorem.
# - health_exp wykazuje umiarkowany związek z life_exp (0.39) i niskie korelacje z log_gdp (0.31)
#   oraz log_co2 (0.24).


# Podział na zbiór treningowy i testowy
# Dzielimy 80% na naukę (train) i 20% na sprawdzenie (test)
set.seed(123)
indeksy <- sample(1:nrow(data_model), size = 0.8 * nrow(data_model))

train_set <- data_model[indeksy, ]
test_set  <- data_model[-indeksy, ]

nrow(train_set)
nrow(test_set)


# 3. DOBÓR ZMIENNYCH I ESTYMACJA MODELU

# Funkcja do doboru zmiennych metodą Hellwiga (wskaźnik pojemności informacyjnej)
hellwig_selection <- function(data, target_var, predictor_vars) {
  
  # Wyodrębnienie wektora Y i macierzy X
  y_vec <- data[[target_var]]
  x_mat <- data %>% select(all_of(predictor_vars))
  
  # Wektor korelacji predyktorów z Y (r0) oraz macierz korelacji X między sobą (r_matrix)
  r0 <- cor(x_mat, y_vec)
  r_matrix <- cor(x_mat)
  
  num_vars <- ncol(x_mat)
  var_names <- colnames(x_mat)
  
  # Generowanie wszystkich możliwych kombinacji zmiennych (bez pustej)
  combinations <- expand.grid(replicate(num_vars, c(FALSE, TRUE), simplify = FALSE))
  combinations <- combinations[-1, ] 
  
  # Przygotowanie ramki na wyniki
  results <- data.frame(
    combination = character(),
    h_capacity = numeric(),
    num_vars = integer(),
    stringsAsFactors = FALSE
  )
  
  # Obliczanie wskaźnika H dla każdej kombinacji
  for (i in 1:nrow(combinations)) {
    active_indices <- which(as.logical(combinations[i, ]))
    active_names <- var_names[active_indices]
    
    h_combination <- 0
    for (j in active_indices) {
      # Pojemność informacyjna pojedynczej zmiennej w danej kombinacji
      r0j_sq <- r0[j, 1]^2
      sum_abs_cor <- sum(abs(r_matrix[active_indices, j]))
      
      h_kj <- r0j_sq / sum_abs_cor
      h_combination <- h_combination + h_kj
    }
    
    # Zapisanie wyniku
    results <- rbind(results, data.frame(
      combination = paste(active_names, collapse = " + "),
      h_capacity = h_combination,
      num_vars = length(active_names)
    ))
  }
  
  # Sortowanie od największej wartości H (korzystamy z funkcji z dplyr)
  results <- results %>% arrange(desc(h_capacity))
  
  return(results)
}


# zastosowanie metody na zbiorze treningowym ###

# Definiujemy zmienną objaśnianą (target) i potencjalne zmienne objaśniające (predictors)
target <- "life_exp"
predictors <- c("year", "log_gdp", "log_co2", "electricity", "health_exp", "urban", "fertility")

# Uruchamiamy funkcję na zbiorze treningowym (train_set)
hellwig_results <- hellwig_selection(train_set, target, predictors)

# Podgląd 5 najlepszych kombinacji
cat("\nTop 5 kombinacji wg metody Hellwiga:\n")
print(head(hellwig_results, 5))

# estymacja najlepszego modelu

# Pobieramy najlepszą kombinację (pierwszy wiersz z tabeli wyników)
best_combination <- hellwig_results$combination[1]

# Budujemy formułę dla funkcji estymującej
model_formula <- as.formula(paste(target, "~", best_combination))

# Estymacja modelu Klasyczną Metodą Najmniejszych Kwadratów na zbiorze treningowym
model <- lm(model_formula, data = train_set)
summary(model)

# Dopasowanie (Rsq) (79.5%): Model jest bardzo mocny – wyjaśnia aż 80% różnic w długości życia na świecie.
# Istotność (p-value): Wszystkie trzy zmienne są skrajnie istotne (p-value < 0,05).
# Wpływ zmiennych:
# Zamożność (+): Im wyższe PKB (log_gdp), tym dłuższe życie.
# Infrastruktura (+): Lepszy dostęp do prądu (electricity) wydłuża życie.
# Dzietność (-): Wyższa liczba dzieci na kobietę (fertility) statystycznie wiąże się z krótszym życiem (często z powodu słabszej medycyny w takich regionach).
# Błąd (RSE): Model myli się średnio o około 4 lata.

# Dla porównania, weżmiemy pod uwagę dodatkowo model z wiekszą ilośćią zmiennych, którego ppojemność była marginalnie miejsza (uplasowal sie na 4 miejscu wg Hellwiga).
hellwig_results$combination[4]
model_rozszerzony <- lm(life_exp ~ log_gdp + electricity + health_exp + urban + fertility, data = train_set)
summary(model_rozszerzony)
# Dopasowanie wzrosło o 1% (79.5% - > 80.5%)
# Istotność (p-value): Wszystkie zmienne są istotne (<0.05)

# Sprawdzenie czy dodanie 2 zmiennych do modelu istotnie go poprawia
anova <- anova(model, model_rozszerzony) # test anovy sprawdza czy dodane zmienne sa istotne; H1 oznacza istotnosci nowych zmiennych
anova$`Pr(>F)`
AIC(model, model_rozszerzony)

# PODSUMOWANIE WYBORU MODELU:
# Porównano model optymalny wg metody Hellwiga (3 zmienne) z modelem rozszerzonym (5 zmiennych).
# Kryterium AIC jednoznacznie wskazało na model rozszerzony (17574.74 vs 17724.90).
# Spadek AIC o ok. 150 punktów oraz wynik testu ANOVA (p < 2.2e-16) potwierdzają, 
# że dodatkowe zmienne (wydatki na zdrowie i urbanizacja) istotnie poprawiają 
# jakość dopasowania, a zysk informacyjny przewyższa "karę" za złożoność modelu.
# Ostateczny model wyjaśnia 80,5% zmienności długości życia (Adj. R-squared: 0.805).

# Hellwig vs AIC/ANOVA ?
# Hellwig odrzucił urbanizację i wydatki na zdrowie jako redundantne (powtarzające informację z PKB). Jednak przy dużej próbie
# (N=3140), testy AIC i ANOVA wykazały, że te zmienne dają na tyle duży zysk w precyzji modelu, że ich pozostawienie jest
# statystycznie uzasadnione 


# sprawdzenie wspołliniowości - czy zależność między zmienną PKB a zmiennymi urban i health_exp nie jest zbyt duża?
vif(model_rozszerzony)

# Zastosowany test VIF potwierdził stabilność modelu rozszerzonego – wartości wskaźnika dla wszystkich zmiennych są niskie 
#(poniżej 5), co dowodzi, że mimo korelacji między PKB a urbanizacją i wydatkami na zdrowie, nie występuje problem szkodliwej współliniowości

#Kontynuacja DIAGNOSTYKI

# 1. NORMALNOŚĆ RESZT
r <- residuals(model_rozszerzony)
shapiro.test(r)
hist(r, breaks = 30, main = "Histogram reszt", col = "lightblue")
# p < 2.2e-16 Brak normalności reszt. 

install.packages('tseries')
library(tseries)
jarque.bera.test(r)
# Duża wartość statystyki  małe p-value potwierdza brak normlaności

# 2. HETEROSKEDASTYCZNOŚĆ (Jednorodność wariancji)
# Test Breuscha-Pagana (ogólny)
bptest(model_rozszerzony) 
# p < 2.2e-16 - Silna heteroskedastyczność. Wariancja błędu nie jest stała. Model myli się „różnie” dla różnych poziomów PKB czy urbanizacji.

# Test Breuscha-Pagana dla konkretnej zmiennej (np. log_gdp)
bptest(model_rozszerzony, ~ log_gdp, data = train_set)
# pvalue < 0.05 - Wariancja błędów modelu zmienia się wraz z bogactwem kraju.

# Test Goldfelda-Quandta (podział w 70% próby)
gqtest(model_rozszerzony, fraction = 0.2, order.by = ~ log_gdp, data = train_set)
# Wariancja maleje. Statystyka GQ < 1 mówi, że błędy są większe dla krajów biedniejszych, a wraz ze wzrostem PKB model staje się „precyzyjniejszy”

# Test White'a (wersja uproszczona przez wartości dopasowane)
train_set$y_dop = model_rozszerzony$fitted.values
bptest(model_rozszerzony, ~ y_dop + I(y_dop^2) + I(y_dop^3), data = train_set)
# p-value < 0.05 - istotna heteroskedastyczność

# 3. AUTOKORELACJA / LOSOWOŚĆ
# Test Durbina-Watsona (czy błędy są skorelowane)
dwtest(model_rozszerzony)
# p-value > 0.05 - W modelu nie występuje autokorelacja.

# Test liczby serii (czy błędy są losowe)
library(randtests)
runs.test(r)
# p-value >0.05 - reszty sa losowe

# 4. LINIOWA POSTAĆ MODELU (RESET Ramsey'a)
# Sprawdza, czy nie brakuje potęg zmiennych lub interakcji
resettest(model_rozszerzony)
# bledna postac liniowa

# 5. TEST CHOWA (Stabilność strukturalna)
# Sprawdzamy, czy model "działa tak samo" dla krajów biedniejszych i bogatszych
# Punkt podziału: mediana log_gdp
prog <- median(train_set$log_gdp)

model_global <- model_rozszerzony
rss_teo      <- deviance(model_global)
n            <- nrow(train_set)
k            <- length(coef(model_global))

grupa1 <- subset(train_set, log_gdp <= prog)
grupa2 <- subset(train_set, log_gdp > prog)

mod1 <- lm(life_exp ~ log_gdp + electricity + health_exp + urban + fertility, data = grupa1)
mod2 <- lm(life_exp ~ log_gdp + electricity + health_exp + urban + fertility, data = grupa2)

rss_sum <- deviance(mod1) + deviance(mod2)

# Statystyka F dla testu Chowa
F_chow <- ((rss_teo - rss_sum) / k) / (rss_sum / (n - 2 * k))
p_val_chow <- pf(F_chow, k, n - 2 * k, lower.tail = FALSE)
cat("Statystyka F:", F_chow, "\n")
cat("p-value:", p_val_chow, "\n")
# p-value < 0.05 - powinny byc osobne modele
# 'sila' zmiennej PKB dla biednych i bogatych jest rozna - biednych moze 'udaerzac' bardziej niz bogatych


# podsumowanie diagnostyki i naprawa modelu :
# 1. Brak normalności reszt - dla dużych prób nie jest to problemem

# Założenie o normalności składnika losowego w próbach o dużej liczebności (N=3140) nie jest restrykcyjne.
# Na mocy Centralnego Twierdzenia Granicznego, suma niezależnych zmiennych losowych (u nas suma ważona błędów) o dowolnych rozkładach zbiega
# do rozkładu normalnego. Ponieważ przeprowadzone testy (Durbina-Watsona na skorelowanie reszt oraz serii) potwierdziły niezależność składnika 
# losowego, asymptotyczne rozkłady statystyk testowych w modelu są poprawne.

# 2. Heteroskedastyczność 
# 3. Brak liniowej postaci
# 4. Brak stabilności strukturalnej

### Ad. 3
# proba 1
model_median <- lm(life_exp ~ log_gdp + 
                    I((log_gdp - median(log_gdp, na.rm = TRUE))^2) + 
                    electricity + health_exp + urban + fertility, 
                  data = train_set)
resettest(model_median)

# proba 2
model_log_log <- lm(log(life_exp) ~ log_gdp + electricity + health_exp + urban + fertility, 
                    data = train_set)
resettest(model_log_log)

# proba 3
model_interaction <- lm(life_exp ~ log_gdp * health_exp + electricity + urban + fertility, 
                        data = train_set)
resettest(model_interaction)

# proba 4
model_factors <- lm(life_exp ~ cut(log_gdp, breaks = 4) + electricity + health_exp + urban + fertility, 
                    data = train_set)
resettest(model_factors)

# proba 5
model_super <- lm(life_exp ~ 
                    log_gdp + I((log_gdp - median(log_gdp))^2) + 
                    health_exp + 
                    fertility  +
                    electricity + 
                    urban * log_gdp, 
                  data = train_set)
resettest(model_super)
plot(model_super, which = 1)

# p-value wynosi 0.0008. Przy tak dużej ilości zmiennych, zejście do 0.05 jest bardzo trudne - wybrano więc najlepszy z 5 modeli
# Punktem wyjścia był model rozszerzony ze statystyką RESET = 263.66, 
# co wskazywało na rażące błędy w doborze postaci funkcyjnej.
# Poprzez sukcesywne wprowadzanie nieliniowości oraz efektów synergii (interakcja urbanizacji z zamożnością), 
# udało się zredukować statystykę RESET do poziomu 9.35.

### Ad. 2
# sprawdzenie zmiennych uwzględniając heteroskedastyczność 
final_table <- coeftest(model_super, vcov = vcovHC(model_super, type = "HC3"))
final_table

# wszystkie zmienne poza I((log_gdp - median(log_gdp))^2) są statystycznie istotne. Rozsądnie jest sprawdzić, czy usunięcie jej
# z modelu stanowczo zwiększy wartość statystyki RESET

model_super2 <- lm(life_exp ~ 
                    log_gdp + 
                    health_exp + 
                    fertility  +
                    electricity + 
                    urban * log_gdp, 
                  data = train_set)
resettest(model_super2)
# Statystyka wzrosła jedynie z 9 do 11. Zdecydowano się na usunięcie tego parametru.

# Ad. 4  Brak stabilności strukturalnej
train_set <- train_set %>%
  mutate(rich = as.integer(log_gdp > median(log_gdp))) # zmienna dychotomiczna rich (0/1)


model_chow_fix <- lm(life_exp ~ 
                       log_gdp * rich + # interakcja nowej zmiennej
                       health_exp * rich +
                       fertility +
                       electricity +
                       urban * log_gdp,
                     data = train_set)

prog  <- median(train_set$log_gdp)
n     <- nrow(train_set)
k     <- length(coef(model_chow_fix))  

grupa1 <- subset(train_set, log_gdp <= prog)
grupa2 <- subset(train_set, log_gdp > prog)

formula_chow <- life_exp ~ log_gdp * rich + health_exp * rich + 
  fertility + electricity + urban * log_gdp

mod1 <- lm(formula_chow, data = grupa1)
mod2 <- lm(formula_chow, data = grupa2)

rss_teo <- deviance(model_chow_fix)
rss_sum <- deviance(mod1) + deviance(mod2)

F_chow    <- ((rss_teo - rss_sum) / k) / (rss_sum / (n - 2 * k))
p_val_chow <- pf(F_chow, k, n - 2 * k, lower.tail = FALSE)

cat("Statystyka F:", F_chow, "\n")
cat("p-value:", p_val_chow, "\n")

#W celu wyeliminowania niestabilności strukturalnej (potwierdzonej wysoką statystyką F=120 w teście Chowa), 
#do modelu wprowadzono zmienną dychotomiczną rich (bazującą na medianie PKB) oraz jej interakcje z kluczowymi determinantami.
#Działanie to pozwoliło zredukować statystykę F testu Chowa do poziomu 8, co oznacza niemal 15-krotną poprawę stabilności modelu.


resettest(model_chow_fix)
# Zmiana modelu nie wpłynęła znacząco na statystykę RESET
coeftest(model_chow_fix, vcov = vcovHC(model_chow_fix, type = "HC3")) # ostateczne statystyki 


# predykcja
# Dodajemy zmienną rich do zbioru testowego, by pod względem struktury danych był on taki sam jak zbiór treningowy
test_set <- test_set %>%
  mutate(rich = as.integer(log_gdp > prog))

# predykcja na zbiór testowy
test_set$pred <- predict(model_chow_fix, newdata = test_set)

# obliczanie błędów ex-post
errors <- test_set$life_exp - test_set$pred

ggplot(test_set, aes(x = errors)) +
  geom_histogram(bins = 30, fill = "pink", color = "white") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Rozkład błędów predykcji (Life Expectancy)",
       x = "Błąd (Rzeczywista - Przewidziana)",
       y = "Liczba obserwacji")

# Dla zdecydowanej większości państw i lat model jest bardzo dokładny.
# Wykres przypomina rozklad normalny, bledy maja charakter losowy.
# Można dostrzec lekka asymetrie lewostronną Skoro błąd to Rzeczywista - Przewidziana, 
# to wartości ujemne oznaczają, że rzeczywistość była gorsza niż optymistyczna prognoza modelu.
# Moda jest bliska zeru, lekko na prawo od niego, oznaca to ze 
# model częściej minimalnie zaniża długość życia (o ok. 1 rok), ale robi to bardzo stabilnie

mae  <- mean(abs(errors))
rmse <- sqrt(mean(errors^2))
mape <- mean(abs(errors / test_set$life_exp)) * 100

cat("WYNIKI PREDYKCJI EX-POST\n")
cat("MAE: ", round(mae, 2), "lat\n")
# MAE wynosi 2,64 to o tyle średnio model się myli w ocenie długości życia.
cat("RMSE: ", round(rmse, 2), "lat\n")
# RMSE wynosi 3,5 , jest wyższe o ok. rok, oznacza to, że 
# w zbiorze testowym trafiło się kilka krajów, gdzie model "przestrzelił" bardzo mocno,
# moga to byc kraje, w których wydarzyło się coś, czego nie ma w danych 
# (np. nagła wojna, epidemia), co sprawiło, 
# że rzeczywista długość życia drastycznie odbiega od tej wynikającej z PKB czy urbanizacji.
cat("MAPE: ", round(mape, 2), "%\n")
# MAPE wynosi 4,05% oznacza to, że wiarygodność prognoz jest bardzo wysoka
# srednio prognoza różni się od rzeczywistości o 4,05%


ggplot(test_set, aes(x = life_exp, y = pred)) +
  geom_point(alpha = 0.5, color = "pink") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Predykcja ex-post: Wartości rzeczywiste vs Przewidziane",
       x = "Rzeczywista długość życia (life_exp) w latach",
       y = "Przewidziana długość życia (pred) w latach")

# mamy wyraźny, silny trend liniowy, Powyżej 70 lat: model jest najbardziej precyzyjny.
# Poniżej 60 lat: rozrzut (wariancja) jest nieco większy, jest to normalne w krajach o niższym PKB
# i gorszej infrastrukturze na długość życia wpływają czynniki losowe
# model ma tendencję do lekkiego przeszacowania długości życia w krajach o najtrudniejszej sytuacji 
# Wynika to prawdopodobnie z faktu, że w ekstremalnych warunkach same pieniądze (log_gdp) czy elektryczność nie wystarczą, 
# by uratować statystykę, jeśli w kraju panuje np. głód lub wojna.

# Mozemy dowiedzieć się więcej o tych przeszacowaniach szukając 10 największych błędów (odstających obserwacji)

outliers <- test_set %>%
  mutate(
    error = life_exp - pred,      
    abs_error = abs(error)         
  ) %>%
  arrange(desc(abs_error)) %>%
  select(country, year, life_exp, pred, error, abs_error) %>%
  head(10)

print(outliers)
#Wniosek
# Model systematycznie przeszacowuje długość życia w tych regionach o 10-15 lat.
# Być może ynika to z faktu, że zmienne ekonomiczne nie uwzględniają gwałtownego wpływu epidemii HIV/AIDS, 
# która w tym okresie zdominowała czynniki demograficzne w tych państwach. Potwierdza to tezę,
# że w sytuacjach ekstremalnych kryzysów zdrowotnych, fundamenty ekonomiczne nieco tracą swoją siłę objaśniającą.


# Możemy również przeanalizować gdzie model "działa" najlepiej?

best_fits <- test_set %>%
  mutate(abs_error = abs(life_exp - pred)) %>%
  arrange(abs_error) %>%
  select(country, year, life_exp, pred, abs_error) %>%
  head(10)

print(best_fits)
# Wniosek:
# test Chowa i podzial na kraje bogate i biedne zadzialal, model nie faworyzuje jednej szerokości geograficznej 
# - potrafi tak samo dobrze opisać rzeczywistość w kraju o średniej długości życia 59 lat (Benin), jak i 82 lata (Francja).


# Analiza błędów w czasie
# Sprawdzamy, czy model radzi sobie tak samo dobrze w roku 2000, jak i w 2020

test_set %>%
  group_by(year) %>%
  summarise(mae_year = mean(abs(life_exp - pred))) %>%
  ggplot(aes(x = year, y = mae_year)) +
  geom_line(color = "pink", size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Średni błąd (MAE) w czasie",
       x = "Rok", y = "MAE (lata)")

# Analiza MAE w czasie wykazuje ogólną tendencję spadkową, 
# co świadczy o rosnącej sile objaśniającej modelu w nowszych okresach
# gwałtowny wzrost błędu w końcowym okresie może być efektem pandemii COVID-19


# Walidacja krzyżowa 

# Przygotowanie ramki na błędy
cv_results <- data.frame(MAE = numeric(), RMSE = numeric(), MAPE = numeric())

# Losujemy przynależność do 10 grup
n_folds <- 10
data_model$fold <- sample(rep(1:n_folds, length.out = nrow(data_model)))

# zanim przejdziemy do petli, musimy zadbac zeby struktury danych byly takie same

# Obliczamy medianę (tak jak wcześniej do testu Chowa)
prog <- median(data_model$log_gdp, na.rm = TRUE)

# Dodajemy zmienną rich do całego zbioru
data_model <- data_model %>%
  mutate(rich = as.integer(log_gdp > prog))

for(i in 1:n_folds) {
  train_cv <- data_model[data_model$fold != i, ]
  test_cv  <- data_model[data_model$fold == i, ]
  

  mod_cv <- lm(formula(model_chow_fix), data = train_cv)
  
  pred_cv <- predict(mod_cv, newdata = test_cv)
  err_cv  <- test_cv$life_exp - pred_cv
  
  cv_results <- rbind(cv_results, data.frame(
    MAE  = mean(abs(err_cv)),
    RMSE = sqrt(mean(err_cv^2)),
    MAPE = mean(abs(err_cv/test_cv$life_exp)) * 100
  ))
}

# Średnie błędy z walidacji 
colMeans(cv_results)

# Wizualizacja wyników walidacji krzyżowej
cv_results_long <- cv_results %>%
  pivot_longer(everything(), names_to = "Miar", values_to = "Wartosc")


ggplot(cv_results_long, aes(x = Miar, y = Wartosc, fill = Miar)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Stabilność błędów w 10-krotnej walidacji krzyżowej",
       y = "Wartość błędu", x = "") +
  scale_fill_brewer(palette = "Pastel1")

# pudelka są wąskie
# Przeprowadzona 10-krotna walidacja krzyżowa potwierdziła stabilność strukturalną modelu.
# Niskie zróżnicowanie miar MAE, RMSE i MAPE pomiędzy poszczególnymi próbami testowymi świadczy o tym, 
# że model nie jest nadmiernie dopasowany do danych treningowych (brak overfittingu). 
# Średni błąd procentowy (MAPE) utrzymujący się na poziomie ok. 4% pozwala uznać model 
# za wysoce wiarygodny w prognozowaniu długości życia


# Porównanie błedów w zbiorach treningowym, testowym, i bledow z walidacji krzyzowej
# Obliczamy błędy dla zbioru treningowego (dla porównania)
pred_train <- predict(model_chow_fix, newdata = train_set)
err_train <- train_set$life_exp - pred_train

summary_table <- data.frame(
  Metryka = c("MAE", "RMSE", "MAPE (%)"),
  Trening = round(c(mean(abs(err_train)), sqrt(mean(err_train^2)), mean(abs(err_train/train_set$life_exp))*100), 2),
  Test    = round(c(mae, rmse, mape), 2),
  Cross_Val = round(as.numeric(colMeans(cv_results)), 2)
)

print(summary_table)
# Różnice między kolumnami Trening, Test a Cross_Val są minimalne
# model świetnie generalizuje wiedzę na nowe, nieznane mu wcześniej dane
# Analiza porównawcza błędów ex-post dla zbiorów treningowego i testowego 
# oraz wyniki 10-krotnej walidacji krzyżowej jednoznacznie potwierdzają wysoką
# jakość predykcyjną modelu. Zbieżność miar MAE (ok. 2.6 roku) oraz MAPE (ok. 4.1%) 
# we wszystkich próbach świadczy o stabilności oszacowanych parametrów i braku overfittingu.



# ================================== Podsumowanie ======================================


# =======================   ETAP 1: PRZYGOTOWANIE I DOBÓR ZMIENNYCH  =============== 

# Usunięcie zmiennych z dużą liczbą braków , pozwoliło zachować dużą i reprezentatywną próbę (N=3140).

# Zastosowanie logarytmu dla PKB (log_gdp) było kluczowe. Relacja między zamożnością a długością życia 
# nie jest liniowa - każdy kolejny dolar daje największy przyrost życia w krajach biednych, a mniejszy w bogatych.

# Metoda Hellwiga vs AIC: Metoda Hellwiga wskazała na fundament (PKB, elektryczność, dzietność), ale kryterium AIC 
# i test ANOVA uzasadniły dodanie urbanizacji i wydatków na zdrowie, co zwiększyło siłę objaśniającą modelu do 80,5%.



# =================== ETAP 2: DIAGNOSTYKA I NAPRAWA MODELU ===============================

# test Chowa (F = 120): Wykazał on, że świat nie jest jednorodny. Kraje biedne i bogate rządzą się innymi prawami.
# Rozwiązaliśmy to wprowadzając zmienną rich

# RESET Ramsey'a:Poprawilismy postać funkcyjną poprzez wprowadzenie interakcji (urban * log_gdp), co zredukowało
# błąd specyfikacji z poziomu rażącego do akceptowalnego.

# Heteroskedastyczność: Zastosowanie odpornych błędów standardowych (HC3) pozwoliło na rzetelną ocenę istotności 
# zmiennych mimo zmiennej wariancji błędu (model bardziej "myli się" w krajach biednych).



# ===================  INTERPRETACJA PARAMETROW W OSTATECZNYM MODELU  ================
coeftest(model_chow_fix, vcov = vcovHC(model_chow_fix, type = "HC3"))

# fertility (-1.799) i wysoko ujemne t value : 
# Każde dodatkowe dziecko na kobietę skraca średnią 
# długość życia o około 1,8 roku. To proksy wysokiej śmiertelności 
#okołoporodowej i braku dostępu do planowania rodziny w krajach rozwijających się.

#electricity (0.151) i wysoko dodatnie t value: 
# Wzrost dostępu do elektryczności o 1 punkt procentowy wydłuża życie o 
# ok. 0,15 roku. To potwierdza, że infrastruktura energetyczna jest fundamentem nowoczesnej medycyny.

# Efekt PKB (log_gdp):
# W krajach biedniejszych (rich=0): wpływ PKB to -2.79 (ujemny).Nie oznacza to, że wzbogacenie szkodzi krajom biednym,
# a to, że sam suchy wzrost PKB w skrajnie zacofanych, wiejskich regionach jest statystycznie mniej istotny niż walka
# z wysoką dzietnością czy brak prądu
# W krajach bogatszych (rich=1): wplyw PKB to 2.43 - widać, że w krajach bogatych PKB staje się znacznie silniejszym "silnikiem" wzrostu życia

# Wydatki na zdrowie (health_exp)
# W krajach biedniejszych: parametr wynosi -0.12 (nieintuicyjne, ale często wynika to z faktu, że w biednych krajach wysoki % PKB na zdrowie 
# to często reakcja na kryzysy/epidemie, a nie profilaktyka).
# W krajach bogatszych: parametr wynosi 0.65 - w krajach bogatych każdy 1% PKB wydany na zdrowie realnie wydłuża życie o ponad pół roku.

# Urbanizacja i PKB (log_gdp:urban)
# parametr interakcji wynosi 0.045 - urbanizacja wzmacnia pozytywny wpływ pieniędzy. Im bogatszy kraj, tym bardziej życie w mieście 
# (dostęp do szpitali, kanalizacji) pomaga wydłużyć życie




# =========================   ETAP 4: PREDYKCJA, ANALIZA BLEDÓW   ===================

# Stabilność: Fakt, że błąd na zbiorze treningowym (2.62), testowym (2.64) i w walidacji krzyżowej (2.63) jest niemal identyczny, dowodzi, że model jest niezwykle stabilny.
# Dokładność (MAPE ~ 4.1%): Oznacza to, że model myli się średnio o jedynie 4%. To wynik, który w naukach społecznych pozwala uznać model za wysoce wiarygodny.
# Analiza wartosci odstajacych: czynniki ekonomiczne to nie wszystko – biologia i nagłe kryzysy zdrowotne mogą zdominować fundamenty gospodarcze.
# Model nie 'zapamiętał' konkretnych rekordów, lecz faktycznie zidentyfikował uniwersalne prawidłowości strukturalne wynika to ze 
# znikomych różnic między błędem MAE na zbiorze treningowym (2.62) a testowym (2.64) oraz wynikami walidacji krzyżowej (2.63)
# Analiza błędów w czasie wykazała, że model zyskuje na precyzji w nowszych okresach (tendencja spadkowa MAE)
# Wysoka precyzja predykcji zarówno dla krajów o niskiej (np. Benin), jak i wysokiej (np. Francja) długości życia potwierdza, 
# że zastosowanie testu Chowa i wprowadzenie zmiennej interakcyjnej rich było kluczowym zabiegiem, który pozwolił modelowi poprawnie opisać dwie różne rzeczywistości społeczno-gospodarcze.



# ============================== WNIOSEK KOŃCOWY ====================================

# Model udowodnił, że długość życia na świecie jest determinowana przede wszystkim przez zamożność (PKB) i bezpieczeństwo demograficzne (dzietność), 
# ale ich siła i sposób oddziaływania zależą od poziomu rozwoju kraju (podział na kraje bogate i biedne)
# z interakcji (rich:health_exp) wynika fundamentalna prawda: pieniądze wydawane bezpośrednio na system ochrony zdrowia "działają" dopiero od pewnego poziomu zamożności kraju.
# Model myli się średnio o jedynie 4% (MAPE), co w skali globalnej czyni go niezwykle uniwersalnym narzędziem do prognozowania trendów demograficznych.