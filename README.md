Spillning 2019
================

``` r
library(tidyverse)
library(RMark) 
library(readxl)
source("functions.R")

data2019 <- read_csv("Bjorn_AC_2019_spillningKorr.csv") %>% 
  rename(Datum = Funnetdato) %>% 
  mutate(Vecka = lubridate::isoweek(Datum),
         Ar = lubridate::year(Datum)) %>% 
  arrange(Datum) %>% 
  mutate(Kon = ifelse(is.na(Kon), "Saknas", Kon))

fit2019 <- data2019 %>% 
  filter(Kon != "Saknas") %>% 
  process_data() %>% 
  fit_models()

fit2019 %>% select(-fit) %>% 
  mutate(model = str_remove_all(model, "~")) %>% 
  knitr::kable(digits = 2, caption = "Resultat modellering 2019 års data")
```

| model                              | DeltaAICc | Hanar | Intervall han | Honor | Intervall hon | Total | Intervall tot |
| :--------------------------------- | --------: | ----: | :------------ | ----: | :------------ | ----: | :------------ |
| pi(1)p(time + mixture)c()f0(Kon)   |      0.00 |   253 | (208, 339)    |   305 | (251, 407)    |   558 | (441, 707)    |
| pi(Kon)p(time + mixture)c()f0(Kon) |      1.99 |   253 | (209, 340)    |   304 | (251, 403)    |   557 | (442, 701)    |
| pi(1)p(time)c()f0(Kon)             |     13.45 |   208 | (193, 231)    |   252 | (234, 277)    |   460 | (429, 494)    |
| pi(1)p(time + Kon)c()f0(Kon)       |     15.43 |   207 | (190, 234)    |   253 | (233, 283)    |   460 | (428, 494)    |
| pi(Kon)p(time)c()f0(Kon)           |     15.47 |   208 | (193, 231)    |   252 | (234, 277)    |   460 | (429, 494)    |
| pi(Kon)p(time + Kon)c()f0(Kon)     |     17.45 |   207 | (190, 234)    |   253 | (233, 283)    |   460 | (428, 494)    |

Resultat modellering 2019 års data
