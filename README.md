Spillning 2019
================

``` r
library(tidyverse)
library(RMark) 
library(readxl)
source("functions.R")

data2019 <- read_csv("Bjorn_AC_2019_spillningKorrKon.csv") %>% 
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
| pi(1)p(time + mixture)c()f0(Kon)   |      0.00 |   233 | (196, 306)    |   283 | (238, 369)    |   516 | (416, 640)    |
| pi(Kon)p(time + mixture)c()f0(Kon) |      2.01 |   234 | (196, 308)    |   282 | (237, 368)    |   516 | (417, 638)    |
| pi(1)p(time)c()f0(Kon)             |      9.74 |   200 | (185, 221)    |   242 | (225, 266)    |   442 | (413, 473)    |
| pi(1)p(time + Kon)c()f0(Kon)       |     11.69 |   198 | (183, 223)    |   243 | (225, 271)    |   441 | (412, 472)    |
| pi(Kon)p(time)c()f0(Kon)           |     11.75 |   200 | (185, 221)    |   242 | (225, 266)    |   442 | (413, 473)    |
| pi(Kon)p(time + Kon)c()f0(Kon)     |     13.71 |   198 | (183, 222)    |   243 | (225, 271)    |   441 | (412, 472)    |

Resultat modellering 2019 års data
