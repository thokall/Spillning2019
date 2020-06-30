Spillningsinventering björn, Västerbottens län 2019
================

Detta bibliotek innehåller programkod och data som använts för
populationsberäkningar av björn, baserat på spillningsinventeringen i
Västerbottens län 2019. Koden är skriven i R och anropar programmet
[MARK](http://www.phidot.org/software/mark/) genom gränssnittet RMark.

``` r
library(tidyverse)
library(RMark) 
source("functions.R")

data2019 <- read_csv("Bjorn_AC_2019_spillning_0616.csv") %>% 
  rename(Datum = Funnetdato) %>% 
  mutate(Vecka = lubridate::isoweek(Datum)) %>% 
  arrange(Datum) %>% 
  mutate(Kon = ifelse(is.na(Kon), "Saknas", Kon))

fit2019 <- data2019 %>% 
  filter(Kon != "Saknas") %>% 
  fit_models()

fit2019 %>% 
  select(Modell = model, dAICc, Hanar,  Honor, Total) %>% 
  knitr::kable(caption = "Populationsskattningar för undersökta modeller baserat på inventeringen i Västerbottens län 2019. 
               Modellerna är rangordnade efter Akaikes informationskriterium med den högst rankade modellen överst.")
```

| Modell                          | dAICc | Hanar          | Honor          | Total          |
| :------------------------------ | ----: | :------------- | :------------- | :------------- |
| pi(1)p(time + mixture)f0(sex)   |   0.0 | 233 (196, 306) | 283 (238, 369) | 516 (416, 640) |
| pi(sex)p(time + mixture)f0(sex) |   2.0 | 234 (196, 308) | 282 (237, 368) | 516 (417, 638) |
| p(time)f0(sex)                  |   7.7 | 200 (185, 221) | 242 (225, 266) | 442 (413, 473) |
| p(time + sex)f0(sex)            |   9.7 | 198 (183, 223) | 243 (225, 271) | 441 (412, 472) |

Populationsskattningar för undersökta modeller baserat på inventeringen
i Västerbottens län 2019. Modellerna är rangordnade efter Akaikes
informationskriterium med den högst rankade modellen överst.
