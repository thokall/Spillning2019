pretty_ci <- function(l, u, loglin = FALSE){
  if (loglin == FALSE){
    paste0("(", round(l), ", ", round(u), ")")
  }
  else
  {
    paste0("(", signif((exp(l)-1)*100, 3), ", ", signif((exp(u)-1)*100, 3), ")")
  }
}

process_data <- function(data, model){
  data %>% group_by(Individ, Kon, Vecka) %>% 
    summarise(n = 1) %>% 
    ungroup() %>% 
    arrange(Vecka) %>%
    pivot_wider(id_cols = c("Individ", "Kon"), 
                names_from = Vecka, values_from = n, 
                values_fill = c(n = 0)) %>% 
    unite("ch", -c("Individ", "Kon"), sep = "") %>% 
    rename(sex = Kon) %>% 
    RMark::process.data(model = model, groups = "sex")
}

fit_models <- function(data){
  p.time.mixture <- list(formula = ~time + mixture, share = TRUE)
  p.time <- list(formula = ~time, share = TRUE)
  p.time.sex <- list(formula = ~time + sex, share = TRUE)
  pi.1 <- list(formula = ~1)
  pi.sex <- list(formula = ~sex)
  f0.sex <-  list(formula = ~sex)
  
  fit1 <- process_data(data, "FullHet") %>% 
    RMark::mark(model = "HetClosed", model.parameters = list(pi = pi.1, p = p.time.mixture, f0 = f0.sex), 
                output = FALSE, silent = TRUE, delete = TRUE)
  fit2 <- process_data(data, "FullHet") %>% 
    RMark::mark(model = "HetClosed", model.parameters = list(pi = pi.sex, p = p.time.mixture, f0 = f0.sex), 
                output = FALSE, silent = TRUE, delete = TRUE)
  fit3 <- process_data(data, "Closed") %>% 
    RMark::mark(model = "Closed", model.parameters = list(p = p.time.sex, f0 = f0.sex), 
                output = FALSE, silent = TRUE, delete = TRUE)
  fit4 <- process_data(data, "Closed") %>% 
    RMark::mark(model = "Closed", model.parameters = list(p = p.time, f0 = f0.sex), 
                output = FALSE, silent = TRUE, delete = TRUE)

  all_fit <- tibble(fit = list(fit1, fit2, fit3, fit4))
  table <- mutate(all_fit, 
                  Hanar = map_dbl(fit, ~.x[["results"]][["derived"]][["N Population Size"]][["estimate"]][1] %>% round()),
                  Honor = map_dbl(fit, ~.x[["results"]][["derived"]][["N Population Size"]][["estimate"]][2] %>% round()),
                  Total = Hanar + Honor,
                  l = map_dbl(fit, ~.x[["results"]][["derived"]][["N Population Size"]][["lcl"]][1]),
                  u = map_dbl(fit, ~.x[["results"]][["derived"]][["N Population Size"]][["ucl"]][1]),
                  Hanar = paste(Hanar, pretty_ci(l, u)),
                  l = map_dbl(fit, ~.x[["results"]][["derived"]][["N Population Size"]][["lcl"]][2]),
                  u = map_dbl(fit, ~.x[["results"]][["derived"]][["N Population Size"]][["ucl"]][2]),
                  Honor = paste(Honor, pretty_ci(l, u)), 
                  tot_var = map_dbl(fit, ~.x[["results"]][["derived.vcv"]][["N Population Size"]] %>% sum()),
                  l = Total / exp(1.96 * sqrt(log(1 + tot_var / Total^2))),
                  u = Total * exp(1.96 * sqrt(log(1 + tot_var / Total^2))),
                  Total = paste(Total, pretty_ci(l, u)),
                  AICc = map_dbl(fit, ~.x[["results"]][["AICc"]]),
                  dAICc = round(AICc - min(AICc), 1),
                  model = map_chr(fit, ~.x[["model.name"]] %>% 
                                    str_remove_all("~|c\\(\\)"))
  ) %>% select(-l, -u, -AICc) %>% 
    arrange(dAICc)
  table
}

prov_table <- function(data){
  veckovis <- data %>% 
    select(Individ, Kon, Vecka) %>% 
    distinct() %>% 
    group_by(Individ, Kon) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(Kon) %>% 
    summarise(`Antal prov, veckovis sammanslagning` = paste0(sum(n), " (", round(mean(n), 2), ")"))
  data %>%  group_by(Individ, Kon) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(Kon) %>% 
    summarise(`Antal individer` = n(), `Antal prov` = paste0(sum(n), " (", round(mean(n), 2), ")")) %>% 
    left_join(veckovis, by = "Kon") %>% 
    rename(`Kön` = Kon)
}
