pretty_ci <- function(l, u, loglin = FALSE){
  if (loglin == FALSE){
    paste0("(", round(l), ", ", round(u), ")")
  }
  else
  {
    paste0("(", signif((exp(l)-1)*100, 3), ", ", signif((exp(u)-1)*100, 3), ")")
  }
}

process_data <- function(data){
  data %>% group_by(Individ, Kon, Vecka) %>% 
    summarise(n = 1) %>% 
    ungroup() %>% 
    arrange(Vecka) %>%
    pivot_wider(id_cols = c("Individ", "Kon"), 
                names_from = Vecka, values_from = n, 
                values_fill = c(n = 0)) %>% 
    unite("ch", -c("Individ", "Kon"), sep = "") %>% 
    RMark::process.data(model = "FullHet", groups = "Kon")
}

fit_models <- function(data){
  p.2 <- list(formula = ~time + mixture, share = TRUE)
  p.3 <- list(formula = ~time, share = TRUE)
  p.4 <- list(formula = ~time + Kon, share = TRUE)
  f0.1 <-  list(formula = ~Kon)
  pi.1 <- list(formula = ~Kon)
  pi.2 <- list(formula = ~1)
  model_list <- create.model.list("FullHet")
  fitted <- mark.wrapper(model_list, data = data, output = FALSE, silent = TRUE, delete = TRUE)
  
  table <- fitted$model.table %>% rownames_to_column("model_number") %>% 
    mutate(model_number = as.numeric(model_number),
           Hanar = map_dbl(model_number, ~fitted[[.x]][["results"]][["derived"]][["N Population Size"]][["estimate"]][1], NA) %>% round(),
           l = map_dbl(model_number, ~fitted[[.x]][["results"]][["derived"]][["N Population Size"]][["lcl"]][1], NA),
           u = map_dbl(model_number, ~fitted[[.x]][["results"]][["derived"]][["N Population Size"]][["ucl"]][1], NA),
           `Intervall han` = pretty_ci(l, u),
           `Honor` = map_dbl(model_number, ~fitted[[.x]][["results"]][["derived"]][["N Population Size"]][["estimate"]][2], NA) %>% round(),
           l = map_dbl(model_number, ~fitted[[.x]][["results"]][["derived"]][["N Population Size"]][["lcl"]][2], NA),
           u = map_dbl(model_number, ~fitted[[.x]][["results"]][["derived"]][["N Population Size"]][["ucl"]][2], NA),
           `Intervall hon` = pretty_ci(l, u),
           Total = `Hanar` + `Honor`,
           tot_var = map_dbl(model_number, ~fitted[[.x]][["results"]][["derived.vcv"]][["N Population Size"]] %>% sum()),
           l = Total / exp(1.96 * sqrt(log(1 + tot_var / Total^2))),
           u = Total * exp(1.96 * sqrt(log(1 + tot_var / Total^2))),
           `Intervall tot` = pretty_ci(l, u),
           fit = map(model_number, ~fitted[[.x]])) %>% 
    select(-model_number, -pi, -p, -c, -f0, -tot_var, -npar, -weight, -AICc, -Deviance, -l, -u)
  as.tibble(table)
}