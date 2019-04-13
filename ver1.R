library(tidyverse)
library(ggplot2)

base <- data.table::fread("../employee_reviews.csv", dec = ".")
base <- base %>% mutate_if(is.integer, as.numeric) %>%
  rename_all(. %>% gsub("-", "_", .))

#base %>% sapply(class) %>% table
base$SalePrice %>% boxplot

#Esta funcionando - 13/04
exploratoria <- function(base){
  #Variaveis Categoricas
  fac <- base %>% select_if(is.character) %>%
    mutate_all(funs(if_else(is.na(.), "NA", .))) %>%
    sapply(function(x) table(x)) %>% unlist %>% as.data.frame() %>%
    tibble::rownames_to_column(var = "Name") %>%
    separate(Name, c("Var", "Cat"), sep = "\\.", remove=FALSE, extra = "merge") %>%
    mutate(Perc = ./nrow(base)) %>%
    select(Var, Cat, ".", Perc) %>%
    rename(n = ".")
  #Variaveis Numericas
  num <- base %>% select_if(is.numeric) %>%
    sapply(function(x) summary(x %>% na.omit)) %>% t %>% as.data.frame %>%
    tibble::rownames_to_column(var = "Name") %>%
    bind_cols("NA" = base %>% select_if(is.numeric) %>% sapply(function(x) sum(is.na(x))/nrow(base))) %>%
    select("Name", "Min.","1st Qu.","Median","Mean","3rd Qu.","Max.", "NA")
  return(list(fac=fac, numeric = num))
}

teste <- exploratoria(base)

#Graficos
# Essa função foi feita para analisar a variável de interesse em relação com outra variável explicativa
# A variável de interesse pode ser numérica ou categorica


graph_resp <- function(base, resp, var2){
  base %>% #mutate_at(vars(overall_ratings), as.factor) %>%
    ggplot(aes_string(x=var2, fill = resp)) +
    geom_bar(position = "fill")

}

base %>% mutate_at(vars(overall_ratings), as.factor) %>%
  graph_resp(resp = "overall_ratings", var2 = "company")
