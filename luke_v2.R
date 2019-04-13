library(tidyverse)

base <- data.table::fread("../train.csv")
#base %>% sapply(class) %>% table
base$SalePrice %>% boxplot

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
  return(list( numeric = num))
}

teste <- exploratoria(base)

exploratoria(data)
