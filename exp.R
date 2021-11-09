library(tidyverse)

base <- iris %>% as.data.frame() %>% mutate(Varcat = "C")

exploratoria <- function(base,grupo=NULL){
  aux_exploratoria <- function(base){
    # Funcao auxiliar
    is_numerical <- function(x) ifelse(is.numeric(x) | is.integer(x),T,F)
    
    aux_cat <- function(data,x){
      
      data <- data %>% 
        mutate_if(is.factor,as.character) %>% 
        select_if(is.character) %>% 
        mutate_all(funs(ifelse(is.na(.) | .=="", "NA", .))) %>% 
        sapply(function(x) table(x)) %>% unlist() %>% 
        as.data.frame() %>% 
        tibble::rownames_to_column("Name")
      
      if(x>1){
        data <- data %>% 
          tidyr::separate(Name, 
                          c("Var","Cat"), 
                          sep = "\\.", 
                          remove = F, 
                          extra = "merge") %>% 
          group_by(Var) %>% 
          mutate(Perc = ./sum(.)) %>% 
          ungroup() %>% 
          dplyr::rename(n = ".") %>% 
          select(-Name)
      }
      
      return(data)
    }
    
    aux_num <- function(data){
      data_aux <- data %>% select_if(is_numerical) 
      data_aux <- data_aux %>% 
        sapply(function(x) summary(x %>% na.omit)) %>% t %>% 
        as.data.frame %>% 
        tibble::rownames_to_column(var = "Name") %>% 
        bind_cols("QTD_NA" = data_aux %>% sapply(function(x) sum(is.na(x))/nrow(base))) %>% 
        bind_cols("SD" = data_aux %>% sapply(function(x) sd(x,na.rm = T))) %>% 
        bind_cols("p1" = data_aux %>% sapply(function(x) quantile((x),na.rm = T,probs = c(0.01)))) %>%
        bind_cols("p5" = data_aux %>% sapply(function(x) quantile((x),na.rm = T,probs = c(0.05)))) %>%
        bind_cols("p10" = data_aux %>% sapply(function(x) quantile((x),na.rm = T,probs = c(0.1)))) %>%
        bind_cols("p90" = data_aux %>% sapply(function(x) quantile((x),na.rm = T,probs = c(0.90)))) %>%
        bind_cols("p95" = data_aux %>% sapply(function(x) quantile((x),na.rm = T,probs = c(0.95)))) %>%
        bind_cols("p99" = data_aux %>% sapply(function(x) quantile((x),na.rm = T,probs = c(0.99)))) %>%
        select("Name","Min.","p1","p5","p10","1st Qu.","Median","Mean","3rd Qu.","p90","p95","p99","Max.","SD","QTD_NA") %>% 
        mutate(PERC_NA = QTD_NA/nrow(data))
      return(data_aux)
    }
    
    # Verificando se existe var categoricas ou numericas
    indica_fac <- c('factor','character') %in% (base %>% sapply(class) %>% unique()) %>% sum
    indica_num <- c('numeric','integer','integer64') %in% (base %>% sapply(class) %>% unique()) %>% sum
    
    # Aplicando a exploratoria
    fac <- NA
    num <- NA
    ## Variaveis Categoricas
    if(indica_fac>=1) fac <- aux_cat(base,indica_fac)
    
    # Variaveis Numericas
    if(indica_num>=1) num <- aux_num(base)
    
    return(list(numeric = num,categorical = fac))
  }
  if(is.null(grupo)) return(aux_exploratoria(base))
  
  else{
    base <- base %>% rename(Nome_Grupo=grupo)
    seg <- base %>% select(Nome_Grupo) %>% pull %>% unique
    cats <- data.frame()
    nums <- data.frame()
    
    
    for(i in seg){
      aux <- base %>% filter(Nome_Grupo==i) %>% select(-Nome_Grupo) %>% aux_exploratoria()
      if(!is.null(aux$categorical)) cats <- cats %>% bind_rows(aux$categorical %>% mutate(GRUPO=i))
      if(!is.null(aux$numeric)) nums <- nums %>% bind_rows(aux$numeric %>% mutate(GRUPO=i))
    }
    return(list(categorical=cats,numeric=nums))
  }
}

teste <- exploratoria(base,grupo = "Species")

teste$categorical %>% View
teste$numeric %>% View



