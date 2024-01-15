pronunciamentos_n0 <- function(){
  library(rvest)
  url_base = "http://www.cpc.org.br/CPC/Documentos-Emitidos/Pronunciamentos"
  inicial <- read_html(url_base)  

  c1 <- inicial %>% html_nodes("a.txt") %>% html_text()
  c2 <- inicial %>% html_nodes("a.txt") %>% html_attr("href")
  pronunciamentos_0 <- data.frame(c1,c2)
  pronunciamentos_0$c2 <- paste0("http://www.cpc.org.br",c2)

  pronunciamentos_0  
}

niv0 <- pronunciamentos_n0()

pronunciamentos_n1 <- function(niv0, ativo = "sim"){
  
  n1 <- niv0
  linhas <- nrow(n1)
  lista1 <- data.frame()
  library(rvest)
  
  for (linha in 1:linhas){
    a <- read_html(n1[linha,2])
    a1 <- a %>% html_nodes("a") %>% html_text()
    a2 <- a %>% html_nodes("a") %>% html_attr("href")
    pronunciamentos_1 <- data.frame(a1,a2)
    pronunciamentos_1$a1 <- trimws(pronunciamentos_1$a1, which = c("left"))
    pronunciamentos_1$a1 <- trimws(pronunciamentos_1$a1, which = c("right"))
    pronunciamentos_1$a1 <- gsub("[\r\n]", "", pronunciamentos_1$a1)
    pronunciamentos_1 <- pronunciamentos_1[pronunciamentos_1$a1 == 'Pronunciamento',]
    
    if(nrow(pronunciamentos_1) == 0){
      next
    }
    
    if(ativo == "sim"){
      pronunciamentos_1 <- pronunciamentos_1[1,]
    }
    
    pronunciamentos_1$a2 <- paste0("http://static.cpc.aatb.com.br/Documentos/", basename(pronunciamentos_1$a2))
    lista1 <- rbind(lista1, pronunciamentos_1)
  }
  lista1
}

lista_01a <- pronunciamentos_n1(niv0, ativo = "não")


download_pronunciamentos <- function(links){
  linhas <- nrow(links)
  arquivos_cache <- list.files()
  
  for (linha in 1:linhas){
    if (basename(links[linha,2]) %in% arquivos_cache){
      next
    } else
      download.file(links[linha,2], basename(links[linha,2]), method = "wininet", mode = "wb")
  }
  
  Sys.sleep(5)
}

download_pronunciamentos(lista_01a)
