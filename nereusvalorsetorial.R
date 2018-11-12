library(XML)
library(RCurl)
library(readxl)
library(dplyr)
require('ggplot2')
require('reshape2')
require('stringr')

pabas <- "~/RLocalData/MIP-BR/"
mipsnereus <- getURL("http://www.usp.br/nereus/?fontes=dados-matrizes",.opts = list(ssl.verifypeer = FALSE) )
links <- getHTMLLinks(mipsnereus)
links <- links[grep("MIP-BR*", links)]

##referencias futuras - importante a segunda
#conclaurl <- "https://cnae.ibge.gov.br/images/concla/downloads/cnae1_0.zip"
#conclalista <-"https://cnae.ibge.gov.br/images/concla/downloads/CNAE1.0.xls"

cnae95 <- read_excel("~/RLocalData/CNAE1.0.xls")
cnaediv95 <- cnae95[-which(is.na(cnae95$X__1)),]
cnaediv95 <- cnaediv95[c(2,5)]
names(cnaediv95) <- cnaediv95[1,]
cnaediv95 <- cnaediv95[2:nrow(cnaediv95),]
write.csv2(cnaediv95,"~/RLocalData/cnae95divisoes.csv")

#Salvar Arquivos e nomes de planilhas
for (link in links) {
  pedaco <- unlist(strsplit(link,"/"))
  arqdest <- paste0("~/RLocalData/MIP-BR/",pedaco[length(pedaco)])
  download.file(link,arqdest)
  foias <-excel_sheets(path = arqdest)
  write.csv2(foias, file = gsub('(.*)(\\.xl)(.*)','\\1-folhas.csv',arqdest))
}
### Criar tabela de produção 1995 a 2009 
producao <- data.frame(matrix(ncol = 46, nrow = 0))


### Criar tabela de produção 2010 a 2013, com nome das colunas
producaonova <- data.frame(matrix(ncol = 72, nrow = 0))




#Criar as séries de produção por setores
for (link in links) {
  pedaco <- unlist(strsplit(link,"/"))
  arqleitura <- paste0(pabas,pedaco[length(pedaco)])
  if (length(grep("56S",arqleitura)) >0 ) next
  anoarq <- as.numeric(gsub('(.*-)([[:digit:]]{4})(.*)','\\2',arqleitura))
  fprod <- read_excel(arqleitura, "Producao",col_names = TRUE)

  if (anoarq < 2010){
    
    fprod <- t(fprod[,84][c(-1,-2),])
    fprod[1,1] <- anoarq
    fprod[1,] <-as.numeric(fprod[1,])
    
    producao <- rbind(producao,fprod, stringsAsFactors = FALSE)
      }
  else {

    fprod <- t(fprod[,132][c(-1,-2),])
    fprod[1,1] <- anoarq
    fprod[1,] <-as.numeric(fprod[1,])
    producaonova <- rbind(producaonova,fprod,stringsAsFactors = FALSE)

      }
}

producao[] <- lapply(producao,function(x) as.numeric(as.character(x)))
producaonova[] <- lapply(producaonova,function(x) as.numeric(as.character(x)))
### Adicionar nomes das colunas
# Adicionar a tabela de 95
cabs <- unlist(strsplit(links[1],"/"))
cabs <- cabs[length(cabs)]
cab <- t(read_excel(paste0(pabas,cabs),"Producao"))
#cab <- paste0(cab[1,]," - ",cab[2,])
cab <- cab[1,]
cab <- cab[c(-1,-2)]
cab[1] <- "Ano"
names(producao) <- cab
#Adicionar à tabela de 2010
cabs <- unlist(strsplit(links[length(links)],"/"))
cabs <- cabs[length(cabs)]
cab <- t(read_excel(paste0(pabas,cabs),"Producao"))
cab <- cab[1,]
#cab <- paste0(cab[1,]," - ",cab[2,])
cab <- cab[c(-1,-2)]
cab[1] <- "Ano"
names(producaonova) <- cab

producao <- producao[!is.na(names(producao))]
producaonova <- producaonova[!is.na(names(producaonova))]

###########PARA MIP 42 SETORES


## setores de acordo à CNAE 95 Divisão - Escolhidos - 
#escset <- data.frame(cnae95 = c("01","05","15","18","21","23","27"),mip42 = c("01","01","24","22","14","17","05"), stringsAsFactors = FALSE)

  prodc <- producao[,c("01","01","24","22","14","17","06")]
  row.names(prodc) <- producao[,1]
# Alimentos e Bebidas - 24 ao 30
  alim <- producao[,c("24","25","26","27","28","29","30")]
  alim <- rowSums(alim)
 # names(alim) <- producao[,1]
  
  prodc[,3] <- alim

# Metalurgia básica 
  metal <- producao[,c("05","06","07")]
  metal <- rowSums(metal)
#  names(metal) <- producao[,1]
  
  prodc[,7] <- metal
  
  ## Ideal : Dados médios 2010 a 2013 -> pesca e aquicultura em proporção ao setor 0280 (Florestal, pesca e aqui)
  pesca280 <- (9162+335+210)/29049
  
  ## Dados médios 2010 a 2013 -> pesca e aqu. em proporção a total de agropecuária+0280
  aqagropec <- 29049/(29049+115344+265107)
  
  ## composicao
  pescagrop <- pesca280*aqagropec
  
  ## Setor AGROPEC se transformará em 1-pescagrop * AGROPEC, nova linha criada pescagrop * AGROPEC
  prodc[,1] <- prodc[,1]*(1-pescagrop)
  prodc[,2] <- prodc[,2]*pescagrop
  
  names(prodc) <- c("01","05","15","18","21","23","27")
  prodc <- add_rownames(prodc, var = "Ano")
  prodcm <- melt(prodc)
  names(prodcm) <- c("ano","divisao","valorprod")
  prodcm <- prodcm[order(prodcm$ano,prodcm$divisao),]
  
  ###########PARA MIP 68 SETORES
  
  prodcn <- producaonova[,c("0191","0280","1091","1400","1700","1991","2491")]
  
  
  ###agricultura e pecuária 0191 + 0192
  agrpec <- producaonova[,c("0191","0192")]
  agrpec <- rowSums(agrpec)
  prodcn[,1] <- agrpec
  
  #### pesca aquicultura = pesca280 * 0280
  prodcn[,2] <- prodcn[,2]*pesca280
  
  ### alimentos e bebidas 1091+1092+1093+1100
  alimn <- producaonova[,c("1091","1092","26","27","28","29","30")]
  alimn <- rowSums(alimn)
  prodcn[,3] <- alimn
  
  ### coque petróleo e biocombustíveis 1991+1992
  coq <- producaonova[,c("1991","1992")]
  coq <- rowSums(coq)
  
  prodcn[,6] <- coq
  
  ### metalurgia básica 2491+2492
  metn <- producaonova[,c("2491","2492")]
  metn <- rowSums(metn)
  
  prodc[,7] <- metn
  row.names(prodcn) <- producaonova[,1]
  
  names(prodcn) <- c("01","05","15","18","21","23","27")
  prodcn <- add_rownames(prodcn, var = "Ano")
  prodcn <- melt(prodcn)
  names(prodcn) <- c("ano","divisao","valorprod")
  prodcn <- prodcn[order(prodcn$ano,prodcn$divisao),]
  
  #gsub('(.* .* .*?) (.*?)','\\1\n\\2', dados a substituir o terceiro espaço)
