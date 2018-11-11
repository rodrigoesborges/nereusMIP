library(XML)
library(RCurl)
library(readxl)

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
producao <- data.frame(matrix(ncol = 48, nrow = 0))


### Criar tabela de produção 2010 a 2013, com nome das colunas
producaonova <- data.frame(matrix(ncol = 74, nrow = 0))




#Criar as séries de produção por setores
for (link in links) {
  pedaco <- unlist(strsplit(link,"/"))
  arqleitura <- paste0(pabas,pedaco[length(pedaco)])
  if (length(grep("56S",arqleitura)) >0 ) next
  anoarq <- as.numeric(gsub('(.*-)([[:digit:]]{4})(.*)','\\2',arqleitura))
  fprod <- read_excel(arqleitura, "Producao",col_names = TRUE)

  if (anoarq < 2010){
    
    fprod <- t(fprod[,84])
    fprod[1,1] <- anoarq

    
    producao <- rbind(producao,fprod)
      }
  else {

    fprod <- t(fprod[,132])
    fprod[1,1] <- anoarq

    producaonova <- rbind(producaonova,fprod)

      }
}


### Adicionar nomes das colunas
# Adicionar a tabela de 95
cabs <- unlist(strsplit(links[1],"/"))
cabs <- cabs[length(cabs)]
cab <- t(read_excel(paste0(pabas,cabs),"Producao"))
cab <- paste0(cab[1,]," - ",cab[2,])
cab[1] <- "Ano"
names(producao) <- cab
#Adicionar à tabela de 2010
cabs <- unlist(strsplit(links[length(links)],"/"))
cabs <- cabs[length(cabs)]
cab <- t(read_excel(paste0(pabas,cabs),"Producao"))
cab <- paste0(cab[1,]," - ",cab[2,])
cab[1] <- "Ano"
names(producaonova) <- cab



###########PARA MIP 42 SETORES
## Dados médios 2010 a 2013 -> pesca e aquicultura em proporção ao setor 0280 (Florestal, pesca e aqui)
pesca280 <- (9162+335+210)/29049

## Dados médios 2010 a 2013 -> pesca e aqu. em proporção a total de agropecuária+0280
aqagropec <- 29049/(29049+115344+265107)

## composicao
pescagrop <- pesca280*aqagropec

## Setor AGROPEC se transformará em 1-pescagrop * AGROPEC, nova linha criada pescagrop * AGROPEC

#gsub('(.* .* .*?) (.*?)','\\1\n\\2', dados a substituir o terceiro espaço)
