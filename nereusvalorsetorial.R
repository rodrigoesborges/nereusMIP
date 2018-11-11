library(XML)
library(RCurl)
library(readxl)

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
producao <- data.frame(matrix(nrow = 48, ncol = 0))
producaonova <- data.frame(nrow = 74, ncol = 0)

#Criar as séries de produção por setores
for (link in links) {
  pedaco <- unlist(strsplit(link,"/"))
  arqleitura <- paste0("~/RLocalData/MIP-BR/",pedaco[length(pedaco)])
  if (length(grep("56S",arqleitura)) >0 ) next
  anoarq <- as.numeric(gsub('(.*-)([[:digit:]]{4})(.*)','\\2',arqleitura))
  fprod <- read_excel(arqleitura, "Producao",col_names = TRUE)

  if (anoarq < 2010){
    
    fprod <- fprod[,84]
    fprod[1,1] <- anoarq

    
    producao <- cbind(producao,fprod)
      }
  else {

    fprod <- fprod[,132]
    fprod[1,1] <- anoarq

    producaonova <- cbind(producaonova,fprod)

      }
}



#gsub('(.* .* .*?) (.*?)','\\1\n\\2', dados a substituir o terceiro espaço)
