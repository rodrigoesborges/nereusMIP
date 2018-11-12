anos <- seq(1995,2013,1)
hanselec <- hanoset[hanoset$ano < 2010 & hanoset$ano > 1994,]
produtoporhora <- hanselec[,3]/prodcm[,3]
prodcm[,4] <- produtoporhora
names(prodcm)[4] <- "produtoporhora"


hanselec2 <- hanoset[hanoset$ano < 2014 & hanoset$ano > 2009,]
prodhn <- hanselec2[,3]/prodcn[,3]
prodcn[,4] <- prodhn
names(prodcn)[4] <- "produtoporhora"

prodcm <- rbind(prodcm,prodcn)
write.csv2(prodcm,paste0("data/horas-set-e-prodporhora_",anos[1],"-",anos[length(anos)],".csv"),row.names = FALSE)
