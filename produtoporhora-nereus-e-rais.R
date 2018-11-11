hanselec <- hanoset[hanoset$ano < 2010 & hanoset$ano > 1994,]
produtoporhora <- hanselec[,3]/prodcm[,3]
prodcm[,4] <- produtoporhora
names(prodcm)[4] <- "produtoporhora"
anos <- seq(1995,2009,1)
write.csv2(prodcm,paste0("data/horas-set-e-prodporhora_",anos[1],"-",anos[length(anos)],".csv"),row.names = FALSE)
