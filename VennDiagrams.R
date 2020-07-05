#Social Media 
x <- list(A=sample(genes,300),B=sample(genes,525),C=sample(genes,440),D=sample(genes,350))
x <- list(Instagram=sample(df,df$Instagram),Facebook=sample(df,df$Facebook),Twitter=sample(df,df$Twitter),TikTok=sample(df,df$TikTok), Snapchat=sample(df,df$Snapchat),Sonstige=sample(df,Textfeld_4))


# nVennR
library(nVennR)
insta <- subset(df, Instagram == "1")$X_Antwort.ID
fb <- subset(df, Facebook == "1")$X_Antwort.ID
twit <- subset(df, Twitter == "1")$X_Antwort.ID
tt <- subset(df, TikTok == "1")$X_Antwort.ID
sc <- subset(df, Snapchat == "1")$X_Antwort.ID
sonst <- subset(df, Textfeld_4 != "")$X_Antwort.ID

myV <- plotVenn(list(Instagram=insta, Facebook=fb, Twitter=twit, TikTok=tt, Snapchat=sc, Sonstige=sonst), nCycles = 2000)
