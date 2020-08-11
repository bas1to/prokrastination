library(tidyverse)
library(reshape2)
library(data.table)

plot.for.categories = function(){
  w = my.read.csv("kategorien.csv")
  ggplot(data = w) + geom_bar(mapping = aes(x = Kategorien)) + coord_flip() +
    ggtitle("Geheimtippskategorien") + ylab("Haeufigkeit") + xlab("Kategorien")
}

my.read.csv = function(file.name) {
  read.csv(file.name, header = TRUE, encoding = "UTF-8")
}

getRowsWithSpecialValue = function(table, colname, value){
  return(table[which(table[colname] == value),])
}

delete.empty.rows = function(table, colname){
  return(table[which(table[colname] != ""),])
}

summarize.col = function(mycol) {
  mysum <- length(which(mycol != ""))
  return(mysum)
}

plotdata = function(table) {
  l = sapply(table, summarize.col)
  i <- length(l)
  names(l)[i] <- 'Sonstiges'
  l[2:i]
}

umfrage = my.read.csv("umfrageonline_final.csv")

make.studyfields.data = function(cols){
  mint = getRowsWithSpecialValue(umfrage, "Mathematik_Informatik_Naturwissenschaften", 1)
  med = getRowsWithSpecialValue(umfrage, "Medizin_Gesundheitswesen", 1)
  kult = getRowsWithSpecialValue(umfrage, "Geistes_Kulturwissenschaften_Gechichte", 1)
  lehramt = getRowsWithSpecialValue(umfrage, "Lehramt", 1)
  recht = getRowsWithSpecialValue(umfrage, "Rechts_Wirtschaftswissenschaften", 1)
  sozi = getRowsWithSpecialValue(umfrage, "Gesellschafts_Sozialwissenschaften", 1)
  musik = getRowsWithSpecialValue(umfrage, "Musik_Kunst_Gestaltung", 1)
  ing = getRowsWithSpecialValue(umfrage, "Ingenieurwissenschaften", 1)
  mint_data = plotdata(mint[cols])
  med_data = plotdata(med[cols])
  kult_data = plotdata(kult[cols])
  lehramt_data = plotdata(lehramt[cols])
  recht_data = plotdata(recht[cols])
  sozi_data = plotdata(sozi[cols])
  musik_data = plotdata(musik[cols])
  ing_data = plotdata(ing[cols])
  cbind(mint_data, med_data, kult_data, lehramt_data, recht_data, sozi_data, musik_data, ing_data)
}


# @param pos = "fill" or "dodge" or "identity"
# @param cols: columns to analyse, like c(4:15) or c(17:28)
# @param ylab.text : text for vertical axis
plot.studyfields = function(cols, pos, ylab.text){
  fields = make.studyfields.data(cols)
  dimnames(fields)[[1]] = c("Videos", "Phone", "Bett", "Essen", "Haushalt", "Kreatives Hobby", "Shoppen", "Ausgehen", 
                            "Computerspiele", "Social Media", "Sonstiges")
  fields_DT = setDT(as.data.frame(fields), keep.rownames = T)[]
  names(fields_DT)[2:9] <- c("MINT", "Medizin", "Geistes- und Kulturwiss.", "Lehramt", "Jura/Wirtschaft", "Sozialwiss.", "Musik/Kunst", "Ingenieurwiss.")
  fields_data <- melt(fields_DT)
  
  ggplot(data = fields_data, mapping = aes(x = variable, y = value, fill = rn)) + 
    geom_bar(position = pos, stat = "identity" , color = "black") +
    theme(axis.text.x = element_text(angle = 65, hjust = 1)) + ggtitle("Prokrastination nach Studienbereich") + xlab("Studienbereich") + ylab(ylab.text) + labs(fill = "Prokrastinationsart")
}



make.bms.data = function(cols){
  bachelor = getRowsWithSpecialValue(umfrage, "Abschluss.", "Bachelor")
  master = getRowsWithSpecialValue(umfrage, "Abschluss.", "Master")
  staatsex = getRowsWithSpecialValue(umfrage, "Abschluss.", "Staatsexamen")
  
  bac_data = plotdata(bachelor[cols])
  mas_data = plotdata(master[cols])
  stex_data = plotdata(staatsex[cols])
  bms = cbind(bac_data, mas_data, stex_data)
}

# @param pos = "fill" or "dodge" or "identity"
# @param cols: columns to analyse, like c(4:15) or c(17:28)
plot.abschluss.prokrastination = function(cols, pos, ylab.text){
  bms = make.bms.data(cols)
  bms_DT = setDT(as.data.frame(bms), keep.rownames = T)[]
  names(bms_DT)[2:4] <- c("Bachelor", "Master", "Staatsexamen")
  bms_data <- melt(bms_DT)
  
  ggplot(data = bms_data, mapping = aes(x = variable, y = value, fill = rn)) + 
    geom_bar(position = pos, stat = "identity" , color = "black") +
    ggtitle("Prokrastination nach Abschlussart") + xlab("Abschluss") + ylab(ylab.text) + labs(fill = "Prokrastinationsart")
}
  

plot.zufried.vs.zeit = function(){
  umfrage$Zufriedenheit_Leistung. = recode(umfrage$Zufriedenheit_Leistung., "sehr unzufriedenstellend - ich bin total unzufrieden" = "sehr unzufriedenstellend", "unzufriedenstellend - ich bin unzufrieden" = "unzufriedenstellend",  "ausreichend - ich bin weder zufrieden noch unzufrieden" = "ausreichend", "zufriedenstellend - ich bin zufrieden" = "zufriedenstellend", "sehr zufriedenstellend - ich bin mehr als zufrieden" = "sehr zufriedenstellend")
  lvls = levels(umfrage$Zufriedenheit_Leistung.)
  l = c(lvls[2], lvls[4], lvls[1], lvls[5], lvls[3])
  umfrage$Zufriedenheit_Leistung. = factor(umfrage$Zufriedenheit_Leistung., levels =  l)
  lvls = levels(umfrage$Zeit_Insgesamt_Prokrastiniert.)
  l = c(lvls[2], lvls[1], lvls[4], lvls[3])
  umfrage$Zeit_Insgesamt_Prokrastiniert. = factor(umfrage$Zeit_Insgesamt_Prokrastiniert., levels =  l)
  
  ggplot(data = umfrage) + geom_bar(mapping = aes(x = Zufriedenheit_Leistung., fill = Zeit_Insgesamt_Prokrastiniert.), position = "dodge") +
    xlab("Leistungszufriedenheit") + ylab("Haeufigkeit (Anzahl)") + labs(fill = "Insgesamte Prokrastinationszeit") +
    coord_flip() + scale_fill_discrete(breaks = c("sehr viel", "viel", "mittel", "nicht so viel"))
}
