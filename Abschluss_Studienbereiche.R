library(tidyverse)
library(reshape2)
library(data.table)

## read.cvs
read.cvs = function(file.name) {
  read.csv("umfrageonline_final.csv", header = TRUE)
}

delete.empty.rows = function(table, colname, toCheck){
  return(table[which(table[colname] == toCheck),])
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

umfrage = read.cvs("umfrageonline_final.csv")

mint = delete.empty.rows(umfrage, "Mathematik_Informatik_Naturwissenschaften", 1)
med = delete.empty.rows(umfrage, "Medizin_Gesundheitswesen", 1)
kult = delete.empty.rows(umfrage, "Geistes_Kulturwissenschaften_Gechichte", 1)
lehramt = delete.empty.rows(umfrage, "Lehramt", 1)
recht = delete.empty.rows(umfrage, "Rechts_Wirtschaftswissenschaften", 1)
sozi = delete.empty.rows(umfrage, "Gesellschafts_Sozialwissenschaften", 1)
musik = delete.empty.rows(umfrage, "Musik_Kunst_Gestaltung", 1)
ing = delete.empty.rows(umfrage, "Ingenieurwissenschaften", 1)


# mint_data = plotdata(mint[4:15])
# med_data = plotdata(med[4:15])
# kult_data = plotdata(kult[4:15])
# lehramt_data = plotdata(lehramt[4:15])
# recht_data = plotdata(recht[4:15])
# sozi_data = plotdata(sozi[4:15])
# musik_data = plotdata(musik[4:15])
# ing_data = plotdata(ing[4:15])
# 
mint_data = plotdata(mint[17:28])
med_data = plotdata(med[17:28])
kult_data = plotdata(kult[17:28])
lehramt_data = plotdata(lehramt[17:28])
recht_data = plotdata(recht[17:28])
sozi_data = plotdata(sozi[17:28])
musik_data = plotdata(musik[17:28])
ing_data = plotdata(ing[17:28])

# mint_data = plotdata(mint[37:47])
# med_data = plotdata(med[37:47])
# kult_data = plotdata(kult[37:47])
# lehramt_data = plotdata(lehramt[37:47])
# recht_data = plotdata(recht[37:47])
# sozi_data = plotdata(sozi[37:47])
# musik_data = plotdata(musik[37:47])
# ing_data = plotdata(ing[37:47])

t = cbind(mint_data, med_data, kult_data, lehramt_data, recht_data, sozi_data, musik_data, ing_data)
k = setDT(as.data.frame(t), keep.rownames = T)[]
names(k)[2:9] <- c("MINT", "Medizin", "Geistes- und Kulturwiss.", "Lehramt", "Jura/Wirtschaft", "Sozialwiss.", "Musik/Kunst", "Ingenieurwiss.")
dfp1 <- melt(k)

ggplot(data = dfp1, mapping = aes(x = variable, y = value, fill = rn)) + 
  geom_bar(position = "dodge", stat = "identity" , color = "black") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) + ggtitle("Prokrastination nach Studienbereich") + xlab("Studienbereich") + ylab("Häufigkeit (Anzahl)") + labs(fill = "Prokrastinationsart")


ggplot(data = dfp1, mapping = aes(x = variable, y = value, fill = rn)) + 
  geom_bar(position = "fill", stat = "identity" , color = "black") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) + ggtitle("Prokrastination nach Studienbereich") + xlab("Studienbereich") + ylab("Häufigkeit (Prozent)") + labs(fill = "Prokrastinationsart")





bachelor = delete.empty.rows(umfrage, "Abschluss.", "Bachelor")
master = delete.empty.rows(umfrage, "Abschluss.", "Master")
staatsex = delete.empty.rows(umfrage, "Abschluss.", "Staatsexamen")

bac_data = plotdata(bachelor[4:15])
mas_data = plotdata(master[4:15])
stex_data = plotdata(staatsex[4:15])

t = cbind(bac_data, mas_data, stex_data)
k2 = setDT(as.data.frame(t), keep.rownames = T)[]
names(k2)[2:4] <- c("Bachelor", "Master", "Staatsexamen")
dfp1 <- melt(k2)

p_abschluss = ggplot(dfp1, aes(x = rn, y= value, fill = variable)) +
  geom_bar(stat="identity", width=0.8, position = "dodge") 
p_abschluss  + theme(axis.text.x = element_text(angle = 65, hjust = 1)) + ggtitle("Prokrastination nach Abschlussart") + xlab("Prokrastinantionsart") + ylab("Häufigkeit (Anzahl)") + labs(fill = "Abschluss")

ggplot(data = dfp1, mapping = aes(x = variable, y = value, fill = rn)) + 
  geom_bar(position = "dodge", stat = "identity" , color = "black") +
  ggtitle("Prokrastination nach Abschlussart") + xlab("Abschluss") + ylab("Haeufigkeit (Anzahl)") + labs(fill = "Prokrastinationsart")


ggplot(data = dfp1, mapping = aes(x = variable, y = value, fill = rn)) + 
  geom_bar(position = "fill", stat = "identity" , color = "black") +
  ggtitle("Prokrastination nach Abschlussart") + xlab("Abschluss") + ylab("Haeufigkeit (Anzahl)") + labs(fill = "Prokrastinationsart")



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