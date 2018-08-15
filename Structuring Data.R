###STRUCTURING THE DATA

#Test import on Herb Treatment, Mar 17 (Budbreak)

#Rep A
#Heb_MarA <- read_excel("C:/Users/nickbrowen/Desktop/STAT Summer Research/Copy of Bacteria 2017.xlsm", 
#            sheet = "A1.HERB.Mar17.A", skip = 4)
#Heb_MarA <- Heb_MarA[-1,]

#Rep B
#Heb_MarB <- read_excel("C:/Users/nickbrowen/Desktop/STAT Summer Research/Copy of Bacteria 2017.xlsm", 
#                       sheet = "A2.HERB.Mar17.B", skip = 4)
#Heb_MarB <- Heb_MarB[-1,]

#Rep C
#Heb_MarB <- read_excel("C:/Users/nickbrowen/Desktop/STAT Summer Research/Copy of Bacteria 2017.xlsm", 
#                      sheet = "A2.HERB.Mar17.B", skip = 4)
#Heb_MarB <- Heb_MarB[-1,]

##Importing all Sheets

# get sheet names
library(readxl)
pathname <- "C:/Users/nickbrowen/Desktop/STAT Summer Research/Copy of Bacteria 2017.xlsm"
sheet_names <- excel_sheets(pathname)
sheet_names2 <- noquote(excel_sheets(pathname))

#import all data sheets
for (i in 1:length(sheet_names)){
  assign(paste(toString(sheet_names2[i])),
    read_excel(pathname, sheet = sheet_names[i], skip = 4)[-1,])
}

#putting all the data frames into a list
rawlist <- list()
for (i in 1:length(sheet_names)){
  rawlist[[sheet_names2[i]]] <- read_excel(pathname, sheet = sheet_names[i], skip = 4)[-1,]
}


##Summing Amount by order within each replicate

#figuring it out on one
#sum_matrix <- rowsum(A1.HERB.Mar17.A$Amount, A1.HERB.Mar17.A$Order)
#assign(paste("summed", "A1.HERB.Mar17.A", sep = "_"), data.frame(Order = rownames(sum_matrix), 
#           Amount = sum_matrix, row.names = NULL))

#big loop to do all

#for (i in 1:length(sheet_names)){
#  sum_matrix <- rowsum(rawlist[[sheet_names2[i]]][,2], rawlist[[sheet_names2[i]]][,7], na.rm = T)
#  assign(paste("summed", sheet_names[i], sep = "_"), 
#         data.frame(Order = rownames(sum_matrix), Amount = sum_matrix, row.names = NULL))
#}

#adding seasons column and summing by order

ADEFGH12 <- rep(rep(c("Budbreak", "Bloom", "Veraison", "Harvest"), each = 3))
BC10 <- rep(c(rep("Budbreak",3), rep("Bloom",2), rep("Veraison",3), rep("Harvest",2)),2)
D11 <- c(rep("Budbreak",3), rep("Bloom",2), rep("Veraison",3), rep("Harvest",3))
season_loop <- c("bacteria",ADEFGH12, BC10,D11, rep(ADEFGH12,4))


for (i in 1:length(sheet_names)){
    sum_matrix <- rowsum(unlist(rawlist[[sheet_names2[i]]][,2], use.names = F), unlist(rawlist[[sheet_names2[i]]][,7], use.names = F), na.rm = T)
    assign(paste("summed", sheet_names[i], sep = "_"), 
           data.frame(Order = rownames(sum_matrix), Amount = sum_matrix, 
                      Season = rep(season_loop[i], length(sum_matrix)),row.names = NULL))
}


##Averaging the 3 replicates

#ab <- merge(a,b,all=T)
#abc <- merge(ab,c,all = T)
#tapplyabc <- tapply(abc$Number, abc$Animal, mean)
#tabc <- data.frame(Animal = names(tapplyabc), AvgNum = tapplyabc, row.names = NULL)


merge.avg <- function(Arep, Brep, Crep = NULL) {
  merged <- merge(Arep, Brep, all = T)
  if (is.null(Crep) == F){
    merged <- merge(merged, Crep, all = T)
  }
  order.averages <- tapply(merged$Amount, merged$Order, mean)
 
  return(data.frame(Order = names(order.averages), Avg_Amount = order.averages,
                            row.names = NULL, Season = rep(Arep[1,3], length(dim(merged)[1]))))
}


H.Mar.avg <- merge.avg(summed_A1.HERB.Mar17.A, summed_A2.HERB.Mar17.B, summed_A3.HERB.Mar17.C)
H.May.avg <- merge.avg(summed_A4.HERB.May.A, summed_A5.HERB.May.B, summed_A6.HERB.May.C)
H.Jul.avg <- merge.avg(summed_A7.HERB.Jul.A, summed_A8.HERB.Jul.B, summed_A9.HERB.Jul.C)
H.Sep.avg <- merge.avg(summed_A10.HERB.Sep.A, summed_A11.HERB.Sep.B, summed_A12.HERB.Sep.C)

NH.Mar.avg <- merge.avg(summed_B1.NH.Mar.A, summed_B2.NH.Mar.B, summed_B3.NH.Mar.C)
NH.May.avg <- merge.avg(summed_B4.NH.May.A, summed_B6.NH.May.C)
NH.Jul.avg <- merge.avg(summed_B7.NH.Jul.A, summed_B8.NH.Jul.B, summed_B9.NH.Jul.C)
NH.Sep.avg <- merge.avg(summed_B10.NH.Sep.A, summed_B11.NH.Sep.B)

SF.Mar.avg <- merge.avg(summed_C1.SF.Mar.A, summed_C2.SF.Mar.B, summed_C3.SF.Mar.C)
SF.May.avg <- merge.avg(summed_C4.SF.May.A, summed_C6.SF.May.C)
SF.Jul.avg <- merge.avg(summed_C7.SF.Jul.A, summed_C8.SF.Jul.B, summed_C9.SF.Jul.C)
SF.Sep.avg <- merge.avg(summed_C10.SF.Sep.A, summed_C11.SF.Sep.B)

OF.Mar.avg <- merge.avg(summed_D1.OF.Mar.A, summed_D2.OF.Mar.B, summed_D3.OF.Mar.C)
OF.May.avg <- merge.avg(summed_D5.OF.May.B, summed_D6.OF.May.C)
OF.Jul.avg <- merge.avg(summed_D7.OF.Jul.A, summed_D8.OF.Jul.B, summed_D9.OF.Jul.C)
OF.Sep.avg <- merge.avg(summed_D10.OF.Sep.A, summed_D11.OF.Sep.B, summed_D12.OF.Sep.C)

NF.Mar.avg <- merge.avg(summed_E1.NF.Mar.A, summed_E2.NF.Mar.B, summed_E3.NF.Mar.C)
NF.May.avg <- merge.avg(summed_E4.NF.May.A, summed_E5.NF.May.B, summed_E6.NF.May.C)
NF.Jul.avg <- merge.avg(summed_E7.NF.Jul.A, summed_E8.NF.Jul.B, summed_E9.NF.Jul.C)
NF.Sep.avg <- merge.avg(summed_E10.NF.Sep.A, summed_E11.NF.Sep.B, summed_E12.NF.Sep.C)

HWD.Mar.avg <- merge.avg(summed_F1.HWD.Mar.A, summed_F2.HWD.Mar.B, summed_F3.HWD.Mar.C)
HWD.May.avg <- merge.avg(summed_F4.HWD.May.A, summed_F5.HWD.May.B, summed_F6.HWD.May.C)
HWD.Jul.avg <- merge.avg(summed_F7.HWD.Jul.A, summed_F8.HWD.Jul.B, summed_F9.HWD.Jul.C)
HWD.Sep.avg <- merge.avg(summed_F10.HWD.Sep.A, summed_F11.HWD.Sep.B, summed_F12.HWD.Sep.C)

LWD.Mar.avg <- merge.avg(summed_G1.LWD.Mar.A, summed_G2.LWD.Mar.B, summed_G3.LWD.Mar.C)
LWD.May.avg <- merge.avg(summed_G4.LWD.May.A, summed_G5.LWD.May.B, summed_G6.LWD.May.C)
LWD.Jul.avg <- merge.avg(summed_G7.LWD.Jul.A, summed_G8.LWD.Jul.B, summed_G9.LWD.Jul.C)
LWD.Sep.avg <- merge.avg(summed_G10.LWD.Sep.A, summed_G11.LWD.Sep.B, summed_G12.LWD.Sep.C)

NCC.Mar.avg <- merge.avg(summed_H1.NCC.Mar.A, summed_H2.NCC.Mar.B, summed_H3.NCC.Mar.C)
NCC.May.avg <- merge.avg(summed_H4.NCC.May.A, summed_H5.NCC.May.B, summed_H6.NCC.May.C)
NCC.Jul.avg <- merge.avg(summed_H7.NCC.Jul.A, summed_H8.NCC.Jul.B, summed_H9.NCC.Jul.C)
NCC.Sep.avg <- merge.avg(summed_H10.NCC.Sep.A, summed_H11.NCC.Sep.B, summed_H12.NCC.Sep.C)



#Combining 8 treatments into 4 season data sets

tempMar.H.NH <- merge(H.Mar.avg, NH.Mar.avg, by = "Order", all = T)[-c(3,5)]
names(tempMar.H.NH)[c(2,3)] <- c("H", "NH")
tempMar.H.NH.SF <- merge(tempMar.H.NH, SF.Mar.avg, by = "Order", all = T)[-5]
names(tempMar.H.NH.SF)[4] <- "SF"
tempMar.H.NH.SF.OF <- merge(tempMar.H.NH.SF, OF.Mar.avg, by = "Order", all = T)[-6]
names(tempMar.H.NH.SF.OF)[5] <- "OF"
tempMar.H.NH.SF.OF.NF <- merge(tempMar.H.NH.SF.OF, NF.Mar.avg, by = "Order", all = T)[-7]
names(tempMar.H.NH.SF.OF.NF)[6] <- "NF"
tempMar.H.NH.SF.OF.NF.HWD <- merge(tempMar.H.NH.SF.OF.NF, HWD.Mar.avg, by = "Order", all = T)[-8]
names(tempMar.H.NH.SF.OF.NF.HWD)[7] <- "HWD"
tempMar.H.NH.SF.OF.NF.HWD.LWD <- merge(tempMar.H.NH.SF.OF.NF.HWD, LWD.Mar.avg, by = "Order", all = T)[-9]
names(tempMar.H.NH.SF.OF.NF.HWD.LWD)[8] <- "LWD"
tempMar.H.NH.SF.OF.NF.HWD.LWD.NCC <- merge(tempMar.H.NH.SF.OF.NF.HWD.LWD, NCC.Mar.avg, by = "Order", all = T)[-10]
names(tempMar.H.NH.SF.OF.NF.HWD.LWD.NCC)[9] <- "NCC"
Mar <- cbind(tempMar.H.NH.SF.OF.NF.HWD.LWD.NCC, "Season" = rep("Budbreak", length(tempMar.H.NH.SF.OF.NF.HWD.LWD.NCC$Order)))
Mar[is.na(Mar)] <- 0

tempMay.H.NH <- merge(H.May.avg, NH.May.avg, by = "Order", all = T)[-c(3,5)]
names(tempMay.H.NH)[c(2,3)] <- c("H", "NH")
tempMay.H.NH.SF <- merge(tempMay.H.NH, SF.May.avg, by = "Order", all = T)[-5]
names(tempMay.H.NH.SF)[4] <- "SF"
tempMay.H.NH.SF.OF <- merge(tempMay.H.NH.SF, OF.May.avg, by = "Order", all = T)[-6]
names(tempMay.H.NH.SF.OF)[5] <- "OF"
tempMay.H.NH.SF.OF.NF <- merge(tempMay.H.NH.SF.OF, NF.May.avg, by = "Order", all = T)[-7]
names(tempMay.H.NH.SF.OF.NF)[6] <- "NF"
tempMay.H.NH.SF.OF.NF.HWD <- merge(tempMay.H.NH.SF.OF.NF, HWD.May.avg, by = "Order", all = T)[-8]
names(tempMay.H.NH.SF.OF.NF.HWD)[7] <- "HWD"
tempMay.H.NH.SF.OF.NF.HWD.LWD <- merge(tempMay.H.NH.SF.OF.NF.HWD, LWD.May.avg, by = "Order", all = T)[-9]
names(tempMay.H.NH.SF.OF.NF.HWD.LWD)[8] <- "LWD"
tempMay.H.NH.SF.OF.NF.HWD.LWD.NCC <- merge(tempMay.H.NH.SF.OF.NF.HWD.LWD, NCC.May.avg, by = "Order", all = T)[-10]
names(tempMay.H.NH.SF.OF.NF.HWD.LWD.NCC)[9] <- "NCC"
May <- cbind(tempMay.H.NH.SF.OF.NF.HWD.LWD.NCC, "Season" = rep("Bloom", length(tempMay.H.NH.SF.OF.NF.HWD.LWD.NCC$Order)))
May[is.na(May)] <- 0

tempJul.H.NH <- merge(H.Jul.avg, NH.Jul.avg, by = "Order", all = T)[-c(3,5)]
names(tempJul.H.NH)[c(2,3)] <- c("H", "NH")
tempJul.H.NH.SF <- merge(tempJul.H.NH, SF.Jul.avg, by = "Order", all = T)[-5]
names(tempJul.H.NH.SF)[4] <- "SF"
tempJul.H.NH.SF.OF <- merge(tempJul.H.NH.SF, OF.Jul.avg, by = "Order", all = T)[-6]
names(tempJul.H.NH.SF.OF)[5] <- "OF"
tempJul.H.NH.SF.OF.NF <- merge(tempJul.H.NH.SF.OF, NF.Jul.avg, by = "Order", all = T)[-7]
names(tempJul.H.NH.SF.OF.NF)[6] <- "NF"
tempJul.H.NH.SF.OF.NF.HWD <- merge(tempJul.H.NH.SF.OF.NF, HWD.Jul.avg, by = "Order", all = T)[-8]
names(tempJul.H.NH.SF.OF.NF.HWD)[7] <- "HWD"
tempJul.H.NH.SF.OF.NF.HWD.LWD <- merge(tempJul.H.NH.SF.OF.NF.HWD, LWD.Jul.avg, by = "Order", all = T)[-9]
names(tempJul.H.NH.SF.OF.NF.HWD.LWD)[8] <- "LWD"
tempJul.H.NH.SF.OF.NF.HWD.LWD.NCC <- merge(tempJul.H.NH.SF.OF.NF.HWD.LWD, NCC.Jul.avg, by = "Order", all = T)[-10]
names(tempJul.H.NH.SF.OF.NF.HWD.LWD.NCC)[9] <- "NCC"
Jul <- cbind(tempJul.H.NH.SF.OF.NF.HWD.LWD.NCC, "Season" = rep("Veraison", length(tempJul.H.NH.SF.OF.NF.HWD.LWD.NCC$Order)))
Jul[is.na(Jul)] <- 0

tempSep.H.NH <- merge(H.Sep.avg, NH.Sep.avg, by = "Order", all = T)[-c(3,5)]
names(tempSep.H.NH)[c(2,3)] <- c("H", "NH")
tempSep.H.NH.SF <- merge(tempSep.H.NH, SF.Sep.avg, by = "Order", all = T)[-5]
names(tempSep.H.NH.SF)[4] <- "SF"
tempSep.H.NH.SF.OF <- merge(tempSep.H.NH.SF, OF.Sep.avg, by = "Order", all = T)[-6]
names(tempSep.H.NH.SF.OF)[5] <- "OF"
tempSep.H.NH.SF.OF.NF <- merge(tempSep.H.NH.SF.OF, NF.Sep.avg, by = "Order", all = T)[-7]
names(tempSep.H.NH.SF.OF.NF)[6] <- "NF"
tempSep.H.NH.SF.OF.NF.HWD <- merge(tempSep.H.NH.SF.OF.NF, HWD.Sep.avg, by = "Order", all = T)[-8]
names(tempSep.H.NH.SF.OF.NF.HWD)[7] <- "HWD"
tempSep.H.NH.SF.OF.NF.HWD.LWD <- merge(tempSep.H.NH.SF.OF.NF.HWD, LWD.Sep.avg, by = "Order", all = T)[-9]
names(tempSep.H.NH.SF.OF.NF.HWD.LWD)[8] <- "LWD"
tempSep.H.NH.SF.OF.NF.HWD.LWD.NCC <- merge(tempSep.H.NH.SF.OF.NF.HWD.LWD, NCC.Sep.avg, by = "Order", all = T)[-10]
names(tempSep.H.NH.SF.OF.NF.HWD.LWD.NCC)[9] <- "NCC"
Sep <- cbind(tempSep.H.NH.SF.OF.NF.HWD.LWD.NCC, "Season" = rep("Harvest", length(tempSep.H.NH.SF.OF.NF.HWD.LWD.NCC$Order)))
Sep[is.na(Sep)] <- 0

Soil2018 <- rbind.data.frame(Mar,May,Jul,Sep)


write.csv(Mar, file = "MarSoil.csv")
write.csv(May, file = "MaySoil.csv")
write.csv(Jul, file = "JulSoil.csv")
write.csv(Sep, file = "SepSoil.csv")
write.csv(Soil2018, file = "Soil2018.csv")



