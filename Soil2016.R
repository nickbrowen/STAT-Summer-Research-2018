#Importing and structuring 2016 Data

CoverCrop <- read.csv("C:/Users/nickbrowen/Desktop/STAT Summer Research/CoverCrop-DNA2016.csv")
Fertilizer <- read.csv("C:/Users/nickbrowen/Desktop/STAT Summer Research/Fertilizer-DNA2016.csv")
Herbicide <- read.csv("C:/Users/nickbrowen/Desktop/STAT Summer Research/Herbicide-DNA2016.csv")


#March

H.Mar <- subset(Herbicide, Herbicide$Treatment == "Herbicide" & Herbicide$Season == "Bud Break",
                select = c(Amount, Order,Treatment,Sample, Season))
H.Mar.sum_matrix <- rowsum(H.Mar$Amount, H.Mar$Order)
H.Mar.sum <- data.frame(Order = rownames(H.Mar.sum_matrix), Amount = H.Mar.sum_matrix, row.names = NULL)


NH.Mar <- subset(Herbicide, Herbicide$Treatment == "No Herbicide" & Herbicide$Season == "Bud Break",
                  select = c(Amount, Order,Treatment,Sample, Season))
NH.Mar.sum_matrix <- rowsum(NH.Mar$Amount, NH.Mar$Order)
NH.Mar.sum <- data.frame(Order = rownames(NH.Mar.sum_matrix), Amount = NH.Mar.sum_matrix, row.names = NULL)


HWD.Mar <- subset(CoverCrop, CoverCrop$Treatment == "High Water" & CoverCrop$Season == "Bud Break",
                select = c(Amount, Order,Treatment,Sample, Season))
HWD.Mar.sum_matrix <- rowsum(HWD.Mar$Amount, HWD.Mar$Order)
HWD.Mar.sum <- data.frame(Order = rownames(HWD.Mar.sum_matrix), Amount = HWD.Mar.sum_matrix, row.names = NULL)

LWD.Mar <- subset(CoverCrop, CoverCrop$Treatment == "Low Water" & CoverCrop$Season == "Bud Break",
                  select = c(Amount, Order,Treatment,Sample, Season))
LWD.Mar.sum_matrix <- rowsum(LWD.Mar$Amount, LWD.Mar$Order)
LWD.Mar.sum <- data.frame(Order = rownames(LWD.Mar.sum_matrix), Amount = LWD.Mar.sum_matrix, row.names = NULL)


NCC.Mar <- subset(CoverCrop, CoverCrop$Treatment == "No Cover Crop" & CoverCrop$Season == "Bud Break",
                  select = c(Amount, Order,Treatment,Sample, Season))
NCC.Mar.sum_matrix <- rowsum(NCC.Mar$Amount, NCC.Mar$Order)
NCC.Mar.sum <- data.frame(Order = rownames(NCC.Mar.sum_matrix), Amount = NCC.Mar.sum_matrix, row.names = NULL)


NF.Mar <- subset(Fertilizer, Fertilizer$Treatment == "No Fertilizer" & Fertilizer$Season == "Bud Break",
                  select = c(Amount, Order,Treatment,Sample, Season))
NF.Mar.sum_matrix <- rowsum(NF.Mar$Amount, NF.Mar$Order)
NF.Mar.sum <- data.frame(Order = rownames(NF.Mar.sum_matrix), Amount = NF.Mar.sum_matrix, row.names = NULL)


OF.Mar <- subset(Fertilizer, Fertilizer$Treatment == "Organic Fertilizer" & Fertilizer$Season == "Bud Break",
                 select = c(Amount, Order,Treatment,Sample, Season))
OF.Mar.sum_matrix <- rowsum(OF.Mar$Amount, OF.Mar$Order)
OF.Mar.sum <- data.frame(Order = rownames(OF.Mar.sum_matrix), Amount = OF.Mar.sum_matrix, row.names = NULL)


SF.Mar <- subset(Fertilizer, Fertilizer$Treatment == "Synthetic Fertilizer" & Fertilizer$Season == "Bud Break",
                 select = c(Amount, Order,Treatment,Sample, Season))
SF.Mar.sum_matrix <- rowsum(SF.Mar$Amount, SF.Mar$Order)
SF.Mar.sum <- data.frame(Order = rownames(SF.Mar.sum_matrix), Amount = SF.Mar.sum_matrix, row.names = NULL)



tempMarsum.H.NH <- merge(H.Mar.sum, NH.Mar.sum, by = "Order", all = T)
names(tempMarsum.H.NH)[c(2,3)] <- c("H", "NH")
tempMarsum.H.NH.SF <- merge(tempMarsum.H.NH, SF.Mar.sum, by = "Order", all = T)
names(tempMarsum.H.NH.SF)[4] <- "SF"
tempMarsum.H.NH.SF.OF <- merge(tempMarsum.H.NH.SF, OF.Mar.sum, by = "Order", all = T)
names(tempMarsum.H.NH.SF.OF)[5] <- "OF"
tempMarsum.H.NH.SF.OF.NF <- merge(tempMarsum.H.NH.SF.OF, NF.Mar.sum, by = "Order", all = T)
names(tempMarsum.H.NH.SF.OF.NF)[6] <- "NF"
tempMarsum.H.NH.SF.OF.NF.HWD <- merge(tempMarsum.H.NH.SF.OF.NF, HWD.Mar.sum, by = "Order", all = T)
names(tempMarsum.H.NH.SF.OF.NF.HWD)[7] <- "HWD"
tempMarsum.H.NH.SF.OF.NF.HWD.LWD <- merge(tempMarsum.H.NH.SF.OF.NF.HWD, LWD.Mar.sum, by = "Order", all = T)
names(tempMarsum.H.NH.SF.OF.NF.HWD.LWD)[8] <- "LWD"
tempMarsum.H.NH.SF.OF.NF.HWD.LWD.NCC <- merge(tempMarsum.H.NH.SF.OF.NF.HWD.LWD, NCC.Mar.sum, by = "Order", all = T)
names(tempMarsum.H.NH.SF.OF.NF.HWD.LWD.NCC)[9] <- "NCC"
Mar2016_sum <- cbind(tempMarsum.H.NH.SF.OF.NF.HWD.LWD.NCC, "Season" = rep("Budbreak", length(tempMarsum.H.NH.SF.OF.NF.HWD.LWD.NCC$Order)))
Mar2016_sum[is.na(Mar2016_sum)] <- 0


#PCA of March 2016

library(FactoMineR)
library(factoextra)

#March Summed PCA
Mar2016_sumpca <- data.frame(Mar2016_sum[c(-1,-10)], row.names = Mar2016_sum$Order)
Mar2016_results <- PCA(Mar2016_sumpca, graph = F, scale.unit = F)
Mar2016results.desc <- dimdesc(Mar2016_results, axes = c(1,2), proba = 1)
#Description of dimension 1
Mar2016results.desc$Dim.1

Marres.pca <- prcomp(Mar2016_sumpca, scale = F)
Marres.pca$rotation #eigenvectors






#May
H.May <- subset(Herbicide, Herbicide$Treatment == "Herbicide" & Herbicide$Season == "Bloom",
                select = c(Amount, Order,Treatment,Sample, Season))
H.May.sum_matrix <- rowsum(H.May$Amount, H.May$Order)
H.May.sum <- data.frame(Order = rownames(H.May.sum_matrix), Amount = H.May.sum_matrix, row.names = NULL)


NH.May <- subset(Herbicide, Herbicide$Treatment == "No Herbicide" & Herbicide$Season == "Bloom",
                 select = c(Amount, Order,Treatment,Sample, Season))
NH.May.sum_matrix <- rowsum(NH.May$Amount, NH.May$Order)
NH.May.sum <- data.frame(Order = rownames(NH.May.sum_matrix), Amount = NH.May.sum_matrix, row.names = NULL)


HWD.May <- subset(CoverCrop, CoverCrop$Treatment == "High Water" & CoverCrop$Season == "Bloom",
                  select = c(Amount, Order,Treatment,Sample, Season))
HWD.May.sum_matrix <- rowsum(HWD.May$Amount, HWD.May$Order)
HWD.May.sum <- data.frame(Order = rownames(HWD.May.sum_matrix), Amount = HWD.May.sum_matrix, row.names = NULL)

LWD.May <- subset(CoverCrop, CoverCrop$Treatment == "Low Water" & CoverCrop$Season == "Bloom",
                  select = c(Amount, Order,Treatment,Sample, Season))
LWD.May.sum_matrix <- rowsum(LWD.May$Amount, LWD.May$Order)
LWD.May.sum <- data.frame(Order = rownames(LWD.May.sum_matrix), Amount = LWD.May.sum_matrix, row.names = NULL)


NCC.May <- subset(CoverCrop, CoverCrop$Treatment == "No Cover Crop" & CoverCrop$Season == "Bloom",
                  select = c(Amount, Order,Treatment,Sample, Season))
NCC.May.sum_matrix <- rowsum(NCC.May$Amount, NCC.May$Order)
NCC.May.sum <- data.frame(Order = rownames(NCC.May.sum_matrix), Amount = NCC.May.sum_matrix, row.names = NULL)


NF.May <- subset(Fertilizer, Fertilizer$Treatment == "No Fertilizer" & Fertilizer$Season == "Bloom",
                 select = c(Amount, Order,Treatment,Sample, Season))
NF.May.sum_matrix <- rowsum(NF.May$Amount, NF.May$Order)
NF.May.sum <- data.frame(Order = rownames(NF.May.sum_matrix), Amount = NF.May.sum_matrix, row.names = NULL)


OF.May <- subset(Fertilizer, Fertilizer$Treatment == "Organic Fertilizer" & Fertilizer$Season == "Bloom",
                 select = c(Amount, Order,Treatment,Sample, Season))
OF.May.sum_matrix <- rowsum(OF.May$Amount, OF.May$Order)
OF.May.sum <- data.frame(Order = rownames(OF.May.sum_matrix), Amount = OF.May.sum_matrix, row.names = NULL)


SF.May <- subset(Fertilizer, Fertilizer$Treatment == "Synthetic Fertilizer" & Fertilizer$Season == "Bloom",
                 select = c(Amount, Order,Treatment,Sample, Season))
SF.May.sum_matrix <- rowsum(SF.May$Amount, SF.May$Order)
SF.May.sum <- data.frame(Order = rownames(SF.May.sum_matrix), Amount = SF.May.sum_matrix, row.names = NULL)



tempMaysum.H.NH <- merge(H.May.sum, NH.May.sum, by = "Order", all = T)
names(tempMaysum.H.NH)[c(2,3)] <- c("H", "NH")
tempMaysum.H.NH.SF <- merge(tempMaysum.H.NH, SF.May.sum, by = "Order", all = T)
names(tempMaysum.H.NH.SF)[4] <- "SF"
tempMaysum.H.NH.SF.OF <- merge(tempMaysum.H.NH.SF, OF.May.sum, by = "Order", all = T)
names(tempMaysum.H.NH.SF.OF)[5] <- "OF"
tempMaysum.H.NH.SF.OF.NF <- merge(tempMaysum.H.NH.SF.OF, NF.May.sum, by = "Order", all = T)
names(tempMaysum.H.NH.SF.OF.NF)[6] <- "NF"
tempMaysum.H.NH.SF.OF.NF.HWD <- merge(tempMaysum.H.NH.SF.OF.NF, HWD.May.sum, by = "Order", all = T)
names(tempMaysum.H.NH.SF.OF.NF.HWD)[7] <- "HWD"
tempMaysum.H.NH.SF.OF.NF.HWD.LWD <- merge(tempMaysum.H.NH.SF.OF.NF.HWD, LWD.May.sum, by = "Order", all = T)
names(tempMaysum.H.NH.SF.OF.NF.HWD.LWD)[8] <- "LWD"
tempMaysum.H.NH.SF.OF.NF.HWD.LWD.NCC <- merge(tempMaysum.H.NH.SF.OF.NF.HWD.LWD, NCC.May.sum, by = "Order", all = T)
names(tempMaysum.H.NH.SF.OF.NF.HWD.LWD.NCC)[9] <- "NCC"
May2016_sum <- cbind(tempMaysum.H.NH.SF.OF.NF.HWD.LWD.NCC, "Season" = rep("Budbreak", length(tempMaysum.H.NH.SF.OF.NF.HWD.LWD.NCC$Order)))
May2016_sum[is.na(May2016_sum)] <- 0


#May PCA
May2016_sumpca <- data.frame(May2016_sum[c(-1,-10)], row.names = May2016_sum$Order)
May2016_results <- PCA(May2016_sumpca, graph = F, scale.unit = F)
May2016results.desc <- dimdesc(May2016_results, axes = c(1,2), proba = 1)
#Description of dimension 1
May2016results.desc$Dim.1

Mayres.pca <- prcomp(May2016_sumpca, scale = F)
Mayres.pca$rotation #eigenvectors
