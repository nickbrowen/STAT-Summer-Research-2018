# get sheet names
library(readxl)
pathname <- "C:/Users/nickbrowen/Desktop/STAT Summer Research/Copy of Bacteria 2017.xlsm"
sheet_names <- excel_sheets(pathname)
sheet_names2 <- noquote(excel_sheets(pathname))
#names(sheet_names2) <- sheet_names2


for (i in 1:length(sheet_names)){
  assign(paste(toString(sheet_names2[i])), read_excel(pathname, sheet = sheet_names[i], skip = 4)[-1,])
}




#for (i in 1:length(sheet_names)){
#  for (j in 1:length(sheet_names2)){
#    sheet_names2[j] <- read_excel(pathname, sheet = sheet_names[i], skip = 4)
#  }
   #sheet_names3[i] <-  sheet_names2[i][-1,]
#}

#for (i in 1:2){
#  for (j in 1:2){
#    test[[j]] <- read_excel(pathname, sheet = sheet_names[i], skip = 4)
#  }
#}



