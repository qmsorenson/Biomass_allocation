



### Add touchup Areas to the main file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AllArea <- read.csv("data/BiomassAllocatoin_AllLeafArea.csv")
redoneArea <- read.csv("data/BiomassAllocation_RedoneLeafArea.csv")
library(Hmisc)
redoneArea$Slice %nin% AllArea$Slice


for (i in 1:nrow(AllArea)){
  if (AllArea[i, "Slice"] %in% redoneArea[,"Slice"]){
    AllArea[i,"Total.Area"] <- redoneArea[redoneArea$Slice == as.character(AllArea[i, "Slice"]), "Total.Area"]
  } else {
    AllArea[i,"Total.Area"] <- AllArea[i,"Total.Area"]
  }
}
write.csv(AllArea, "data/BiomassAllocation_LeafArea.csv")
