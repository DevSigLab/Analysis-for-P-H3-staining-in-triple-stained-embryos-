## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(xlsx)
library(readxl)
library(tidyverse)
library(writexl)


## ----message=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

embryo<-read_csv("/Volumes/lab-hillc/data/STPs/light_microscopy/outputs/For Todd/values/August_10_2022_3D_Output_HPC_ClusterNuclear_3D_Labels4.csv")

embryo1<-as.data.frame(embryo)

#applying cell size filter
embryo2<-subset(embryo1, subset = AreaShape_Volume < 1350)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#setting channels tresholds using Mean Int values and their variation across cells. This will be used to define cell types


data <- as.data.frame( embryo2[,17:19],drop=false)
mean<-sapply(data,mean)
mean
SD <-sapply(data, sd)
treshold<-mean+(1*SD)
treshold

SDchannels<-as.data.frame.numeric(SD)
C2SD<-SDchannels['Intensity_MeanIntensity_SOX32',]
C3SD<-SDchannels['Intensity_MeanIntensity_PH3',]
C4SD<-SDchannels['Intensity_MeanIntensity_TBX16',]

C2SD
C3SD
C4SD



tresholddf<-as.data.frame.numeric(treshold)
tresholddf
C2<-tresholddf['Intensity_MeanIntensity_SOX32',]
C2treshold<-C2+(0.25*C2SD)
C2treshold
C3<-tresholddf['Intensity_MeanIntensity_PH3',]
C3treshold<-C3 #+(0.2*C3SD)
C3treshold
C4<-tresholddf['Intensity_MeanIntensity_TBX16',]
C4treshold<-C4-C4SD
C4treshold



#To  distinguish sox32 positive cells more precisely I  have created an additional filter taking into account "intra-nuclear" variation in sox32 intensity


C2intSD<-as.data.frame(embryo2[,29],drop=false)
C2intSDmean<-sapply(C2intSD,mean)
C2intSDdev<-sapply(C2intSD,sd)
C2intSDmeandf<-(C2intSDmean+(C2intSDdev*1.4))
C2inttreshold<-C2intSDmeandf

C2inttreshold<-as.data.frame.numeric(C2inttreshold)
C2inttreshold

C2inttreshold2<-C2inttreshold['embryo2[, 29]',]
C2inttreshold2


#counting the number of endodermal cells (sox32+ tbx16+ cells) cells   

data1<-subset(embryo2, subset = Intensity_MeanIntensity_SOX32 > C2treshold  & Intensity_MeanIntensity_TBX16 > C4treshold)
data1<-subset(data1, subset =  Intensity_StdIntensity_SOX32 > C2inttreshold2)
data1t<-transpose(data1)
data1tl<-length(data1t)
endoderm<-as.data.frame.numeric(data1tl)
endoderm

#counting the number of ph3+ cells into the endoderm (sox32+ tbx16+ cells) cell pool

data2<- subset(embryo2, subset = Intensity_MeanIntensity_SOX32 >  C2treshold  & Intensity_MeanIntensity_PH3 > C3treshold & Intensity_MeanIntensity_TBX16 > C4treshold )
data2<-subset(data2, subset =  Intensity_StdIntensity_SOX32 > C2inttreshold2)
data2t<-transpose(data2)
data2tl<-length(data2t)
ph3endoderm<-as.data.frame.numeric(data2tl)

ph3endoderm

# counting the number of  mesodermal cells (sox32- tbx16+)

data3<- subset(embryo2, subset = Intensity_MeanIntensity_SOX32 < C2treshold  &  Intensity_MeanIntensity_TBX16 > C4treshold)

data3<-subset(data3, subset =  Intensity_StdIntensity_SOX32 < C2inttreshold2)
data3t<-transpose(data3)
data3tl<-length(data3t)
mesoderm<-as.data.frame.numeric(data3tl)
mesoderm
#counting the number of ph3+ cells into the mesodermal (sox32- tbx16+ cells) cell pool

data4<-subset(embryo2, subset = Intensity_MeanIntensity_SOX32 < C2treshold  & Intensity_MeanIntensity_PH3 > C3treshold & Intensity_MeanIntensity_TBX16 > C4treshold)

data4<-subset(data4, subset =  Intensity_StdIntensity_SOX32 < C2inttreshold2)
data4t<-transpose(data4)
data4tl<-length(data4t)
ph3mesoderm<-as.data.frame.numeric(data4tl)
ph3mesoderm



# counting the nubmer of number of non endodermal cell pool (sox32-cells) 

data5<-subset(embryo2, subset = Intensity_MeanIntensity_SOX32 < C2treshold)

data5<-subset(data5, subset =  Intensity_StdIntensity_SOX32 < C2inttreshold2)
data5t<-transpose(data5)
data5tl<-length(data5t)
allnonendodermal<-as.data.frame.numeric(data5tl)
allnonendodermal

# counting the number of ph3+ cells into the non endodermal cell pool (sox32-cells) 

data6<-subset(embryo2, subset = Intensity_MeanIntensity_SOX32 < C2treshold &  Intensity_MeanIntensity_PH3 > C3treshold)

data6<-subset(data6, subset =  Intensity_StdIntensity_SOX32 < C2inttreshold2)
data6t<-transpose(data6)
data6tl<-length(data6t)
nonendodermalph3<-as.data.frame.numeric(data6tl)
nonendodermalph3


Ph3ratioendo<-(ph3endoderm/endoderm)*100
Ph3rationoendo<-(nonendodermalph3/allnonendodermal)*100
Ph3ratiomeso<-(ph3mesoderm/mesoderm)*100

Ph3ratioendo
Ph3rationoendo
Ph3ratiomeso

toplot<-merge(Ph3ratioendo,Ph3rationoendo)
toplotmeso<-merge(toplot,Ph3ratiomeso)
toplotmeso

write_xlsx(toplotmeso,"/Volumes/lab-hillc/data/STPs/light_microscopy/outputs/For Todd/values/Embryo4.xlsx")




