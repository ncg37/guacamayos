library (pavo)
library(dplyr)
library(tidyr)
library(tibble)
library(RColorBrewer) 

palette<-brewer.pal(7, "Dark2")

glauco_90_90 <- getspec ("./glauco_90_90", ext = "txt", decimal = ".") %>% 
  procspec(opt = 'smooth', span = 0.2, fixneg = 'zero') %>% #applies a smoothing function and sets all negative values to zero.
  aggspec(by = 3, FUN = mean) #reads .csv files with reflectance data, 3 files per patch, 7 patches per individual
glauco_45_25 <- getspec ("./glauco_45_25", ext = "txt", decimal = ".") %>% 
  procspec(opt = 'smooth', span = 0.2, fixneg = 'zero') %>% #applies asmoothing function and sets all negative values to zero.
  aggspec(by = 3, FUN = mean)
glauco_45_90 <- getspec ("./glauco_45_90", ext = "txt", decimal = ".") %>% 
  procspec(opt = 'smooth', span = 0.2, fixneg = 'zero') %>% #applies asmoothing function and sets all negative values to zero.
  aggspec(by = 3, FUN = mean) 

sum_glauco<-rbind(summary(glauco_90_90), summary(glauco_45_25), summary(glauco_45_90))

bypatch<-(cbind(glauco_90_90[,1:2], glauco_45_90[,2], glauco_45_25[,2],
             glauco_90_90[,3], glauco_45_90[,3], glauco_45_25[,3],
             glauco_90_90[,4], glauco_45_90[,4], glauco_45_25[,4],
             glauco_90_90[,5], glauco_45_90[,5], glauco_45_25[,5],
             glauco_90_90[,6], glauco_45_90[,6], glauco_45_25[,6]))

colnames(bypatch)<- c("wl", "back9090","back4590", "back4525",
                      "belly9090","belly4590", "belly4525",
                      "chest9090","chest4590", "chest4525",
                      "head9090","head4590", "head4525",
                      "rump9090","rump4590", "rump4525")
           
explorespec (bypatch, by=3, legpos="topleft", col=palette, scale="free")

palm_dorsal_90_90<- getspec ("./palm_dorsal_90_90", ext = "txt", decimal = ",") %>% 
  procspec(opt = 'smooth', span = 0.2, fixneg = 'zero') %>% #applies asmoothing function and sets all negative values to zero.
  aggspec(by = 3, FUN = mean) #reads .csv files with reflectance data, 3 files per patch, 7 patches per individual
palm_dorsal_45_90<- getspec ("./palm_dorsal_45_90", ext = "txt", decimal = ",") %>% 
  procspec(opt = 'smooth', span = 0.2, fixneg = 'zero') %>% #applies asmoothing function and sets all negative values to zero.
  aggspec(by = 3, FUN = mean) 
palm_dorsal_45_25<- getspec ("./palm_dorsal_45_25", ext = "txt", decimal = ",") %>% 
  procspec(opt = 'smooth', span = 0.2, fixneg = 'zero') %>% #applies asmoothing function and sets all negative values to zero.
  aggspec(by = 3, FUN = mean) 

sum_palm_dorsal<-rbind(summary(palm_dorsal_90_90), summary(palm_dorsal_45_90), summary(palm_dorsal_45_25))

palms_dorsal <-cbind(palm_dorsal_90_90, palm_dorsal_45_90[,2:5], palm_dorsal_45_25[,2:5])

colnames(palms_dorsal)<- c("wl", "dorsal9090","dorsal9090", "dorsal9090",
                      "dorsal9090","dorsal4590", "dorsal4590",
                      "dorsal4590","dorsal4590", "dorsal4525",
                      "dorsal4525","dorsal4525", "dorsal4525")

aggplot(palms_dorsal, by= 4, legend =TRUE, legpos="topleft", 
        FUN.error = function(x) sd(x) / sqrt(length(x)),
         col=palette, scale="free")

palm_ventral_90_90<- getspec ("./palm_ventral_90_90", ext = "txt", decimal = ",") %>% 
  procspec(opt = 'smooth', span = 0.2, fixneg = 'zero') %>% #applies asmoothing function and sets all negative values to zero.
  aggspec(by = 3, FUN = mean) #reads .csv files with reflectance data, 3 files per patch, 7 patches per individual
palm_ventral_45_90<- getspec ("./palm_ventral_45_90", ext = "txt", decimal = ",") %>% 
  procspec(opt = 'smooth', span = 0.2, fixneg = 'zero') %>% #applies asmoothing function and sets all negative values to zero.
  aggspec(by = 3, FUN = mean) 
palm_ventral_45_25<- getspec ("./palm_ventral_45_25", ext = "txt", decimal = ",") %>% 
  procspec(opt = 'smooth', span = 0.2, fixneg = 'zero') %>% #applies asmoothing function and sets all negative values to zero.
  aggspec(by = 3, FUN = mean) 

sum_palm_ventral<-rbind(summary(palm_ventral_90_90), summary(palm_ventral_45_90), summary(palm_ventral_45_25))

palms_ventral <-cbind(palm_ventral_90_90, palm_ventral_45_90[,2:5], palm_ventral_45_25[,2:5])

colnames(palms_ventral)<- c("wl", "ventral9090","ventral9090", "ventral9090",
                    "ventral9090","ventral4590", "ventral4590",
                    "ventral4590","ventral4590", "ventral4525",
                    "ventral4525","ventral4525", "ventral4525")

aggplot(palms_ventral, by= 4, legend =TRUE, legpos="topleft", 
        FUN.error = function(x) sd(x) / sqrt(length(x)),
        col=palette, scale="free")


#Visual Models----

glauco_90_90_vismodel<- vismodel(glauco_90_90, visual="avg.uv", achromatic="bt.dc", 
                          relative=FALSE, bkg="ideal")
glauco_45_90_vismodel<- vismodel(glauco_45_90, visual="avg.uv", achromatic="bt.dc", 
                                 relative=FALSE, bkg="ideal")
glauco_45_25_vismodel<- vismodel(glauco_45_25, visual="avg.uv", achromatic="bt.dc", 
                                 relative=FALSE, bkg="ideal")

palm_dorsal_90_90_vismodel<- vismodel(palm_dorsal_90_90, visual="avg.uv", achromatic="bt.dc", 
                                 relative=FALSE, bkg="ideal")
palm_dorsal_45_90_vismodel<- vismodel(palm_dorsal_45_90, visual="avg.uv", achromatic="bt.dc", 
                                 relative=FALSE, bkg="ideal")
palm_dorsal_45_25_vismodel<- vismodel(palm_dorsal_45_25, visual="avg.uv", achromatic="bt.dc", 
                                 relative=FALSE, bkg="ideal")

palm_ventral_90_90_vismodel<- vismodel(palm_ventral_90_90, visual="avg.uv", achromatic="bt.dc", 
                                      relative=FALSE, bkg="ideal")
palm_ventral_45_90_vismodel<- vismodel(palm_ventral_45_90, visual="avg.uv", achromatic="bt.dc", 
                                      relative=FALSE, bkg="ideal")
palm_ventral_45_25_vismodel<- vismodel(palm_ventral_45_25, visual="avg.uv", achromatic="bt.dc", 
                                      relative=FALSE, bkg="ideal")

#Glauco-palm----
#deltaS DORSAL

gl_d_90_90 <- rbind(glauco_90_90_vismodel, palm_dorsal_90_90_vismodel)

gr <- gsub("^([^_]*_){2}", "", rownames(gl_d_90_90)) #create a vector to subset by 
gr

gl_d_90_90_dist <- as.data.frame (bootcoldist(gl_d_90_90, gr, n=c(1,1.9,2.9,2.5), weber=0.105, 
                                      achromatic = FALSE))

gl_d_90_90_dist=gl_d_90_90_dist[c(3,7,10,13,14),]


gl_d_45_90 <- rbind(glauco_45_90_vismodel, palm_dorsal_45_90_vismodel)

gr <- gsub("^([^_]*_){2}", "", rownames(gl_d_45_90)) #create a vector to subset by 
gr

gl_d_45_90_dist <- as.data.frame (bootcoldist(gl_d_45_90, gr, n=c(1,1.9,2.9,2.5), weber=0.105, 
                                              achromatic = FALSE))

gl_d_45_90_dist=gl_d_45_90_dist[c(5,9,12,14,15),]


gl_d_45_25 <- rbind(glauco_45_25_vismodel, palm_dorsal_45_25_vismodel)

gr <- gsub("^([^_]*_){2}", "", rownames(gl_d_45_25)) #create a vector to subset by 
gr

gl_d_45_25_dist <- as.data.frame (bootcoldist(gl_d_45_25, gr, n=c(1,1.9,2.9,2.5), weber=0.105, 
                                              achromatic = FALSE))

gl_d_45_25_dist=gl_d_45_25_dist[c(5,9,12,14,15),]


#deltaS VENTRAL---


gl_v_90_90 <- rbind(glauco_90_90_vismodel, palm_ventral_90_90_vismodel)

gr <- gsub("^([^_]*_){2}", "", rownames(gl_v_90_90)) #create a vector to subset by 
gr

gl_v_90_90_dist <- as.data.frame (bootcoldist(gl_v_90_90, gr, n=c(1,1.9,2.9,2.5), weber=0.105, 
                                              achromatic = FALSE))

gl_v_90_90_dist=gl_v_90_90_dist[c(3,7,10,13,14),]


gl_v_45_90 <- rbind(glauco_45_90_vismodel, palm_ventral_45_90_vismodel)

gr <- gsub("^([^_]*_){2}", "", rownames(gl_v_45_90)) #create a vector to subset by 
gr

gl_v_45_90_dist <- as.data.frame (bootcoldist(gl_v_45_90, gr, n=c(1,1.9,2.9,2.5), weber=0.105, 
                                              achromatic = FALSE))

gl_v_45_90_dist=gl_v_45_90_dist[c(5,9,12,14,15),]


gl_v_45_25 <- rbind(glauco_45_25_vismodel, palm_ventral_45_25_vismodel)

gr <- gsub("^([^_]*_){2}", "", rownames(gl_v_45_25)) #create a vector to subset by 
gr

gl_v_45_25_dist <- as.data.frame (bootcoldist(gl_v_45_25, gr, n=c(1,1.9,2.9,2.5), weber=0.105, 
                                              achromatic = FALSE))

gl_v_45_25_dist=gl_v_45_25_dist[c(5,9,12,14,15),]


write.csv(rbind(sum_glauco, sum_palm_dorsal, sum_palm_ventral), "summary_data.csv")
write.csv(rbind(gl_d_90_90_dist, gl_d_45_90_dist, gl_d_45_25_dist),"deltaS_dorsal.csv")
write.csv(rbind(gl_v_90_90_dist, gl_v_45_90_dist, gl_v_45_25_dist),"deltaS_ventral.csv")
          


#Glauco-glauco----

glauco_vismodel<- vismodel(bypatch, visual="avg.uv", achromatic="bt.dc", 
                                 relative=FALSE, bkg="ideal")

glauco_back<-subset(glauco_vismodel, subset="back")
glauco_belly<-subset(glauco_vismodel, subset="belly")
glauco_chest<-subset(glauco_vismodel, subset="chest")
glauco_head<-subset(glauco_vismodel, subset="head")
glauco_rump<-subset(glauco_vismodel, subset="rump")

back_dist <- as.data.frame (coldist(glauco_back, n=c(1,1.9,2.9,2.5), weber=0.105, 
                                              achromatic = FALSE))
belly_dist <- as.data.frame (coldist(glauco_belly, n=c(1,1.9,2.9,2.5), weber=0.105, 
                                    achromatic = FALSE))
chest_dist <- as.data.frame (coldist(glauco_chest, n=c(1,1.9,2.9,2.5), weber=0.105, 
                                    achromatic = FALSE))
head_dist <- as.data.frame (coldist(glauco_head, n=c(1,1.9,2.9,2.5), weber=0.105, 
                                    achromatic = FALSE))
rump_dist <- as.data.frame (coldist(glauco_rump, n=c(1,1.9,2.9,2.5), weber=0.105, 
                                    achromatic = FALSE))

write.csv(rbind(back_dist, belly_dist, chest_dist, head_dist, rump_dist), "glauco_dist.csv")


#palm-palm----

palms_dorsal_vismodel<- vismodel(palms_dorsal, visual="avg.uv", achromatic="bt.dc", 
                                       relative=FALSE, bkg="ideal")

gr <- gsub("dorsal", "", rownames(palms_dorsal_vismodel))
gr1 <- gsub("\\.[0-9]", "", gr)


palm_dorsal_dist<- as.data.frame (bootcoldist(palms_dorsal_vismodel, gr1, n=c(1,1.9,2.9,2.5), weber=0.105, 
                                              achromatic = FALSE))
