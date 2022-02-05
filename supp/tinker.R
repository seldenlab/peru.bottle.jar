# load packages
library(here)
library(geomorph)
library(tidyverse)
library(wesanderson)

# read GM data
source('readmulti.csv.R')
setwd("./data")
filelist <- list.files(pattern = ".csv")
coords <- readmulti.csv(filelist)
setwd("../")

# read qualitative data
qdata <- read.csv("qdata.csv", 
                  header = TRUE, 
                  row.names = 1)

qdata <- qdata[match(dimnames(coords)[[3]],
                     rownames(qdata)),]

#select landmarks associated with body
b <- c(10:19, 27:36)

# new coords body
body.coords <- coords[b,,]

# gpa
Y.gpa <- gpagen(body.coords, 
                PrinAxes = TRUE, 
                ProcD = TRUE, 
                Proj = TRUE, 
                print.progress = FALSE)

# geomorph data frame
gdf <- geomorph.data.frame(shape = Y.gpa$coords, 
                           size = Y.gpa$Csize,
                           type = qdata$type)

# add centroid size to qdata
qdata$csz <- Y.gpa$Csize

# principal components analysis
pca<-gm.prcomp(Y.gpa$coords)
summary(pca)

# set plot parameters (types)
pch.gps.type <- c(15,17)[as.factor(qdata$type)]
col.gps.type <- wes_palette("Moonrise2")[as.factor(qdata$type)]
col.hull.type <- c("#798E87","#C27D38")

# plot pca by comb
pc.plot.type <- plot(pca, 
                     asp = 1,
                     pch = pch.gps.type,
                     col = col.gps.type)
shapeHulls(pc.plot.type, 
           groups = qdata$type,
           group.cols = col.hull.type)

ref <- mean.shape <- mshape(Y.gpa$coords)
plotRefToTarget(M1 = ref, 
                M2 = pca$shapes$shapes.comp1$min, 
                method = "TPS")

plotRefToTarget(M1 = ref, 
                M2 = pca$shapes$shapes.comp1$max, 
                method = "TPS")

plotRefToTarget(M1 = ref, 
                M2 = pca$shapes$shapes.comp2$min, 
                method = "TPS")

plotRefToTarget(M1 = ref, 
                M2 = pca$shapes$shapes.comp2$max,
                method = "TPS")

# subset landmark coordinates to produce mean shapes
new.coords<-coords.subset(A = Y.gpa$coords,
                          group = qdata$type)
names(new.coords)

# type shape means
mean <- lapply(new.coords, mshape)

# plot mean shapes (type)
plot(mean$bottle)
plot(mean$jar)

# comparison plots
plotRefToTarget(mean$bottle,
                mean$jar, 
                method = c("point"),
                mag = 2)
