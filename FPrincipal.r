#############################################
install.packages("randomForest")
install.packages("e1071")  
install.packages("xgboost")
install.packages("neuralnet")
install.packages("nnet")
install.packages("deepNN")
install.packages("data.table")
install.packages("parallel")
install.packages("snow")
install.packages("future.apply")
install.packages("rgdal")
install.packages("terra")
install.packages("knitr")
install.packages("raster")

#############################################
require(randomForest)
require(e1071)  
require(xgboost)
require(neuralnet)
require(nnet)
require(deepNN)
require(data.table)
require(parallel)
require(snow)
require(future.apply)
require(rgdal)
require(terra)
library(knitr)
library(raster)
#############################################
#############################################
######### Input files ##############

OPEN<-'C:/TDF_algorithm_classification/data/' # change file folder
name.shape<-'shape_point.shp'  ## control points
file.img<-'raster_ROI.tif'  # Raster with bands


# land cover categories in name.shape
name.CLASES<<-'cobertura' # change

# Number of cores
n.core<-4

########################
## Location of functions
FuncionesLocal<-"C:/TDF_algorithm_classification/" #change file folder


########################
# save files
SAVE<-'C:/TDF_algorithm_classification/data/'#change file folder


## Function that constructs training and validation data
propVal<-0.30 #proportion of data for validation
source('C:/TDF_algorithm_classification/Preparedata.r') #change file folder with function

DAT.todos<-extract.dat(file.img, name.shape, name.CLASES,OPEN,SAVE, n.core, Normalize = TRUE)

D.All<-d.val(propVal, DAT.todos)

sel.n<-30 #Number of data to be selected per point, default is 1
ndt<-100 #nmodles only randomForest

itrain<-f.train(D.All$d.train, name.CLASES, sel.n, ndt, n.core)
nmodel<-length(itrain)
DAT.test<-D.All$d.Val


##############################################
# run algorithms for classification
#############################################
source(paste0(FuncionesLocal, 'Models.r'))

mRF<-FunctionRF(itrain, name.CLASES, SAVE,DAT.test, OPEN, file.img, FuncionesLocal)

mPoly<-FunctionPoly(itrain, name.CLASES, SAVE,DAT.test, OPEN, file.img, FuncionesLocal)

mRad<-FunctionRad(itrain, name.CLASES, SAVE,DAT.test, OPEN, file.img, FuncionesLocal)

mnet<-Functionnnet(itrain, name.CLASES, SAVE,DAT.test, OPEN, file.img, FuncionesLocal)


###########################################################
######Algorithm assembly ##########################


my.f <- list.files(OPEN, pattern = "\\.tif$", full.names = TRUE)
files.names <- c("Nnet", "RF", "SVM_Radial", "SVM_Poly") # raster of each algorithm
my.files <- my.f[basename(my.f) %in% paste0(files.names, ".tif")]
x <- rast(my.files)

Rassembled <- app(x, fun='modal', cores=6) # # final raster assembled based on mode

writeRaster(Rassembled, "Rassembled.tif", overwrite=TRUE)


##### confusion table and metrics to Rassembled without filter
v<-as.data.frame(vect(paste0(OPEN, name.shape)), geom='xy')
v$pred<-extract(Rassembled, v[,2:3])$modal
v$Obser<-as.numeric(as.factor(v$cobertura))
tconfu1<-f.confusiontablemetrics(table(v$Obser, v$pred))
print(tconfu1)
write.table(tconfu1, file="tconfu_Rassembled.csv", sep=",")


# confusion table at Rassembled filtered with threshold 100 and 8 neighbors

FinalMap <- terra::sieve(Rassembled,100, direction=8) # filter Rassembled all land cover
writeRaster(FinalMap, "FinalMap.tif",  overwrite=TRUE)

vf<-as.data.frame(vect(paste0(OPEN, name.shape)), geom='xy')
vf$pred<-extract(FinalMap, vf[,2:3])$modal
vf$Obser<-as.numeric(as.factor(vf$cobertura))
tconfu2<-f.confusiontablemetrics(table(vf$Obser, vf$pred))
print(tconfu2)
write.table(tconfu2, file="tconfu_FinalMap.csv", sep=",")

# Forest cover selection

TDF10m_map<-FinalMap == 2

