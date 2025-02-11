
library(randomForest)
library(e1071)  
library(nnet)
library(data.table)
library(parallel)
library(snow)
library(future.apply)
library(rgdal)
library(terra)

#######################################################################
#######################################################################
#				____________
# 				randomForest 
#				------------
#######################################################################
#######################################################################
FunctionRF<-cmpfun(function(...){
source(paste0(FuncionesLocal, 'CombineRF.r'))
source(paste0(FuncionesLocal, 'ConfuTable.r'))
#################
n.class<<-nlevels(itrain[[1]][,get(eval(name.CLASES))])
m<-as.formula(paste(paste(name.CLASES[1], '~'), paste(names(itrain[[1]])[-c(1:2)], collapse='+')))
m.rf<-cmpfun(function(x) randomForest(m, x,ntree=100))


f.bestmodel<-cmpfun(function(Data, modls,Var.class,...) {
errrrr<-unlist(lapply(modls, function(m)   last(f.confusiontablemetrics(f.confusionAll(Data, m, Var.class))) [,'F1 score']))
Reduce(my_combine, modls[rev(order(errrrr))[1:10]])
})


###############

plan(multisession, workers = n.core) 
options(future.rng.onMisuse="ignore")
modls<-future_lapply(itrain,FUN=m.rf)

Data<-DAT.test
Var.class<-Data[,get(eval(name.CLASES))]
mRf<-f.bestmodel(DAT.test,modls, Var.class)
tconfu<-f.confusiontablemetrics(f.confusionAll(Data, mRf, Var.class))

########################################################################
RAST <- Rastnorm01(rast(paste0(OPEN, file.img)))
CLASI<-predict(RAST, mRf, cores=n.core, cpkgs="randomForest")
writeRaster(CLASI, filename=paste0(SAVE, 'RF', '.tif'), overwrite=TRUE)

print(knitr::kable(tconfu), caption='test/predict')

fwrite(tconfu, file=paste0(SAVE,'MetricsPerformsRF.csv'), sep=',')
save(mRf, file=paste0(SAVE,'trainModRF.RData'))
print(paste0('Model is save in:', SAVE))
CLASI
})

#######################################################################
#######################################################################
#				_______________
# 				SVM  polynomial
#				---------------
#######################################################################
#######################################################################
FunctionPoly<-cmpfun(function(...){
source(paste0(FuncionesLocal, 'ConfuTable.r'))
#################
m<-as.formula(paste(paste(name.CLASES[1], '~'), paste(names(itrain[[1]])[-c(1:2)], collapse='+')))
m.svm.Poly<-cmpfun(function(x) e1071::svm(m, x, scale=FALSE, type='C-classification',tolerance=0.1, kernel="polynomial", degree=3, coef0=0, cost = 10))


predSVMfun <- cmpfun(function(m, d, ...) predict(m, newdata=d, ...))

f.bestmodel<-cmpfun(function(Data, modls,Var.class,...) {
errrrr<-unlist(lapply(modls, function(m)   last(f.confusiontablemetrics(f.confusionAll(Data, m, Var.class))) [,'F1 score']))
modls[which.max(errrrr)][[1]]
})
###############

plan(multisession, workers = n.core) 
options(future.rng.onMisuse="ignore")
modls<-future_lapply(itrain,FUN=m.svm.Poly)

Data<-DAT.test
Var.class<-Data[,get(eval(name.CLASES))]
msvmPoly<-f.bestmodel(DAT.test,modls, Var.class)
tconfu<-f.confusiontablemetrics(f.confusionAll(Data, msvmPoly, Var.class))

########################################################################
RAST <- Rastnorm01(rast(paste0(OPEN, file.img)))
CLASI<-predict(RAST, msvmPoly, fun=predSVMfun, na.rm=TRUE, cores = n.core, cpkgs = "e1071") 
		writeRaster(CLASI, filename=paste0(SAVE, 'SVM_Poly','.tif'), overwrite=TRUE)

print(knitr::kable(tconfu), caption='test/predict')

fwrite(tconfu, file=paste0(SAVE,'MetricsPerformsSVMPolyn.csv'), sep=',')
save(msvmPoly, file=paste0(SAVE,'trainModsvmPoly.RData'))
print(paste0('Model is save in:', SAVE))
CLASI
})

#######################################################################
#######################################################################
#				__________
# 				SVM Radial
#				----------
#######################################################################
#######################################################################
FunctionRad<-cmpfun(function(...){
source(paste0(FuncionesLocal, 'ConfuTable.r'))
m<-as.formula(paste(paste(name.CLASES[1], '~'), paste(names(itrain[[1]])[-c(1:2)], collapse='+')))
m.svm.radial<- cmpfun(function(x) e1071::svm(m, x, scale=FALSE, type='C-classification',tolerance=0.1,  kernel = "radial", cost = 5))

predSVMfun <- cmpfun(function(m, d, ...) predict(m, newdata=d, ...))

f.bestmodel<-cmpfun(function(Data, modls,Var.class,...) {
errrrr<-unlist(lapply(modls, function(m)   last(f.confusiontablemetrics(f.confusionAll(Data, m, Var.class))) [,'F1 score']))
modls[which.max(errrrr)][[1]]
})
###############

plan(multisession, workers = n.core) 
options(future.rng.onMisuse="ignore")
modls<-future_lapply(itrain,FUN=m.svm.radial)

Data<-DAT.test
Var.class<-Data[,get(eval(name.CLASES))]
msvmRad<-f.bestmodel(DAT.test,modls, Var.class)
tconfu<-f.confusiontablemetrics(f.confusionAll(Data, msvmRad, Var.class))

########################################################################
RAST <- Rastnorm01(rast(paste0(OPEN, file.img)))
CLASI<-predict(RAST, msvmRad, fun=predSVMfun, na.rm=TRUE, cores = n.core, cpkgs = "e1071") 
		writeRaster(CLASI, filename=paste0(SAVE, 'SVM_Radial','.tif'), overwrite=TRUE)

print(knitr::kable(tconfu), caption='test/predict')

fwrite(tconfu, file=paste0(SAVE,'MetricsPerformsSVMRadial.csv'), sep=',')
save(msvmRad, file=paste0(SAVE,'trainModsvmRad.RData'))
print(paste0('Model is save in:', SAVE))
CLASI
})

#######################################################################
#######################################################################
#				____
# 				nnet
#				----
#######################################################################
#######################################################################
Functionnnet<-cmpfun(function(...){
source(paste0(FuncionesLocal, 'ConfuTable.r'))
m<-as.formula(paste(paste(name.CLASES[1], '~'), paste(names(itrain[[1]])[-c(1:2)], collapse='+')))
n.class<<-nlevels(itrain[[1]][,get(eval(name.CLASES))])
m.nnet<-cmpfun(function(x) nnet(m, x,size=n.class*10, decay=5e-4, maxit=400, trace = FALSE))



predNNETfun <- cmpfun(function(m, d, ...) factor(predict(m, newdata=d,type ='class',...)))

f.bestmodel<-cmpfun(function(Data, modls,Var.class,...) {
errrrr<-unlist(lapply(modls, function(m)   last(f.confusiontablemetrics(f.confusionnnet(Data, m, Var.class))) [,'F1 score']))
modls[which.max(errrrr)][[1]]
})
###############

plan(multisession, workers = n.core) 
options(future.rng.onMisuse="ignore")
modls<-future_lapply(itrain,FUN=m.nnet)

Data<-DAT.test
Var.class<-Data[,get(eval(name.CLASES))]
mnnet<-f.bestmodel(DAT.test,modls, Var.class)
tconfu<-f.confusiontablemetrics(f.confusionnnet(Data, mnnet, Var.class))

########################################################################
RAST <- Rastnorm01(rast(paste0(OPEN, file.img)))
CLASI<-predict(RAST, mnnet, fun=predNNETfun, cores = n.core,cpkgs="nnet")
	 writeRaster(CLASI, filename=paste0(SAVE, 'Nnet', '.tif'), overwrite=TRUE)

print(knitr::kable(tconfu), caption='test/predict')

fwrite(tconfu, file=paste0(SAVE,'MetricsPerformsnnet.csv'), sep=',')
save(mnnet, file=paste0(SAVE,'trainModnnet.RData'))
print(paste0('Model is save in:', SAVE))
CLASI
})



