library(compiler)
library(data.table)
library(terra)
library(parallel)

######### normalize image #############################
extract.dat<-cmpfun(function(file.img, name.shape, name.CLASES,OPEN,SAVE, n.core, exact=NULL, Normalize=FALSE, envir=.GlobalEnv) {


  if(is.null(n.core)) n.core<-detectCores()-1 #
if(Normalize)    RAST <- Rastnorm01(rast(paste0(OPEN, file.img))) else RAST <- rast(paste0(OPEN, file.img))
   name.band<<-names(RAST)
   
CLA <- vect(paste0(OPEN, name.shape))
	DAT<-data.table(extract(RAST,CLA, na.rm=FALSE, method='simple', df=TRUE))
CLA <-data.frame(as.data.frame(CLA)[,name.CLASES[1]], 1:nrow(CLA))
names(CLA)<-c( name.CLASES[1], 'ID')
      
	DAT.todos <- merge(data.table(CLA), DAT, by='ID', all=TRUE) 
      nLevels<-nlevels(as.factor(data.frame(CLA)[,name.CLASES[1]]))
  DAT.todos<-na.omit(DAT.todos)
  DAT.todos[,name.CLASES[1]:=as.factor(get(eval(name.CLASES[1])))]
  return(DAT.todos)
})


#####################################################################
		## Separate data in train and validate
#####################################################################

d.val<-cmpfun(function(propVal, DAT.todos) {

sumVal<-DAT.todos[,unique(ID), by=eval(name.CLASES)]
mVal<-sumVal[,.N, by=eval(name.CLASES)]
mVal[, polVal:=round(N*propVal)]
V.pol<-unlist(lapply(1:nrow(mVal), function(i) sample(sumVal[mVal[i ,get(eval(name.CLASES))]==get(eval(name.CLASES)), ]$V1,  mVal[i ,polVal])))

VAL.d<-DAT.todos[, ifelse(ID%in%V.pol,1,0)]
return(list(d.train=DAT.todos[VAL.d==0,], d.Val=DAT.todos[VAL.d==1,]))
})

#################################################
			##Set data ###
#################################################
f.train<-cmpfun(function(data, name.CLASES, sel.n, ndt, ...) {
  if(is.null(n.core)) n.core<-detectCores()-1
  DAT.todos<-data.table(data)
un<-dim(DAT.todos)[1]==length(unique(DAT.todos$ID))
if(isTRUE(un)) {f.random.dat<- cmpfun( function(i)  sub.dt<-DAT.todos[,.SD[sample(.N,min(.N,sel.n))],by = get(eval(name.CLASES))][,-c('get')] ) } else{
f.random.dat<- cmpfun( function(i)  sub.dt<-DAT.todos[,.SD[sample(.N,min(.N,sel.n))],by = 'ID'])
}

cl<-makeCluster(n.core)
clusterEvalQ(cl, library('data.table'))
clusterExport(cl=cl,list('DAT.todos', 'name.CLASES', 'sel.n'), envir=environment())
setDat.parar<-parLapply(cl,c(1:ndt),fun=f.random.dat)
stopCluster(cl)
return(setDat.parar)
})



###############################################
	# Standardize image data
###############################################

# Normalize to [0,1]
Rastnorm01 <- cmpfun(function (xx) {
#x<-rast(paste0(OPEN, file.img))
MaRast<-global(xx, fun="max", na.rm=TRUE)
MiRast<-global(xx, fun="min",na.rm=TRUE)
(xx-MiRast[,1])/(MaRast[,1]-MiRast[,1])
})
  

