##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
		#Confusion table#
##################################################################
##################################################################
f.confusiontablemetrics<-cmpfun(function(T.contin,...) {
total<-apply(T.contin,1,sum)
VP<-diag(T.contin)
FP<-apply(T.contin,2,sum)- diag(T.contin)
FN<-apply(T.contin,1,sum) - diag(T.contin)
VN<-sum(VP)-VP

preciS<- VP/(VP+FP)
recall<-  VP/(VP+FN) 
specificity<-VN/(VN+FP) 
F1score<- 2*((preciS*recall)/(preciS+recall)) 
Prec.recallRatio<-preciS/recall
BalancedAccurasyScore <-mean (recall)
preciS.average<-mean (preciS)
F1score.average<- BalancedAccurasyScore*preciS.average/(BalancedAccurasyScore+preciS.average)


FNR<- FN/(FN+VP)   #miss rate or false negative rate (FNR)
FPR<- FP/(FP+VN) #miss rate or false positive rate
mean(FNR)


metrics<-cbind(preciS, recall,specificity,  F1score)
summa<-data.table(as.data.frame.matrix(as.matrix(T.contin)), metrics)
dsumm<-data.table(t(c(rep(NA, ncol(T.contin)), as.vector(apply(metrics, 2, median, na.rm =T))))    )
colnames(dsumm)<-colnames(summa)
metricaEnd<-rbind(summa, dsumm)
colnames(metricaEnd)<- c(colnames(summa)[-c((ncol(summa)-3):ncol(summa))], 'Presicion', 'Recall',  "specificity", 'F1 score')


return(data.table(Class=c(rownames(T.contin), NA), metricaEnd))
})


###solo genera la tabla de contingencia


f.confusionAll<-cmpfun(
	function(Data, mod, Var.class,...) {
	table(Var.class,predict(mod, newdata=Data) )
})

f.confusionXGB<-cmpfun(
	function(Data, mod, Var.class,...){
	f.DcontinXgbts(Data, mod, Var.class)
})

f.confusionnnet<-cmpfun(
	function(Data, mod, Var.class,...){
	PREDD<- factor(predict(mod, newdata=Data,type ='class'))
	table(Var.class,PREDD)
})


