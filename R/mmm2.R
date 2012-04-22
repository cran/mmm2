mmm2 <-
function(data,nresp,rtype=TRUE,interaction=NULL,coefnames=NULL,family="gaussian",tol=0.001,maxiter=25,corstr="independence",Mv=1,silent=TRUE){

data<-data.frame(data)

if (rtype==TRUE){
if (length(coefnames)!=0){
lcoef<-length(coefnames)
colcov<-ncol(data)-(nresp+1)+(nresp-1)+(nresp-1)*length(interaction)
if (lcoef!=(colcov+1)) stop("Length of coefficient names are not multiple of number of coefficients")
}
}else{
if (length(coefnames)!=0){
lcoef<-length(coefnames)
colcov<-ncol(data)-(nresp+1)
if (lcoef!=(colcov+1)) stop("Length of coefficient names are not multiple of number of coefficients")
}
}

mresp<-NULL
for (i in 1:dim(data)[1]){
mresp<-rbind(mresp,t(data[i,2:(2+nresp-1)]))
}

covmat<-NULL
for (i in 1:dim(data)[1]){
cov<-NULL
for (j in 1:nresp){
cov<-rbind(cov,data[i,(1+nresp+1):dim(data)[2]])
}
covmat<-rbind(covmat,cov)
}

id<-NULL
for (i in 1:dim(data)[1]){
id2<-NULL
for (j in 1:nresp){
id2<-rbind(id2,data[i,1])
}
id<-rbind(id,id2)
}

if (rtype==TRUE){
r<-matrix(rep(0,nresp*(nresp-1)),nrow=nresp)
for (i in 1:(nresp-1)){
r[(nrow(r)-(i-1)),i]<-1
}

resptype<-NULL
for (i in 1:(nrow(covmat)/nrow(r))){
resptype<-rbind(resptype,r)
}

interact<-NULL
if (length(interaction)!=0){
for (i in 1:length(interaction)){
interact<-cbind(interact,resptype*covmat[,interaction[i]])
}
covmat<-cbind(covmat,resptype,interact)
}else{
covmat<-cbind(covmat,resptype)
}

}
covmat<-as.matrix(covmat)
covmat<-cbind(rep(1,nrow(covmat)),covmat)

if (length(coefnames)!=0){
colnames(covmat)<-coefnames
}#if

library(gee)
fitted.<-covmat
gee1<-gee(mresp~-1+fitted.,id=id,family=family,tol=tol,maxiter=maxiter,corstr=corstr,Mv=Mv,silent=silent)
#summary1<-summary(gee1)
if (length(coefnames)!=0){
#row.names(summary1$coefficients)<-coefnames
#row.names(gee1$coefficients)<-coefnames
}
#list1<-summary1
#list1
gee1
}

