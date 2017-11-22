mkv<-function(m,classes=5,fixed=FALSE,type=7,prob=TRUE,...){ #COmpute Markox transition matrix
  if(is.null(dim(m))==TRUE) stop("You must provide a matrix conteaining n spatial unita and t periods of time")
  t<-dim(m)[2]
  n<-dim(m)[1]
  if(t<2) stop("At least you must provide two period of time for this analysis")
  if (fixed==TRUE){
    if(length(classes)!=1){
      if(max(classes)==1){
        cuts<-quantile(as.vector(m),classes,type = type, ...)
      }
    } else {
      cuts<-quantile(as.vector(m) ,seq(0,1,length.out=classes),type = type, ...)
    }
    x<-apply(m,2,discret,classes=cuts)
  } else{
    x<-apply(m,2,discret,classes=classes)
  }
  clases<-as.numeric(unique(as.factor(x)))
  clases<-sort(clases)
  mm<-matrix(0,nrow=length(clases),ncol=length(clases))
    for (i in 1:dim(x)[1]){
      for(j in 1:(dim(x)[2]-1)){
        mm[x[i,j], x[i,j + 1]] <- mm[x[i,j], x[i,j+1]] + 1
      }
    }
  if(prob==TRUE) {
    for (i in 1:length(clases)) {mm[i, ] <- mm[i, ] / sum(mm[i, ])}
  }
  rownames(mm)<-clases
  colnames(mm)<-clases
  class(mm)<-c("matrix","mkvm")
  mm
}
