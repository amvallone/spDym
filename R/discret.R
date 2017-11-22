discret<-function(x, classes=5,type=7,...){ #subdivide una variable en clases
    if(length(classes)!=1){
      if(max(classes)==1){
        aux<-quantile(x,classes,type = type, ...)
        aux<-c(-Inf,aux[2:(length(aux)-1)],Inf)
        output<-as.numeric(cut(x,breaks=aux))
      } else {
        aux<-c(-Inf,classes,Inf)
        output<-as.numeric(cut(x,breaks=aux))
      }
    } else {
      output<-as.numeric(cut(x,breaks=classes))
    }
    return(output)
}
