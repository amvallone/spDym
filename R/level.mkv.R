level.mkv<-function(m,col=c("gray","red"),xlab="Final Period",ylab="Initial Period",title=NULL,
                    cell.size=8,axis.size=16,tick.size=14,...){
  if(sum(colSums(m))!=ncol(m)){
    for (i in 1:ncol(m)) {m[i, ] <- m[i, ] / sum(m[i, ])}
  }
  m<-round(m,4)
  x<-1:nrow(m)
  y<-1:ncol(m)
  Probabilities<-as.vector(t(m))
  data<-expand.grid(x=x,y=y)
  ggplot(data,aes(x,y,Probabilities,label=as.character(Probabilities)))+geom_tile(aes(fill=Probabilities))+theme_bw() +
    scale_fill_gradient(low=col[1], high=col[2],... )+
    labs(x=xlab, y=ylab,title=title)+
    theme(axis.title=element_text(size=axis.size,face="bold"),
          axis.text = element_text(size=tick.size),
          legend.text =element_text(size=tick.size),
          legend.title=element_text(size=.8*axis.size,face="bold"),
          plot.title = element_text(size=2*axis.size,hjust=0.5),...)+
    geom_text(size=cell.size,...)+scale_y_reverse()+scale_x_continuous(position = "top")
}
