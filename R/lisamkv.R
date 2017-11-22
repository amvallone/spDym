lisamkv<-function(x,W){#COmputa LISA MARKOV matrix and moves
  if(is.null(dim(x))==TRUE) stop("You must provide a matrix conteaining n spatial unita and t periods of time")
  t<-dim(x)[2]
  n<-dim(x)[1]
  if(t<2) stop("At least you must provide two period of time for this analysis")
  y<-matrix(0,nrow=n,ncol=t)
  for (i in 1:t){
    y[,i]<-lag(W,x[,i])
  }
  L.x<-apply(x,2,stz)
  L.y<-apply(y,2,stz)
  LISA<-matrix(0,nrow=n,ncol=t)
  for(j in 1:dim(x)[2]){
    for(i in 1:dim(x)[1]){
      if (L.x[i,j]>=0 & L.y[i,j]>=0) LISA[i,j]<-1
      if (L.x[i,j]>=0 & L.y[i,j]<0) LISA[i,j]<-4
      if (L.x[i,j]<0 & L.y[i,j]>=0) LISA[i,j]<-2
      if (L.x[i,j]<0 & L.y[i,j]<0) LISA[i,j]<-3
    }
  }
  move<-matrix(0,nrow=n,ncol=(t-1))
  lmatrix<-matrix(0,nrow=4,ncol=4)
  for (i in 1:(t-1)){
    for (j in 1:n){
      if (LISA[j,i]==1){
        if (LISA[j,i+1]==1) {move[j,i]<-1 ; lmatrix[1,1]<-lmatrix[1,1]+1}
        if (LISA[j,i+1]==2) {move[j,i]<-2 ; lmatrix[1,2]<-lmatrix[1,2]+1}
        if (LISA[j,i+1]==3) {move[j,i]<-3 ; lmatrix[1,3]<-lmatrix[1,3]+1}
        if (LISA[j,i+1]==4) {move[j,i]<-4 ; lmatrix[1,4]<-lmatrix[1,4]+1}
      }
      if (LISA[j,i]==2){
        if (LISA[j,i+1]==1) {move[j,i]<-5 ; lmatrix[2,1]<-lmatrix[2,1]+1}
        if (LISA[j,i+1]==2) {move[j,i]<-6 ; lmatrix[2.2]<-lmatrix[2,2]+1}
        if (LISA[j,i+1]==3) {move[j,i]<-7 ; lmatrix[2.3]<-lmatrix[2,3]+1}
        if (LISA[j,i+1]==4) {move[j,i]<-8 ; lmatrix[2,4]<-lmatrix[2,4]+1}
      }
      if (LISA[j,i]==3){
        if (LISA[j,i+1]==1) {move[j,i]<-9 ; lmatrix[3,1]<-lmatrix[3,1]+1}
        if (LISA[j,i+1]==2) {move[j,i]<-10 ; lmatrix[3,2]<-lmatrix[3,2]+1}
        if (LISA[j,i+1]==3) {move[j,i]<-11 ; lmatrix[3,3]<-lmatrix[3,3]+1}
        if (LISA[j,i+1]==4) {move[j,i]<-12 ; lmatrix[3,4]<-lmatrix[3,4]+1}
      }
      if (LISA[j,i]==4){
        if (LISA[j,i+1]==1) {move[j,i]<-13 ; lmatrix[4,1]<-lmatrix[4,1]+1}
        if (LISA[j,i+1]==2) {move[j,i]<-14 ; lmatrix[4,2]<-lmatrix[4,2]+1}
        if (LISA[j,i+1]==3) {move[j,i]<-15 ; lmatrix[4,3]<-lmatrix[4,3]+1}
        if (LISA[j,i+1]==4) {move[j,i]<-16 ; lmatrix[4,4]<-lmatrix[4,4]+1}
      }
    }
  }
  p.lisa<-lmatrix
  for (i in 1:4) {p.lisa[i, ] <- p.lisa[i, ] / sum(p.lisa[i, ])}
  output<-list(move,lmatrix,p.lisa)
  names(output)<-c("move","lisamatrix","p.lisamatrix")
  class(output)<-"lasamkv"
  return(output)
}
