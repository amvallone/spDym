stz<-function(x){# estandira una variable
    y<-(x-mean(x))/sd(x)
   return(y)
}
