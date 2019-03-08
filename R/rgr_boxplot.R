rgr_boxplot = function(x) UseMethod('rgr_boxplot')
rgr_boxplot.data.frame = function(x){
  x =  x[,sapply(x[1,],is.numeric)]
  layout(matrix(1:ncol(x),1))
  for(i in 1: ncol(x)){
    boxplot(x[,i],ylab = colnames(x)[i],col = 'green',
            main=paste('Boxplot of',colnames(x)[i]))
    try(rug(jitter(x[,i]),col='red',side = 2));abline(h=mean(x[,i],na.rm=T),lty=2,col = 'red')
    grid()
  }
}

rgr_boxplot.numeric = function(x){
  boxplot(x[,i],ylab = deparse(substitute(x)),col = 'green',
          main=paste('Boxplot of',deparse(substitute(x))))
  grid()
  try(rug(jitter(x),col='red',side = 2));abline(h=mean(x,na.rm=T),lty=2,col = 'red')
}

rgr_boxplot.data.frame(iris)
