rgr_hist = function(x,...) UseMethod('rgr_hist')
rgr_hist.numeric = function(x){
  hist(x,prob = T,breaks=29,xlab=deparse(substitute(x)),
       col = 'green',main = paste('Histogram of',deparse(substitute(x))))
  lines(density(x,na.rm = T),col = 'red',lty=2)
  grid()
}
rgr_hist.integer = function(x){
  hist(x,prob = T,breaks=29,xlab=deparse(substitute(x)),
       col = 'green',main = paste('Histogram of',deparse(substitute(x))))
  lines(density(x,na.rm = T),col = 'red',lty=2)
  grid()
}
#rgr_hist(1:10)
rgr_hist.data.frame = function(x){
  x =  x[,sapply(x[1,],is.numeric)]
  layout(matrix(1:ncol(x),1))
  for(i in 1: ncol(x)){
    hist(x[,i],prob = T,breaks=29,xlab=colnames(x)[i],
         col = 'green',main = paste('Histogram of',colnames(x)[i]))
    lines(density(x[,i],na.rm = T),col = 'red',lty=2)
    rug(jitter(x[,i]),col = 'red')
    grid()
  }
  layout(1)
}
#rgr_hist(iris)
