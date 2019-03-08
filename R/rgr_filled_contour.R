rgr_filled_contour = function(x,...) UseMethod('rgr_contour')
rgr_filled_contour.matrix=function(x,xbase=NULL,ybase=NULL){
  if(is.null(xbase)){
    filled.contour(z=x,plot.title = paste("Filled contour of",deparse(substitute(x))))
  }else{
    filled.contour(x=xbase,y=ybase,x)
  }

}
#rgr_contour.data.frame =
rgr_filled_contour.function = function(x,asp='2',
                                      xbase=seq(from=-3,to=3,length.out = 30),
                                      ybase=seq(from=-3,to=3,length.out = 30),
                                      Length = 2){
  if(Length == 2){
    M= matrix(NA,length(xbase),length(ybase))
    for(i in seq_along(xbase)) for(j in seq_along(ybase)) M[i,j] = x(xbase[i],ybase[j])
    filled.contour(z=M,x=xbase,y=ybase)
  }
}
#filled.contour((function(x,y) x^2+y^2))
