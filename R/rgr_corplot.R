ugr_corplot = function(x,method = 'ellipse',type = "lower"){
  x = ugr_filter_numeric(x)
  x=cor(x)
  corrplot::corrplot(x,method = method,type=type)
  title(paste('Linear correlation'))
  symnum(x)
}
#ugr_corplot(iris)
