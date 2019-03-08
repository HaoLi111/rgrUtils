rgr_corplot = function(x,method = 'ellipse',type = "lower"){
  x = rgr_filter_numeric(x)
  x=cor(x)
  corrplot::corrplot(x,method = method,type=type)
  title(paste('Linear correlation'))
  symnum(x)
}
#rgr_corplot(iris)
