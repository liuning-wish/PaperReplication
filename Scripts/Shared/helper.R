
colTrans = function(col,perc) {
  rgb(t(grDevices::col2rgb(col)), maxColorValue=255, alpha = perc*255)
}


globe = function(x,
                 y,
                 col,
                 prc = 1,
                 cex,
                 min_cex = .01,
                 bias = c(0, 0),
                 res = 10,
                 sh = 0,
                 sh_bias = c(0, 0)){
  
  cexs = seq(cex, cex * min_cex, length = res)
  cols = f.utils:::colormixer(col, par()$bg, seq(prc, 0, length = res),'hex')
  
  xs = seq(x, x + bias[1], length = res)
  ys = seq(y, y + bias[2], length = res)
  
  if(sh != 0) points(xs[1] + sh_bias[1], 
                     ys[1] + sh_bias[2],
                     cex = cexs[1], col = colTrans(par()$bg,1-sh),
                     pch = 16)
  
  for(i in 1:res){
    points(xs[i], ys[i], cex = cexs[i], col = cols[i], pch = 16)
  }
}
