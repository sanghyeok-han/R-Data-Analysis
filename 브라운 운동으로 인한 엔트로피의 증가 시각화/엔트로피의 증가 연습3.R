# x, y, z variables
x <- mtcars$wt
y <- mtcars$disp
z <- mtcars$mpg
# Compute the linear regression (z = ax + by + d)
fit <- lm(z ~ x + y)
# predict values on regular xy grid
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(fit)
# scatter plot with regression plane
scatter3D(x, y, z, pch = 18, cex = 2, 
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = "wt", ylab = "disp", zlab = "mpg",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = "mtcars")

?scatter3D

####

library(magrittr)

repeat{ # 처음 원자들의 좌표를 정해주는 기능
  
  dc<- data.frame(particle= 1:20, Temp= rep(c(1,2), each= 10), num= 0, x= sample(1:5,20, replace= T), y= sample(1:10,20, replace= T),
                  z= sample(1:10,20, replace= T), delx= 0, dely= 0, delz= 0) # dots' coordinate # Temp의 1: low, 2: high
  
  if(!any(duplicated(dc[,4:6]))){ # 좌표가 겹치는 원자가 있는 지를 확인해서 없으면 repeat을 끝내는 기능
    break
  }
}


grid.lines= 30

xr<- rep(5,30) # x.axis surf range
yr<- seq(0, 10, length.out= 30) # y.axis surf range
zr<- matrix(sort(seq(0, 10, length.out= 900), decreasing= T), ncol= 30, byrow= T) # z.axis surf range

le<- 0
ri<- 0

le_space<- ""
ri_space<- ""

for(j in 1:30){
  
  if(dc[j,"x"]<=5){
    le %<>% +1
  } else{
    ri %<>% +1
  }
}

if(le<10 & ri>10){
  le_space<- "    "
  ri_space<- "  "
} else if(le>10 & ri<10){
  le_space<- "  "
  ri_space<- "    "
} else{
  le_space<- "  "
  ri_space<- "  "
}

if(le<10){
  le_space<- " "
}

if(ri<10){
  ri_space<- " "
}

scatter3D(dc$x, dc$y, dc$z, colvar= dc$Temp, col= c("blue", "red"), theta= 20, phi=30, pch= 19, cex=1, bty = "g", colkey = FALSE, xlim= c(0,10), ylim= c(0,10),
          zlim= c(0,10), ticktype = "detailed", surf= list(x= xr, y= yr, z= zr, facets= NA, col=c(rev(rainbow(20)))), main ="엔트로피의 증가",
          sub= paste0("총 원자 수: 30  왼쪽 방 원자 수:  ", le_space, le, "   오른쪽 방에 있는 원자의 수:  ", ri_space, ri))

scatter3D(dc$x, dc$y, dc$z, colvar= dc$Temp, col= c("blue", "red"), theta= 20, phi=30, pch= 19, cex=1, bty = "g", colkey = FALSE, xlim= c(0,10), ylim= c(0,10),
          zlim= c(0,10), ticktype = "detailed", main ="엔트로피의 증가", surf= list(x= xr , y= yr, z= zr, facets= NA, col=c(topo.colors(50))),
          sub= paste0("                     총 원자의 수:    20\n   왼쪽 방에 있는 원자의 수:  ", le_space, le, "\n오른쪽 방에 있는 원자의 수:  ", ri_space, ri))
?rainbow
         

?scatter3D 

scatter3D(dc$x, dc$y, dc$z, colvar= dc$Temp, col= c("blue", "red"), theta= 20, phi=30, pch= 19, cex=1, bty = "u", col.panel ="ghost white", col.grid = "black", colkey = FALSE, xlim= c(0,10), ylim= c(0,10),
          zlim= c(0,10), ticktype = "detailed", surf= list(x= xr, y= yr, z= zr, facets= NA, col=c(rev(rainbow(20)))), main ="엔트로피의 증가",
          sub= paste0("총 원자 수: 30  왼쪽 방 원자 수:  ", le_space, le, "  오른쪽 방 원자 수:  ", ri_space, ri))


scatter3D(dc$x, dc$y, dc$z, colvar= dc$Temp, col= c("blue", "red"), theta= 20, phi=30, pch= 19, cex=1.1, bty = "u", col.panel ="#006700", col.grid = "#005B00",
          col.axis= "black", colkey = FALSE, xlim= c(0,10), ylim= c(0,10),
          zlim= c(0,10), ticktype = "detailed", surf= list(x= xr, y= yr, z= zr, facets= NA, col=c(rev(rainbow(20)))), main ="엔트로피의 증가",
          sub= paste0("총 원자 수: 30  왼쪽 방 원자 수:  ", le_space, le, "  오른쪽 방 원자 수:  ", ri_space, ri))
