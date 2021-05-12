library(plot3D)

a<- mtcars

a$am[which(mtcars$am == 0)] <- 'Automatic'
a$am[which(mtcars$am == 1)] <- 'Manual'
a$am <- as.factor(mtcars$am)

str(a)

?scatter3D

####

repeat{ # 처음 원자들의 좌표를 정해주는 기능
  
  dc<- data.frame(particle= 1:20, Temp= rep(c(1,2), each= 10), num= 0, x= sample(1:5,20, replace= T), y= sample(1:5,20, replace= T),
                  z= sample(1:10,20, replace= T)) # dots' coordinate
  
  if(!any(duplicated(dc[,4:6]))){ # 좌표가 겹치는 원자가 있는 지를 확인해서 없으면 repeat을 끝내는 기능
    break
  }
}
str(dc)

scatter3D(dc$x, dc$y, dc$z, colvar= NULL, col.var= dc$Temp, col= c("red","blue"), pch= 19, cex=1, phi= 0,  bty = "g", colkey = FALSE, ticktype = "detailed",  main ="엔트로피의 증가")


####

df<- data.frame(a= c(1,1,2,2), x= c(1,2,5,6), y= c(1,2,5,6), z= c(1,2,5,6))

scatter3D(df$x, df$y, df$z, colvar= df$a, col= c("red", "blue"))

#499649
#80DA80
#92C049
#006700