scatter3D(dc$x, dc$y, dc$z, colvar= NULL, col.var= dc$Temp, col= c("red","blue"), pch= 19, cex=1, phi= 0,  bty = "g", colkey = FALSE, ticktype = "detailed",  main ="엔트로피의 증가")
dc2

##

library(plot3D)

repeat{ # 처음 원자들의 좌표를 정해주는 기능
  
  dc<- data.frame(particle= 1:30, Temp= rep(c(1,2), each= 15), num= 0, x= sample(1:5,30, replace= T), y= sample(1:10,30, replace= T),
                  z= sample(1:10,30, replace= T), delx= 0, dely= 0, delz= 0) # dots' coordinate # Temp의 1: low, 2: high
  
  if(!any(duplicated(dc[,4:6]))){ # 좌표가 겹치는 원자가 있는 지를 확인해서 없으면 repeat을 끝내는 기능
    break
  }
}

xr<- rep(5,20) # x.axis surf range
yr<- seq(0, 10, length.out= 20) # y.axis surf range
zr<- matrix(sort(seq(0, 10, length.out= 400), decreasing= T), ncol= 20, byrow= T) # z.axis surf range

# cm.colors

# i가 1, 21, 41, 61, 81될 때마다 위치가 바뀜 (i%%20==1) 



for(i in 1:20){
  
  scatter3D(dc$x, dc$y, dc$z, colvar= dc$Temp, col= c("blue", "red"), theta= 20, phi=30, pch= 19, cex=1, bty = "g", colkey = FALSE, xlim= c(0,10), ylim= c(0,10),
            zlim= c(0,10), ticktype = "detailed", surf= list(x= xr, y= yr, z= zr, facets= NA, col=c(rev(rainbow(20)))), main ="엔트로피의 증가")
  
  dc2<- data.frame(NULL)
  
  if(i%%20==1){
    
    dc2_1<- data.frame(NULL)
    success<- 0 # 후에 원자들의 좌표가 겹치지 않은 경우 1을 할당함
    
    repeat{ # 원자들의 좌표가 무조건 겹칠 수밖에 없다면 원자 1부터 다시 새로운 위치를 정해주는 기능
      
      for(j in 1:30){ # 각 원자들이 0.05초 동안 움직여서 나온 새로운 좌표를 기존의 데이터 프레임에 추가해주는 기능 (원자는 총 20개)
        
        ct<- 0 # count
        
        repeat{ # 새로 정해준 원자들의 위치가 겹치지 않고 원자의 위치가 주어진 좌표를 벗어나지 않을 경우에만 원자의 새로운 위치 값을 계속 쌓아주는 기능
          
          change<- sample(1:2,1)
          
          if(unique(dc[dc$particle==j,"Temp"])==2){ # 원자의 온도가 높은 경우
            
            mv<- 2 # mv: move
          } else{ # 원자의 온도가 낮은 경우
            
            mv<- 0.8
          }
          
          a<- dc[dc$particle== j & dc$num==i-1,4] # 원자의 기존 x 좌표
          b<- dc[dc$particle== j & dc$num==i-1,5] # 원자의 기존 y 좌표
          c<- dc[dc$particle== j & dc$num==i-1,6] # 원자의 기존 z 좌표
          
          if(ct==30){ ## 원자의 위치가 계속 겹치는 경우 일단 움직이지 못한다고 판단하는 기능
            
            cv<- c(a,b,c) # cv: changed value
            
          } else{ ## 원자의 원래 위치를 기준으로 하여 새로운 위치를 정해주는 기능
            
            x<- a + runif(1, min= -mv, max= mv) # 1초 동안 움직인 새로운 x좌표를 먼저 정해주는 기능
            
            y<- runif(1, min=(2* b- sqrt(4* b^2+ 4* (mv^2- (x- a)^2-b^2)))/ 2, max= (2* b+ sqrt(4* b^2+ 4* (mv^2- (x- a)^2-b^2)))/ 2)
            
            cons<- c^2 + c(x-a)^2 + c(y-b)^2 - mv^2 # 구의 방정식에서 z만 변수일 경우의 상수 부분(왼쪽으로 이항했을 경우)
            
            z<- switch(change, c+ sqrt(c^2- cons), c- sqrt(c^2- cons))
            
            delta_x<- (x-a)/ 20 # x 좌표가 0.05초 동안 바뀌는 정도
            delta_y<- (y-b)/ 20 # y 좌표가 0.05초 동안 바뀌는 정도
            delta_z<- (z-c)/ 20 # z 좌표가 0.05초 동안 바뀌는 정도
            
            cv<- c(a+ delta_x, b+ delta_y, c+ delta_z)
            cv_1<- c(x,y,z)
            
          }
          
          if(any(cv_1< 0) | any(cv_1> 10)){ # 원자가 1초 동안 같은 방향으로 움직일 때 주어진 좌표를 벗어나게 되는 경우 위치를 다시 정해주는 기능
            
            next
          }
          
          cvd_1<- data.frame(particle= j, Temp= unique(dc[dc$particle==j,"Temp"]), num= i, x= cv_1[1], y= cv_1[2], z= cv_1[3]) # cdf: changed value data.frame
          dc2_1<- rbind(dc2_1, cvd_1)
          
          if(!any(duplicated(dc2_1[,4:6]))){ # 1초간 움직인 원자들의 위치가 전혀 겹치지 않는 경우
            
            success<- 1
            
            cvd<- data.frame(particle= j, Temp= unique(dc[dc$particle==j,"Temp"]), num= i, x= cv[1], y= cv[2], z= cv[3], delx= delta_x, dely=  delta_y, delz= delta_z) # cdf: changed value data.frame
            dc2<- rbind(dc2, cvd)
            
            break
          } else if(ct>30){ # 1초 이후에 원자가 움직이지 못하는데 원래의 위치도 다른 원자가 차지하여 아예 존재할 수 없게 되는 경우 원자 1부터 새로운 위치를 다시 쌓아가게 만드는 기능
            break
          }
          
          dc2<- dc2[-nrow(dc2),]
          
          ct<- ct+1
        }
      }
      
      if(success==1){
        break
      }
    }
    
    dc<- dc2
  } else{
    
    for(j in 1:30){
      
      cv<- c(dc$x[j]+ dc$delx[j], dc$y[j]+ dc$dely[j], dc$z[j]+ dc$delz[j])
      
      cvd<- data.frame(particle= j, Temp= unique(dc[dc$particle==j,"Temp"]), num= i, x= cv[1], y= cv[2], z= cv[3], delx= dc$delx[j], dely= dc$dely[j], delz= dc$delz[j])
      
      dc2<- rbind(dc2, cvd)
    }
    
    dc<- dc2
  }
}
