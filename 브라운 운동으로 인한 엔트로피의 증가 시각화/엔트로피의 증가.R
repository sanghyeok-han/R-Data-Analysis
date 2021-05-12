library(ggplot2)
library(magrittr)


# 모든 원자들의 온도가 같은 경우

repeat{
  
  dc<- data.frame(x= sample(1:5,20, replace= T), y= sample(1:10,20, replace= T)) # dots' coordinate
  rownames(dc)<- paste0("particle ", 1:20)
  
  if(!any(duplicated(dc))){
    break
  }
}

for(i in 1:50){
  
  le<- 0
  ri<- 0
  
  for(j in 1:20){
    
    if(dc[j,1]<=5){
      le %<>% +1
    } else{
      ri %<>% +1
    }
  }
  
  le_space<- ""
  ri_space<- ""
  
  if(le<10){
    le_space<- "  "
  }
  
  if(ri<10){
    ri_space<- "  "
  }
  
  graph<- ggplot(dc, aes(x= x, y= y)) + geom_point() + scale_x_continuous(breaks= 1:10, limits= c(1,10)) + scale_y_continuous(breaks= 1:10, limits= c(1,10)) +
                                        geom_vline(xintercept= 5.5, linetype= 2, color= "blue", size=1.5) +
                                        labs(caption= paste0("                          총 원자의 수:  20\n    왼쪽 방에 있는 원자의 수:  ", le_space, le, "\n오른쪽 방에 있는 원자의 수:  ", ri_space, ri)) +
                                        theme(plot.caption= element_text(hjust= 0))
  print(graph)

  dc2<- data.frame(NULL)
  
  for(j in 1:20){
    
    repeat{
      
      change<- sample(1:4,1)
      cv<- switch(change, c(dc[j,1]+0.5, dc[j,2]), c(dc[j,1]-0.5, dc[j,2]), c(dc[j,1], dc[j,2]+0.5), c(dc[j,1], dc[j,2]-0.5)) # cv: changed value
      
      if(any(cv< 1) | any(cv> 10)){
        
        next
      }
      
      dc2<- rbind(dc2, cv)
      
      if(!any(duplicated(dc2))){
        
        break
      }
      
      dc2<- dc2[-nrow(dc2),]
      
    }
  }
  
  dc<- dc2
  colnames(dc)<- c("x","y")

  
  Sys.sleep(0.1)
}

## 연습장


a<- data.frame(a= c(1,2,3), b= c(3,4,5))
a

for(i in 1:5){
  g<- ggplot(a, aes(x=a, y=b)) + geom_point()
  print(g)
  Sys.sleep(3)
  
  a[3,1]<- sample(1:3, 1)
  
  
}

a[1,]<- switch(1, c(a[1,1],a[1,2]+1))

a
all(a[1,] > 2)

a<- 2

while(T){
  
  a<- a+1
  
  if(a<10){
    next
  }
}

a<- " "
a