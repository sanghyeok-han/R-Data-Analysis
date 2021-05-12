library(ggplot2)
library(stringr)


show<- function(c_mt,turn,ft,dc=0){ # 현재 판의 상태를 시각적으로 보여주는 기능 # dc: description,  1 이상이면 초반 설명, 0이면 플레이 중
  
  if(turn== ft){ # 현재 차례의 플레이어의 색깔 표시를 O, X의 색깔과 맞춰주는 기능
    sp<- "X" # sp: shape
    cl<- "red" # cl: color
  } else{
    sp<- "O"
    cl<- "blue"
  }
  
  mode(c_mt)<- "character" # c_mt의 원소들을 character로 바꿔주는 기능
  
  c_mt<- gsub("0", "", c_mt) # c_mt의 "0","1","2"를 각각 "","X","O"로 바꿔주는 기능
  c_mt<- gsub("1", "X", c_mt)
  c_mt<- gsub("2", "O", c_mt)
  
  c_df<- data.frame(num= 1:9, type= c(c_mt[1,], c_mt[2,], c_mt[3,]), x= rep(seq(1.5,7.5,3), times= 3), y= rep(c(7.5,4.5,1.5), each= 3)) # current data.frame
  
  if(dc==0){ # 게임 진행 중의 경우
    
    board<- ggplot(c_df, aes(x= x,y= y, color= type)) + geom_text(aes(label= type), size= 30) +  scale_x_continuous(breaks= c(0,3,6,9), limits= c(0,9), expand= c(0.005,0)) +
      scale_y_continuous(breaks= c(0,3,6,9), limits= c(0,9), expand= c(0,0.04)) + scale_color_manual(values= c("black","red","blue"), guide= F) +
      geom_vline(xintercept= seq(0,9,3), size= 1.5, linetype= 2) + geom_hline(yintercept= seq(0,9,3), size= 1.5, linetype=  2) + ggtitle(paste0(sp, "의 차례입니다.\n")) +
      theme(plot.title= element_text(face= "bold", size= 16, vjust= 2, hjust= 0.95, color= cl), axis.title= element_blank(), axis.text= element_blank())
    print(board)
    
  } else{ # 게임 초반 설명 및 난이도 설정의 경우
    
    cl<- "black"
    
    if(dc==1){
      
      c_df$type<- 1:9
      title<- "각 칸의 번호는 가로 방향으로 1부터 9까지 증가합니다.\n"
    } else if(dc==2){
      
      title<- "난이도를 선택해 주세요(쉬움: 0, 보통: 1)\n"
    } else if(dc==3){
      
      title<- "난이도를 다시 선택해 주세요(쉬움: 0, 보통: 1)\n"
    } else if(dc==4){
      
      title<- "게임을 시작합니다 !       \n"
    }
    
    board<- ggplot(c_df, aes(x= x,y= y)) + geom_text(aes(label= type), size= 30) +  scale_x_continuous(breaks= c(0,3,6,9), limits= c(0,9), expand= c(0.005,0)) +
      scale_y_continuous(breaks= c(0,3,6,9), limits= c(0,9), expand= c(0,0.04)) + geom_vline(xintercept= seq(0,9,3), size= 1.5, linetype= 2) +
      geom_hline(yintercept= seq(0,9,3), size= 1.5, linetype=  2) + ggtitle(title) +
      theme(plot.title= element_text(face= "bold", size= 16, vjust= 2, hjust= 0.95, color= cl), axis.title= element_blank(), axis.text= element_blank())
    print(board)
  }
}

turn<- 1

if(T){
  
  ft<- sample(1:2, 1) # 첫 번째 턴을 플레이어와 컴퓨터 중에서 누가 먼저 할 지 정하는 기능 first turn, # 1: 플레이어, 2: 컴퓨터
  turn<- ft # 첫 번째 턴을 할당해주는 기능
  
  c_mt<- matrix(rep(0,9), nrow= 3) # 초반의 게임 상태 정보를 할당함(전부 0)
  
  show(c_mt,turn,ft,1) # 게임 칸 순서 설명
  show(c_mt,turn,ft,2) # 게임 난이도 설정
  
  di<- as.integer(readline("난이도를 선택해 주세요(쉬움: 0, 보통: 1): "))
  
  while(di!=0 & di!= 1){ # 게임 난이도를 잘못 입력한 경우
    
    di<- as.integer(readline("다시 선택해 주세요(쉬움: 0, 보통: 1): "))
    
    if(di==0 | di==1){
      
      break
    }
    
    show(c_mt,turn,ft,3) # 빈 판과 함께 다시 입력하라는 메시지를 출력해줌
  }
  
  show(c_mt,turn,ft,4) # 게임 시작을 알려줌
  
  Sys.sleep(1.5)
  
  turn<-1
  
  repeat{ # 승부가 날 때까지 게임을 반복해주는 기능
    
    if(turn== 1){ # 플레이어의 차례인 경우
      
      lc<- as.integer(readline("표시를 할 위치를 선택해주세요(1~9까지의 칸 중 빈칸 번호 입력 가능): ")) # location
      
      while(lc<1 | lc>9 | c(c_mt[1,], c_mt[2,], c_mt[3,])[lc]==1 | c(c_mt[1,], c_mt[2,], c_mt[3,])[lc]==2){ # 플레이어가 칸을 벗어나거나 이미 표시가 있는 곳에 표시를 하려는 경우
        
        lc<- as.integer(readline("위치를 다시 선택해주세요(1~9까지의 칸 중 빈칸 번호 입력 가능): "))
      }
      
      i<- ifelse(lc<=3, 1, ifelse(lc>=4 & lc<=6, 2, 3))
      j<- ifelse(lc%%3==1, 1, ifelse(lc%%3==2, 2, 3))
      
      c_mt[i,j]<- ifelse(ft==turn, 1, 2) # 해당 위치에 플레이어가 처음 시작했으면 1(X)을 나중에 시작했으면 2(O)를 할당함
    }
    
    
    if(turn== 2){ # 컴퓨터의 차례인 경우
      if(di==0){ # 난이도를 쉬움으로 설정한 경우
        
        
        
        
      } else{ # 난이도를 보통으로 설정한 경우
        
        
        
      }
    }
    
    show(c_mt,turn,ft)
    
    if(turn==1){ # 플레이어의 순서를 바꿔주는 기능
      turn<- 2
    } else{
      turn<- 1
    }
    
    break
  }
}

nc<- function(mt){ # c_mt의 원하는 서브셋에 0,1,2이 각각 몇 개씩 있는 지를 반환해주는 함수, nc: number counter 
  
  c<- NULL # c: count
  c[1]<- length(unlist(str_match_all(mt, "0")))
  c[2]<- length(unlist(str_match_all(mt, "1")))
  c[3]<- length(unlist(str_match_all(mt, "2")))
  
  return(c)
}

nc(a[1,])[1]

