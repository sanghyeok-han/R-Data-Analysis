if(require(ggplot2)==F){
  
  install.packages("ggplot2")
  library(ggplot2)
}

if(require(stringr)==F){
  
  install.packages("stringr")
  library(stringr)
}


show<- function(c_mt, ft, dc, turn=0){ # 현재 판의 상태를 시각적으로 보여주는 기능, dc: description, 0이면 플레이 중, 1~4면 초반 설명, 5~9이면 결과 및 다시 시작 여부 설명, 10이면 종료 설명
  
  if(turn%%2==1){ # 현재 차례의 플레이어의 색깔 표시를 O, X의 색깔과 맞춰주는 기능
    
    sp<- ifelse(ft==1, "플레이어(X)", "컴퓨터(x)") # sp: shape
    cl<- "red" # cl: color
  } else{
    sp<- ifelse(ft==2, "플레이어(O)", "컴퓨터(O)")
    cl<- "blue"
  }
  
  c_df<- data.frame(num= 1:12, type= factor(c(c_mt[1,], c_mt[2,], c_mt[3,],0,1,2), levels= c(0,1,2), labels= c("","X","O")), x= c(rep(seq(1.5,7.5,3), times= 3), rep(10,3)), y= c(rep(c(7.5,4.5,1.5), each= 3), rep(10,3))) # current data.frame
  
  if(dc==0){ # 게임 진행 중의 경우
    
    board<- ggplot(c_df, aes(x= x,y= y, color= type)) + geom_text(aes(label= type), size= 30) +  scale_x_continuous(breaks= c(0,3,6,9), limits= c(0,9), expand= c(0.005,0)) +
            scale_y_continuous(breaks= c(0,3,6,9), limits= c(0,9), expand= c(0,0.04)) + scale_color_manual(values= c("black","red","blue"), guide= F) +
            geom_vline(xintercept= seq(0,9,3), size= 1.5, linetype= 2) + geom_hline(yintercept= seq(0,9,3), size= 1.5, linetype=  2) + ggtitle(paste0(sp, "의 차례입니다.\n")) +
            theme(plot.title= element_text(face= "bold", size= 16, vjust= 2, hjust= 0.95, color= cl), axis.title= element_blank(), axis.text= element_blank())
    print(board)
    
  } else if((dc>=1 & dc<=4) | dc==10){ # 게임 초반 설명 및 난이도 설정의 경우
    
    cl<- "black"
    
    if(dc==1){
      
      c_df$type<- 1:12
      title<- "각 칸의 번호는 가로 방향으로 1부터 9까지 증가합니다.\n"
    } else if(dc==2){
      
      title<- "난이도를 선택해 주세요(쉬움: 0, 보통: 1, 어려움: 2)\n"
    } else if(dc==3){
      
      title<- "난이도를 다시 선택해 주세요(쉬움: 0, 보통: 1, 어려움: 2)\n"
    } else if(dc==4){
      
      title<- "게임을 시작합니다 !       \n"
    } else{
      
      title<- "게임을 종료합니다 !       \n"
    }
    
    board<- ggplot(c_df, aes(x= x,y= y)) + geom_text(aes(label= type), size= 30) +  scale_x_continuous(breaks= c(0,3,6,9), limits= c(0,9), expand= c(0.005,0)) +
            scale_y_continuous(breaks= c(0,3,6,9), limits= c(0,9), expand= c(0,0.04)) + geom_vline(xintercept= seq(0,9,3), size= 1.5, linetype= 2) +
            geom_hline(yintercept= seq(0,9,3), size= 1.5, linetype=  2) + ggtitle(title) +
            theme(plot.title= element_text(face= "bold", size= 16, vjust= 2, hjust= 0.95, color= cl), axis.title= element_blank(), axis.text= element_blank())
    print(board)
  } else{ # 게임 결과 설명의 경우
    
    cl<- "black"
    
    if(dc==5){
      
      title<- "플레이어가 승리하였습니다 !      \n"
    } else if(dc==6){
      
      title<- "컴퓨터가 승리하였습니다 !     \n"
    } else if(dc==7){
      
      title<- "게임을 비겼습니다 !       \n"
    } else if(dc==8){
      
      title<- "게임을 이어서 하시겠습니까? (예: 1, 아니오: 2)\n"
    } else{
      
      title<- "다시 입력해 주세요(예: 1, 아니오: 2)\n"
    }
    
    board<- ggplot(c_df, aes(x= x,y= y, color= type)) + geom_text(aes(label= type), size= 30) +  scale_x_continuous(breaks= c(0,3,6,9), limits= c(0,9), expand= c(0.005,0)) +
            scale_y_continuous(breaks= c(0,3,6,9), limits= c(0,9), expand= c(0,0.04)) + scale_color_manual(values= c("black","red","blue"), guide= F) +
            geom_vline(xintercept= seq(0,9,3), size= 1.5, linetype= 2) + geom_hline(yintercept= seq(0,9,3), size= 1.5, linetype=  2) + ggtitle(title) +
            theme(plot.title= element_text(face= "bold", size= 16, vjust= 2, hjust= 0.95, color= cl), axis.title= element_blank(), axis.text= element_blank())
    print(board)
  }
}


nc<- function(mt){ # c_mt의 원하는 서브셋에 0,1,2이 각각 몇 개씩 있는 지를 반환해주는 함수, nc: number counter 
  
  c<- NULL # c: count
  c[1]<- length(unlist(str_match_all(mt, "0")))
  c[2]<- length(unlist(str_match_all(mt, "1")))
  c[3]<- length(unlist(str_match_all(mt, "2")))
  
  return(c)
}


checker<- function(c_mt, ft, turn){ # 승부 결정 여부 및 승자를 확인해주는 함수
  
  
  ws<- 0 # winner's shape, 1: X, 2: O
  result<- 0
  
  for(i in 1:3){
    
    if(nc(c_mt[i,])[2]== 3){ # 한 행에 1(x)이 3개 있는 경우
      
      ws<- 1
      break
    }
    
    if(nc(c_mt[i,])[3]== 3){ # 한 행에 2(O)이 3개 있는 경우
      
      ws<- 2
      break
    }
    
    if(nc(c_mt[,i])[2]== 3){ # 한 열에 1(x)이 3개 있는 경우
      
      ws<- 1
      break
    }
    
    if(nc(c_mt[,i])[3]== 3){ # 한 열에 2(O)이 3개 있는 경우
      
      ws<- 2
      break
    }
  }
  
  if(nc(diag(c_mt))[2]==3){ # 왼쪽 대각선에 1(X)이 3개 있는 경우
    
    ws<- 1
  }
  
  if(nc(diag(c_mt))[3]==3){ # 왼쪽 대각선에 2(O)이 3개 있는 경우
    
    ws<- 2
  }
  
  if(nc(c(c_mt[1,], c_mt[2,], c_mt[3,])[c(3,5,7)])[2]== 3){ # 오른쪽 대각선에 1(X)이 3개 있는 경우
    
    ws<- 1
  }
  
  if(nc(c(c_mt[1,], c_mt[2,], c_mt[3,])[c(3,5,7)])[3]== 3){ # 오른쪽 대각선에 2(O)이 3개 있는 경우
    
    ws<- 2
  }
  
  if(ws==1){ # X가 승리한 경우
    
    if(ft==1){
      
      result<- 1 # 플레이어의 승리
    } else{
      
      result<- 2 # 컴퓨터의 승리
    }
  } else if(ws==2){ # O가 승리한 경우
    
    if(ft==1){
      
      result<- 2 # 컴퓨터의 승리
    } else{
      
      result<- 1 # 플레이어의 승리
    }
  }
  
  if(turn==9 & ws==0){ # 모든 칸에 표시가 있는 대도 승부가 나지 않는 경우
    
    result<- 3
  }
  
  return(result)
}

ft<- sample(1:2, 1) # 첫 번째 턴을 플레이어와 컴퓨터 중에서 누가 먼저할지 정하는 기능, ft: first turn, ft의 경우 1: 플레이어, 2: 컴퓨터

c_mt<- matrix(rep(0,9), nrow= 3) # 초반의 게임 상태를 설정해 줌(전부 0(빈칸))

show(c_mt,ft,1) # 게임 칸 순서 설명

Sys.sleep(4)

repeat{ # 게임 반복을 가능하게 만들어주는 기능
  
  show(c_mt,ft,2) # 게임 난이도 설정
  
  di<- as.integer(readline("난이도를 선택해 주세요(쉬움: 0, 보통: 1, 어려움: 2): "))
  
  while(di!=0 & di!=1 & di!=2){ # 게임 난이도를 잘못 입력한 경우
    
    di<- as.integer(readline("다시 선택해 주세요(쉬움: 0, 보통: 1, 어려움: 2): "))
    
    show(c_mt,ft,3) # 빈 판과 함께 다시 입력하라는 메시지를 출력해줌
  }
  
  show(c_mt,ft,4) # 게임 시작을 알려줌
  
  Sys.sleep(1.5)
  
  
  for(turn in 1:9){ # 승부가 날 때까지 게임을 반복해주는 기능
    
    options(warn= -1)
    
    if((ft==1 & turn%%2==1) | (ft==2 & turn%%2==0)){ # 플레이어의 차례의 경우
      
      show(c_mt,ft,0,turn)
      lc<- as.integer(readline("표시를 할 위치를 선택해주세요(1~9까지의 칸 중 빈칸 번호 입력 가능): ")) # location
      
      while(lc<1 | lc>9 | c(c_mt[1,], c_mt[2,], c_mt[3,])[lc]==1 | c(c_mt[1,], c_mt[2,], c_mt[3,])[lc]==2){ # 플레이어가 칸을 벗어나거나 이미 표시가 있는 곳에 표시를 하려는 경우
        
        lc<- as.integer(readline("위치를 다시 선택해주세요(1~9까지의 칸 중 빈칸 번호 입력 가능): "))
      }
      
      i<- ifelse(lc<=3, 1, ifelse(lc>=4 & lc<=6, 2, 3))
      j<- ifelse(lc%%3==1, 1, ifelse(lc%%3==2, 2, 3))
      
      c_mt[i,j]<- ifelse(ft==1, 1, 2) # 해당 위치에 플레이어가 처음 시작했으면 1(X)을 나중에 시작했으면 2(O)를 할당함
    } else{ # 컴퓨터의 차례인 경우
      
      done<- 0 # 컴퓨터가 표시를 하였는지의 여부를 0(표시 안함)으로 초기화 시켜주는 기능
      
      show(c_mt,ft,0,turn) # 컴퓨터가 표시를 하기 전의 판의 상황을 보여줌
      
      cs<- ifelse(ft==1, 2, 1) # 컴퓨터의 표시 모양 번호를 파악해주는 기능(1(X), 2(O)), cs: computer's shape, 참고로 ft가 1이면 cs는 2고 ft가 2면 cs는 1이다.
      
      
      if(di==0){ ## 난이도를 쉬움으로 설정한 경우, 참고로 nc(c_mt[1,][1])는 해당 서브세팅의 0의 개수, nc(c_mt[1,][2])는 1의 개수, nc(c_mt[1,][3])는 2의 개수이다.
        
        # 컴퓨터가 특정 위치에 안 두면 바로 패배하는 경우
        
        for(a in 1:3){
          
          if(nc(c_mt[a,])[ft+1]== 2 & nc(c_mt[a,])[1]== 1){ # 각 행에서 2가 2개이고 0이 하나인 경우
            
            c_mt[a,grep(0, c_mt[a,])]<- cs
            done<- 1
            
            break
          }
          
          if(nc(c_mt[,a])[ft+1]== 2 & nc(c_mt[,a])[1]== 1){ # 각 열에서 2가 2개이고 0이 하나인 경우
            
            c_mt[grep(0, c_mt[,a]),a]<- cs
            done<- 1
            
            break
          }
        }
        
        if(done==0 & nc(diag(c_mt))[ft+1]== 2 & nc(diag(c_mt))[1]== 1){ # 왼쪽 대각선에서 2가 2개이고 0이 하나인 경우
          
          c_mt[grep(0, diag(c_mt)), grep(0, diag(c_mt))]<- cs
          done<- 1
        }
        
        if(done==0 & nc(c(c_mt[1,], c_mt[2,], c_mt[3,])[c(3,5,7)])[ft+1]== 2 & nc(c(c_mt[1,], c_mt[2,], c_mt[3,])[c(3,5,7)])[1]== 1){ # 오른쪽 대각선에서 2가 2개이고 0이 하나인 경우
          
          if(c_mt[1,3]==0){
            
            c_mt[1,3]<- cs
          } else if(c_mt[2,2]==0){
            
            c_mt[2,2]<- cs
          } else{
            
            c_mt[3,1]<- cs
          }
          
          done<- 1
        }
        
        # 컴퓨터가 빈 자리에 아무 곳이나 두는 경우
        
        if(done==0){
          
          es<- which(as.numeric(c_mt)==0) # empty_space
          res<- es[sample(1:length(es), 1)]  # random_empty_space
          
          c_mt[res]<- cs
        }
        
        
      } else{  ## 난이도를 보통 또는 어려움으로 설정한 경우
        
        # 컴퓨터가 한 표시만 하면 바로 승리하는 경우  
        
        for(a in 1:3){
          
          if(nc(c_mt[a,])[cs+1]== 2 & nc(c_mt[a,])[1]== 1){ # 각 행에서 1이 2개이고 0이 하나인 경우
            
            c_mt[a,grep(0, c_mt[a,])]<- cs
            done<- 1
            
            break
          }
          
          if(nc(c_mt[,a])[cs+1]== 2 & nc(c_mt[,a])[1]== 1){ # 각 열에서 1이 2개이고 0이 하나인 경우
            
            c_mt[grep(0, c_mt[,a]),a]<- cs
            done<- 1
            
            break
          }
        }
        
        if(done==0 & nc(diag(c_mt))[cs+1]== 2 & nc(diag(c_mt))[1]== 1){ # 왼쪽 대각선에서 1이 2개이고 0이 하나인 경우
          
          c_mt[grep(0, diag(c_mt)), grep(0, diag(c_mt))]<- cs
          done<- 1
        }
        
        if(done==0 & nc(c(c_mt[1,], c_mt[2,], c_mt[3,])[c(3,5,7)])[cs+1]== 2 & nc(c(c_mt[1,], c_mt[2,], c_mt[3,])[c(3,5,7)])[1]== 1){ # 오른쪽 대각선에서 1이 2개이고 0이 하나인 경우
          
          if(c_mt[1,3]==0){
            
            c_mt[1,3]<- cs
          } else if(c_mt[2,2]==0){
            
            c_mt[2,2]<- cs
          } else{
            
            c_mt[3,1]<- cs
          }
          
          done<- 1
        }
        
        # 컴퓨터가 특정 위치에 안 두면 바로 패배하는 경우
        
        if(done==0){
          
          for(a in 1:3){
            
            if(nc(c_mt[a,])[ft+1]== 2 & nc(c_mt[a,])[1]== 1){ # 각 행에서 2가 2개이고 0이 하나인 경우
              
              c_mt[a,grep(0, c_mt[a,])]<- cs
              done<- 1
              
              break
            }
            
            if(nc(c_mt[,a])[ft+1]== 2 & nc(c_mt[,a])[1]== 1){ # 각 열에서 2가 2개이고 0이 하나인 경우
              
              c_mt[grep(0, c_mt[,a]),a]<- cs
              done<- 1
              
              break
            }
          }
          
          if(done==0 & nc(diag(c_mt))[ft+1]== 2 & nc(diag(c_mt))[1]== 1){ # 왼쪽 대각선에서 2가 2개이고 0이 하나인 경우
            
            c_mt[grep(0, diag(c_mt)), grep(0, diag(c_mt))]<- cs
            done<- 1
          }
          
          if(done==0 & nc(c(c_mt[1,], c_mt[2,], c_mt[3,])[c(3,5,7)])[ft+1]== 2 & nc(c(c_mt[1,], c_mt[2,], c_mt[3,])[c(3,5,7)])[1]== 1){ # 오른쪽 대각선에서 2가 2개이고 0이 하나인 경우
            
            if(c_mt[1,3]==0){
              
              c_mt[1,3]<- cs
            } else if(c_mt[2,2]==0){
              
              c_mt[2,2]<- cs
            } else{
              
              c_mt[3,1]<- cs
            }
            
            done<- 1
          }
        }
        
        if(di==2 & done==0){ ## 난이도를 어려움으로 설정한 경우
          
          if(turn==1){ # 전체 1턴에 컴퓨터가 1(X)인 경우
            
            c_mt[2,2]<- cs
            done<- 1
          } else if(turn==2){ # 전체 2턴에 컴퓨터가 2(O)인 경우
            
            if(c_mt[2,2]==0){
              
              c_mt[2,2]<- cs
              done<- 1
            } else{
              
              c_mt[sample(c(1,3), 1), sample(c(1,3), 1)]<- cs
              done<- 1
            }
          } else if(turn==3){ # 전체 3턴에 컴퓨터가 1(X)인 경우
            
            if(c_mt[1,2]==ft){
              
              c_mt[3, sample(c(1,3), 1)]<- cs
              done<- 1
            } else if(c_mt[2,1]== ft){
              
              c_mt[sample(c(1,3), 1), 3]<- cs
              done<- 1
            } else if(c_mt[3,2]== ft){
              
              c_mt[1, sample(c(1,3), 1)]<- cs
              done<- 1
            } else if(c_mt[2,3]== ft){
              
              c_mt[sample(c(1,3), 1), 1]<- cs
              done<- 1
            } else if(c_mt[1,1]== ft){
              
              c_mt[3,3]<- cs
              done<- 1
            } else if(c_mt[1,3]== ft){
              
              c_mt[3,1]<- cs
              done<- 1
            } else if(c_mt[3,1]== ft){
              
              c_mt[1,3]<- cs
              done<- 1
            } else if(c_mt[3,3]== ft){
              
              c_mt[1,1]<- cs
              done<- 1
            }
          } else if(turn==4){ # 전체 4턴에 컴퓨터가 2(O)인 경우
            
            if(all(diag(c_mt)== c(2,1,1)) | all(diag(c_mt)== c(1,1,2))){
              
              ra<- sample(1:2, 1) # random
              
              if(ra==1){
                
                c_mt[1,3]<- cs
              } else{
                
                c_mt[3,1]<- cs
              }
              
              done<- 1
            } else if(all(c(c_mt[3,1], c_mt[2,2], c_mt[1,3])== c(2,1,1)) | all(c(c_mt[3,1], c_mt[2,2], c_mt[1,3])== c(1,1,2))){
              
              ra<- sample(1:2, 1)
              
              if(ra==1){
                
                c_mt[1,1]<- cs
              } else{
                
                c_mt[3,3]<- cs
              }
              
              done<- 1
            } else if(all(diag(c_mt)== c(1,2,1)) | all(c(c_mt[3,1], c_mt[2,2], c_mt[1,3])== c(1,2,1))){
              
              ra<- sample(1:4, 1)
              
              if(ra==1){
                
                c_mt[1,2]<- cs
              } else if(ra==2){
                
                c_mt[2,1]<- cs
              } else if(ra==3){
                
                c_mt[2,3]<- cs
              } else{
                
                c_mt[3,2]<- cs
              } 
              
              done<- 1
            } else if(all(c(c_mt[1,2], c_mt[2,1], c_mt[2,2])== c(1,1,2))){
              
              ra<- sample(1:3, 1)
              
              if(ra==1){
                
                c_mt[1,1]<- cs
              } else if(ra==2){
                
                c_mt[1,3]<- cs
              } else{
                
                c_mt[3,1]<- cs
              }
              
              done<- 1
            } else if(all(c(c_mt[1,2], c_mt[2,3], c_mt[2,2])== c(1,1,2))){
              
              ra<- sample(1:3, 1)
              
              if(ra==1){
                
                c_mt[1,1]<- cs
              } else if(ra==2){
                
                c_mt[1,3]<- cs
              } else{
                
                c_mt[3,3]<- cs
              }
              
              done<- 1
            } else if(all(c(c_mt[2,1], c_mt[3,2], c_mt[2,2])== c(1,1,2))){
              
              ra<- sample(1:3, 1)
              
              if(ra==1){
                
                c_mt[1,1]<- cs
              } else if(ra==2){
                
                c_mt[3,1]<- cs
              } else{
                
                c_mt[3,3]<- cs
              }
              
              done<- 1
            } else if(all(c(c_mt[2,3], c_mt[3,2], c_mt[2,2])== c(1,1,2))){
              
              ra<- sample(1:3, 1)
              
              if(ra==1){
                
                c_mt[1,3]<- cs
              } else if(ra==2){
                
                c_mt[3,1]<- cs
              } else{
                
                c_mt[3,3]<- cs
              }
              
              done<- 1
            } else if(all(c_mt[2,]== c(1,2,1))){
              
              ra<- sample(1:2, 1)
              
              if(ra==1){
                
                c_mt[1,2]<- cs
              } else{
                
                c_mt[3,2]<- cs
              }
              
              done<- 1
            } else if(all(c_mt[,2]== c(1,2,1))){
              
              ra<- sample(1:2, 1)
              
              if(ra==1){
                
                c_mt[2,1]<- cs
              } else{
                
                c_mt[2,3]<- cs
              }
              
              done<- 1
            }
          } else if(turn==5){ # 전체 5턴에 컴퓨터가 1(X)인 경우
            
            if(all(diag(c_mt)== c(2,1,1)) & c_mt[2,3]==0){
              
              c_mt[2,3]<- cs
              done<- 1
            } else if(all(diag(c_mt)== c(2,1,1)) & c_mt[3,2]==0){
              
              c_mt[3,2]<- cs
              done<- 1
            } else if(all(diag(c_mt)== c(1,1,2)) & c_mt[1,2]==0){
              
              c_mt[1,2]<- cs
              done<- 1
            } else if(all(diag(c_mt)== c(1,1,2)) & c_mt[2,1]==0){
              
              c_mt[2,1]<- cs
              done<- 1
            } else if(all(c(c_mt[3,1], c_mt[2,2], c_mt[1,3])== c(2,1,1)) & c_mt[2,1]==0){
              
              c_mt[2,1]<- cs
              done<- 1
            } else if(all(c(c_mt[3,1], c_mt[2,2], c_mt[1,3])== c(2,1,1)) & c_mt[3,2]==0){
              
              c_mt[3,2]<- cs
              done<- 1
            } else if(all(c(c_mt[3,1], c_mt[2,2], c_mt[1,3])== c(1,1,2)) & c_mt[1,2]==0){
              
              c_mt[1,2]<- cs
              done<- 1
            } else if(all(c(c_mt[3,1], c_mt[2,2], c_mt[1,3])== c(1,1,2)) & c_mt[2,3]==0){
              
              c_mt[2,3]<- cs
              done<- 1
            }
          } else if(turn==6){ # 전체 6턴에 컴퓨터가 2(O)인 경우
            
            if(all(c(c_mt[1,2], c_mt[2,], c_mt[3,2])== c(1,2,2,1,1))){
              
              c_mt[sample(c(1,3), 1),1]<- cs
              done<- 1
            } else if(all(c(c_mt[1,2], c_mt[2,], c_mt[3,2])== c(2,1,2,1,1))){
              
              c_mt[1, sample(c(1,3),1)]<- cs
              done<- 1
            } else if(all(c(c_mt[1,2], c_mt[2,], c_mt[3,2])== c(1,1,2,2,1))){
              
              c_mt[sample(c(1,3), 1),3]<- cs
              done<- 1
            } else if(all(c(c_mt[1,2], c_mt[2,], c_mt[3,2])== c(1,1,2,1,2))){
              
              c_mt[3, sample(c(1,3),1)]<- cs
              done<- 1
            } 
          } else if(turn==7){ # 전체 7턴에 컴퓨터가 1(X)인 경우
            
            for(a in sample(1:4)){
              
              if(a==1 & c_mt[1,2]==0){
                
                c_mt[1,2]<- cs
                done<- 1
                break
              } else if(a==2 & c_mt[2,1]==0){
                
                c_mt[2,1]<- cs
                done<- 1
                break
              } else if(a==3 & c_mt[2,3]==0){
                
                c_mt[2,3]<- cs
                done<- 1
                break
              } else if(a==4 & c_mt[3,2]==0){
                
                c_mt[3,2]<- cs
                done<- 1
                break
              }
            }
          }
        }
        
        # 컴퓨터가 빈 자리에 아무 곳이나 두는 경우
        
        if(done==0){
          
          es<- which(as.numeric(c_mt)==0) # empty_space
          res<- es[sample(1:length(es), 1)]  # random_empty_space
          
          c_mt[res]<- cs
        }
      }
      
      Sys.sleep(2.5)
    }
    
    
    result<- checker(c_mt, ft, turn) # 0: 결과 아직 안났음, 1: 플레이어의 승리, 2: 컴퓨터의 승리, 3: 비김
    
    if(result> 0){
      
      if(result==1){
        
        print("플레이어가 승리하였습니다!")
        show(c_mt,ft,5)
      } else if(result==2){
        
        print("컴퓨터가 승리하였습니다!")
        show(c_mt,ft,6)
      } else{
        
        print("게임을 비겼습니다!")
        show(c_mt,ft,7)
      }
      
      Sys.sleep(3)
      
      break
    }
  }
  
  show(c_mt,ft,8) # 게임을 이어서 할지를 물어보는 기능
  
  re<- as.integer(readline("게임을 이어서 하시겠습니까? (예: 1, 아니오: 2): ")) # re game
  
  while(re!=1 & re!=2){ # 숫자를 잘못 입력한 경우
    
    re<- as.integer(readline("다시 선택해 주세요(예: 1, 아니오: 2): "))
    
    show(c_mt,ft,9) # 다시 입력하라는 메시지를 출력해줌
  }
  
  if(re==2){
    
    show(c_mt,ft,10) # 종료 메시지를 출력해줌
    
    break
  }
  
  c_mt<- matrix(rep(0,9), nrow= 3) # 초반의 게임 상태를 설정해 줌(전부 0(빈칸))
  
  if(ft==1){ # 게임 시작 순서를 바꿔주는 기능
    
    ft<- 2
  } else{
    
    ft<- 1
  }
}