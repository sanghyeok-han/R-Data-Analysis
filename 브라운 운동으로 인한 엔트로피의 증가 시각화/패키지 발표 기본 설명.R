library(plot3D)

df<- data.frame(x =rep(10,20), y= seq(1, 20), z= seq(1,20))

# 기본 3D 산포도
scatter3D(df$x, df$y, df$z) 

# 인수 bty를 통한 격자 무늬 추가
scatter3D(df$x, df$y, df$z, bty= "b2")

# 인수 col을 통한 팔레트 변경
scatter3D(df$x,df$y,df$z, bty= "b2", col= ramp.col(c("red", "yellow", "blue")))

# ticktype를 통한 눈금 생성
scatter3D(df$x,df$y,df$z, bty= "b2", col= ramp.col(c("red", "yellow", "blue")), ticktype= "detailed")

# bty= "u"와 col.panel, col.grid를 통한 사용자 정의 배경 설정
scatter3D(df$x,df$y,df$z, bty= "u", col= ramp.col(c("red", "yellow", "blue")), ticktype= "detailed",
          col.panel="skyblue", col.grid= "blue")

# colkey를 통해서 범례의 위치를 옮김
scatter3D(df$x,df$y,df$z, bty= "u", col= ramp.col(c("red", "yellow", "blue")), ticktype= "detailed",
          col.panel="skyblue", col.grid= "blue", colkey = list(side = 1, length= 0.5))

# pch=19를 통한 점의 모양의 변경
scatter3D(df$x,df$y,df$z, bty= "u", pch= 19, col = ramp.col(c("red", "yellow", "blue")), ticktype = "detailed",
          col.panel="skyblue", col.grid= "blue", colkey = list(side = 1, length= 0.5))

# cex를 통한 점의 크기 변경
scatter3D(df$x,df$y,df$z, bty= "u", pch= 19, cex= 2, col = ramp.col(c("red", "yellow", "blue")), ticktype = "detailed",
          col.panel="skyblue", col.grid= "blue", colkey = list(side = 1, length= 0.5))

# pch=3을 통한 점의 모양 변경
scatter3D(df$x,df$y,df$z, bty= "u", pch= 3, cex= 2, col = ramp.col(c("red", "yellow", "blue")), ticktype = "detailed",
          col.panel="skyblue", col.grid= "blue", colkey = list(side = 1, length= 0.5))

# type= "h"를 통한 아래로 이어지는 선 추가
scatter3D(df$x,df$y,df$z, bty= "u", type= "h", pch= 3, cex= 2, col = ramp.col(c("red", "yellow", "blue")), ticktype = "detailed",
          col.panel="skyblue", col.grid= "blue", colkey = list(side = 1, length= 0.5))

# text3D 함수를 통한 텍스트 추가
text3D(df$x, df$y, df$z,  labels = rownames(df), add = TRUE, colkey = FALSE, cex = 0.8)

# theta와 phi를 통한 좌표 각도 변경 
scatter3D(df$x,df$y,df$z, bty= "u", type= "h", pch= 3, cex= 2, col = ramp.col(c("red", "yellow", "blue")), ticktype = "detailed",
          col.panel="skyblue", col.grid= "blue", colkey = list(side = 1, length= 0.5), theta = 10, phi = 10)



