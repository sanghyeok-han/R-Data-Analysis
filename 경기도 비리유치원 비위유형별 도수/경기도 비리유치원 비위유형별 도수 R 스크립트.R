data <- read.csv("경기도.csv")

str(data)

library(dplyr)

result_df <- data %>% group_by(비위유형) %>% count()

result_df <- data %>% group_by(비위유형) %>% count() %>% arrange(result_df$n)

result_df <- data %>% group_by(비위유형) %>% count()

result_df <- data %>% group_by(비위유형) %>% count() %>% arrange(result_df$n)


print(result_df)

print(result_df2)

is.data.frame(result_df)

rownames(result_df)=result_df$n

result_df2 = arrange(result_df$n)

data2 <- result_df[order(result_df$n)]
data2=as.data.frame(data2)

barplot(result_df$n, names.arg = result_df$비위유형, xlab="비위유형", ylab="도수", 
        main="경기도 비리유치원 비위유형별 도수", col = rev(heat.colors(length(result_df$n))),
        border = rev(heat.colors(length(result_df$n))), ylim=c(0,400))

function(x)rev(heat.colors(x)
