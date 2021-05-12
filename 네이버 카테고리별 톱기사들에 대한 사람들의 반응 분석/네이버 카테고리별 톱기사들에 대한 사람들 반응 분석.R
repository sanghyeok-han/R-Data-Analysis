library(RSelenium)
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(reshape2)

remDr<- remoteDriver(remoteServerAddr="localhost",
                     port=4445L,
                     browserName="chrome")

remDr$open()
remDr$navigate("https://news.naver.com/")

Sys.sleep(2)
page_source<- remDr$getPageSource()

catagory_name<- read_html(page_source[[1]]) %>% html_nodes("div.lnb_menu ul li") %>% html_text()
catagory_name<- catagory_name[3:8]

catagory_url_part<- read_html(page_source[[1]]) %>% html_nodes("div.lnb_menu ul li a") %>% html_attr("href")
catagory_url_part<- catagory_url_part[3:8]

catagory_url<- NULL

for(i in 1:length(catagory_url_part)){                   # URL을 접근 가능한 URL로 만들어 주는 기능
  
  catagory_url[i]<- paste0("https://news.naver.com", catagory_url_part[i])
}

total_article_info_df<- NULL

for(i in 1:length(catagory_url)){ # 페이지 정보를 분류별 기사별로 스크래핑하는 기능
  
  remDr$navigate(catagory_url[i])
  
  Sys.sleep(1)
  more_see_bt<- remDr$findElement(using="css selector", "div.cluster_more a")
  
  while(T){
    
    value<- tryCatch(
      {
        more_see_bt$clickElement()
      },
      error= function(e)
      {
        return (1)
      },
      finally= {}
    )
    
    if(is.numeric(value)==T){
      if(value==1){
        value<- 0
        break
      }
    }
    
    Sys.sleep(1)
  }
  
  Sys.sleep(1)
  page_source<- remDr$getPageSource()
  
  title<- read_html(page_source[[1]]) %>% html_nodes("a.cluster_text_headline") %>% html_text()
  title_url<- read_html(page_source[[1]]) %>% html_nodes("a.cluster_text_headline") %>% html_attr("href")
  
  for(j in 1:length(title_url)){
    
    remDr$navigate(title_url[j])
    
    Sys.sleep(1)
    page_source<- remDr$getPageSource()
    
    emotion_responses<- read_html(page_source[[1]]) %>% html_nodes("span.u_likeit_list_count") %>% html_text()
    emotion_responses<- emotion_responses[1:5]
    emotion_responses<- gsub(",", "", emotion_responses)
    emotion_responses<- as.integer(emotion_responses)
    
    article_info_df<- data.frame(카테고리= catagory_name[i], 제목= title[j], 좋아요= emotion_responses[1], 훈훈해요= emotion_responses[2],
                                 슬퍼요= emotion_responses[3], 화나요= emotion_responses[4], 후속기사_원해요= emotion_responses[5])
    
    total_article_info_df<- rbind(total_article_info_df, article_info_df)
  }
  
  print(i) # 진행 확인용
}

 write.csv(total_article_info_df, "네이버 카테고리별 톱기사들에 대한 사람들의 반응 분석 데이터_2019_8_06.csv", row.names= F)


# 데이터 전처리 과정


total_article_info_df<- read.csv("네이버 카테고리별 톱기사들에 대한 사람들의 반응 분석 데이터_2019_8_06.csv", stringsAsFactors= F)
# colnames(total_article_info_df)

grouped_total_article_info_df<- total_article_info_df %>% group_by(카테고리) %>% summarise(좋아요= sum(좋아요), 훈훈해요= sum(훈훈해요),
                                                                                           슬퍼요= sum(슬퍼요), 화나요= sum(화나요),
                                                                                           후속기사_원해요= sum(후속기사_원해요))

grouped_total_article_info_df<- grouped_total_article_info_df[c(6,2,3,4,5,1),]
grouped_total_article_info_df$카테고리 <- factor(grouped_total_article_info_df$카테고리, levels= grouped_total_article_info_df$카테고리)
#levels(grouped_total_article_info_df$카테고리)

melted_data<- melt(grouped_total_article_info_df)
colnames(melted_data)<- c("카테고리", "반응", "반응_수_합계")
View(total_article_info_df)

# 데이터 시각화


visualised_data<- ggplot(melted_data, aes(x= 카테고리, y= 반응_수_합계, fill= 반응)) +
                  geom_bar(stat= "identity", position= "dodge", color= "black") +
                  scale_fill_manual(values= c("#33FF00", "#FFFF00", "#3300FF", "#FF3300", "purple")) +
                  ggtitle("네이버 카테고리별 헤드라인 기사들에 \n대한 사람들의 반응 분석(2019-8-06)") +
                  theme(plot.title= element_text(face="bold", lineheight= 1, size=15, vjust=4, hjust=0.5))
visualised_data


#library(extrafont)
#font_import(pattern= "THE")
#loadfonts(device = "postscript")
