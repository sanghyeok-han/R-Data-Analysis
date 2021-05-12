library(RSelenium)
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

remDr<- remoteDriver(remoteServerAddr="localhost",
                     port=4445L,
                     browserName="chrome")

remDr$open()

remDr$navigate("https://www.career.go.kr/cnet/front/base/job/jobList.do#tab1")
Sys.sleep(0.5)

total_career_info_df<- NULL

for(i in 2:36){                    # 모든 직업 분류의 직업 정보 스크래핑
  
  page_source<- remDr$getPageSource()
  
  highest_category_name<- read_html(page_source[[1]]) %>% html_node(paste0("ul.srchItemList_re li:nth-child(", i, ") > label")) %>% html_text # 직업 분류 이름
    
  check_bt<- remDr$findElement(using= "css selector", paste0("ul.srchItemList_re li:nth-child(", i, ') > input[type="checkbox"]')) # 직업 분류 체크박스
  check_bt$clickElement() # 해당 분류 체크박스에 체크를 함
  
  Sys.sleep(0.5)
  search_bt<- remDr$findElement(using= "css selector", "p.btnWrap_c a") # 검색 버튼
  search_bt$clickElement()
  
  Sys.sleep(0.5)
  page_source<- remDr$getPageSource()
  
  page_number<- read_html(page_source[[1]]) %>% html_nodes("div.pagination") %>% html_text %>% str_trim %>% str_match_all("[0-9]+") %>% unlist %>%
                length # 페이지 쪽수
  
  for(j in 1:page_number){                           # 한 직업 분류의 직업 정보 스크래핑
    
    Sys.sleep(0.5)
    page_source<- remDr$getPageSource()
    
    career_name_number<- read_html(page_source[[1]]) %>% html_nodes("ul.search_result_list .result_list_head strong a") %>% html_text
    career_name_number<- Filter(function(x) {nchar(x)>=1}, career_name_number) %>% length
    
    career_name<- read_html(page_source[[1]]) %>% html_nodes("ul.search_result_list .result_list_head strong a") %>% html_text # 직업 이름
    career_name<- Filter(function(x) {nchar(x)>=1}, career_name)
    
    for(k in 1:career_name_number){  # 한 직업의 직업 정보 스크래핑
      
      career_info<- read_html(page_source[[1]]) %>% html_nodes(paste0("li:nth-child(", k, ") .result_list_head span")) %>% html_text
      
      if(any(grepl("연봉", career_info))==T){                    # 연봉이라는 단어가 있으면 연봉 관련 정보를 텍스트 마이닝해서 변수에 저장하는 기능
        career_salary<- career_info[grep("연봉", career_info)]
        career_salary<- gsub("연봉 : ", "", career_salary)
        career_salary<- gsub(" 만원", "만원", career_salary)
      } else{
        career_salary<- NA  # 연봉 관련 정보가 없으면 변수에 NA를 저장하는 기능
      }
      
      if(any(grepl("전망", career_info))==T){                    # 전망이라는 단어가 있으면 일자리전망 관련 정보를 텍스트 마이닝해서 변수에 저장하는 기능
        career_prospection<- career_info[grep("전망", career_info)]
        career_prospection<- gsub("일자리전망 : ", "", career_prospection)
      } else{
        career_prospection<- NA # 일자리전망 관련 정보가 없으면 변수에 NA를 저장하는 기능
      }
      
      if(any(grepl("발전", career_info))==T){                 # 발전이라는 단어가 있으면 발전가능성 관련 정보를 텍스트 마이닝해서 변수에 저장하는 기능
        development_possibility<- career_info[grep("발전", career_info)]
        development_possibility<- gsub("발전가능성 : ", "", development_possibility)
      } else{
        development_possibility<- NA   #  발전가능성 관련 정보가 없으면 변수에 NA를 저장하는 기능
      }
      
      if(any(grepl("평등", career_info))==T){                # 평등이라는 단어가 있으면 고용평등 관련 정보를 텍스트 마이닝해서 변수에 저장하는 기능
        employment_equity<- career_info[grep("평등", career_info)]
        employment_equity<- gsub("고용평등 : ", "", employment_equity)
      } else{
        employment_equity<- NA     # 고용평등 관련 정보가 없으면 변수에 NA를 저장하는 기능
      }
      
      career_info_df<- data.frame(직업_분류= highest_category_name, 직업_이름= career_name[k], 연봉= career_salary, 일자리전망= career_prospection,
                                  발전가능성= development_possibility, 고용평등= employment_equity)
      total_career_info_df<- rbind(total_career_info_df, career_info_df)
    }
    
    if(j<page_number){                             # 마지막 페이지가 아닌 경우 다음 페이지로 넘어가는 기능
      
      next_page_bt<- remDr$findElement(using= "css selector", paste0("div.pagination a:nth-child(", j+1, ")"))
      next_page_bt$clickElement()
    }
  }
  
  Sys.sleep(0.5)
  check_bt<- remDr$findElement(using= "css selector", paste0("ul.srchItemList_re li:nth-child(", i, ') > input[type="checkbox"]')) # 직업 분류 체크박스
  check_bt$clickElement() # 해당 분류 체크박스의 체크를 해제시킴
  
  print(i)  # 진행 확인용
}

View(total_career_info_df)

# write.csv(total_career_info_df, "커리어넷 직업 정보.csv", row.names= F)

total_career_info_df<- read.csv("커리어넷 직업 정보.csv", stringsAsFactors= T)


# 직업 분류별 연봉 분포 시각화

colnames(total_career_info_df)[3]<- "연봉(만원)"

sapply(total_career_info_df, class)

names(total_career_info_df)

salary_info<- total_career_info_df %>% select(1,2,3) %>% na.omit() %>% group_by(직업_분류, `연봉(만원)`) %>% count()

salary_ratio_info<- salary_info %>% spread(`연봉(만원)`, n) %>% replace(is.na(.), 0) %>%
                    mutate_at(.funs= funs(비율= ./(`2000 미만`+`2000 이상`+`3000 이상`+`4000 이상`)*100),
                              .vars= vars(`2000 미만`:`4000 이상`)) %>% select(1,6:9) %>%
                    arrange(desc(`4000 이상_비율`), desc(`3000 이상_비율`), desc(`2000 이상_비율`), desc(`2000 미만_비율`))

salary_info$직업_분류<- factor(salary_info$직업_분류, levels= rev(salary_ratio_info$직업_분류))

View(salary_ratio_info)
View(salary_info)

salary_bar<- ggplot(salary_info, aes(x= 직업_분류, y= n, fill= `연봉(만원)`))+ geom_bar(stat= "identity", position= "fill") + coord_flip() +
             ggtitle("커리어넷 직업 분류별 연봉대의 비율\n") +
             scale_x_discrete(name= "직업 분류") + scale_y_continuous(name= "연봉 비율") + scale_fill_discrete(name= "연봉(만원)") +
             theme(plot.title= element_text(face= "bold", size= 16, vjust= 2, hjust= 0.7), axis.title= element_text(face= "bold", size= 14))

salary_bar


# 직업 분류별 일자리전망 시각화


total_career_info_df$일자리전망<- factor(total_career_info_df$일자리전망, levels= c("보통미만", "보통이상", "좋음", "매우좋음"))

prospection_info<- total_career_info_df %>% select(1,2,4) %>% na.omit() %>% group_by(직업_분류, 일자리전망) %>% count()

prospection_ratio_info<- prospection_info %>% spread(일자리전망, n) %>% replace(is.na(.), 0) %>%
                         mutate_at(.funs= funs(비율= ./(매우좋음+좋음+보통이상+보통미만)*100),
                                   .vars= vars(매우좋음:보통미만)) %>% select(1,6:9) %>%
                         arrange(desc(매우좋음_비율), desc(좋음_비율), desc(보통이상_비율), desc(보통미만_비율))

prospection_info$직업_분류<- factor(prospection_info$직업_분류, levels= rev(prospection_ratio_info$직업_분류))

prospection_bar<- ggplot(prospection_info, aes(x= 직업_분류, y= n, fill= `일자리전망`))+ geom_bar(stat= "identity", position= "fill") + coord_flip() +
                  ggtitle("커리어넷 직업 분류별 일자리전망의 비율\n") +
                  scale_x_discrete(name= "직업 분류") + scale_y_continuous(name= "일자리전망 비율") + scale_fill_discrete(name= "일자리전망") +
                  theme(plot.title= element_text(face= "bold", size= 16, vjust= 2, hjust= 0.7 ), axis.title= element_text(face= "bold", size= 14))

prospection_bar


# 직업 분류별 발전가능성 시각화

total_career_info_df$발전가능성<- factor(total_career_info_df$발전가능성, levels= c("보통미만", "보통이상", "좋음", "매우좋음"))

development_info<- total_career_info_df %>% select(1,2,5) %>% na.omit() %>% group_by(직업_분류, 발전가능성) %>% count()

development_ratio_info<- development_info %>% spread(발전가능성, n) %>% replace(is.na(.), 0) %>%
                         mutate_at(.funs= funs(비율= ./(매우좋음+좋음+보통이상+보통미만)*100),
                                   .vars= vars(매우좋음:보통미만)) %>% select(1,6:9) %>%
                         arrange(desc(매우좋음_비율), desc(좋음_비율), desc(보통이상_비율), desc(보통미만_비율))

development_info$직업_분류<- factor(development_info$직업_분류, levels= rev(development_ratio_info$직업_분류))

development_bar<- ggplot(development_info, aes(x= 직업_분류, y= n, fill= `발전가능성`))+ geom_bar(stat= "identity", position= "fill") + coord_flip() +
                  ggtitle("커리어넷 직업 분류별 발전가능성의 비율\n") +
                  scale_x_discrete(name= "직업 분류") + scale_y_continuous(name= "발전가능성 비율") + scale_fill_discrete(name= "발전가능성") +
                  theme(plot.title= element_text(face= "bold", size= 16, vjust= 2, hjust= 0.7), axis.title= element_text(face= "bold", size= 14))

development_bar
