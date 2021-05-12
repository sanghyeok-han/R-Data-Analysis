library(RSelenium)
library(rvest)
library(stringr)
library(dplyr) # bind_rows 기능 사용 가능
#library(ggplot2) # 시각화 기능 사용 가능
#library(reshape2) # 시각화를 위한 데이터 형태 변경 가능
library(corrplot) # 시각화 기능 사용 가능

options(scipen = 100)

remDr<- remoteDriver(remoteServerAddr="localhost",
                     port=4445L,
                     browserName="chrome")

# 국가별 GDP 첫 페이지 스크래핑

remDr$open()
remDr$navigate("https://www.naver.com")

searching_words<- remDr$findElement(using= "css selector", "span.green_window input")
searching_words$setElementAttribute("value", "세계 GDP")

searching_bt<- remDr$findElement(using= "id", "search_btn")
searching_bt$clickElement()

Sys.sleep(0.3)
page_source<- remDr$getPageSource()


nation_name<- remDr$findElements(using="css selector", ".nation_name a")
nation_name<- read_html(page_source[[1]]) %>% html_nodes("strong.nation_name a") %>% html_text()
nation_name

#nation_name<- remDr$findElements(using= "css selector", "strong.nation_name a")
#nation_name<- lapply(nation_name, function (x) {x$getElementText()})
#nation_name<- unlist(nation_name)
#nation_name

nation_GDP<- read_html(page_source[[1]]) %>% html_nodes(".figure_text") %>% html_text()
nation_GDP

# 국가별 GDP 전체 페이지 스크래핑

remDr$open()
remDr$navigate("https://www.naver.com")

searching_words<- remDr$findElement(using= "css selector", "span.green_window input")
searching_words$setElementAttribute("value", "세계 GDP")

searching_bt<- remDr$findElement(using= "id", "search_btn")
searching_bt$clickElement()

Sys.sleep(0.3)
page_source<- remDr$getPageSource()


total_page_number<- read_html(page_source[[1]]) %>% html_nodes("span.page_number ._total") %>% html_text()

total_nation_GDP_info<- NULL

for(i in 1:total_page_number){
  
  nation_name<- remDr$findElements(using="css selector", ".nation_name a")
  nation_name<- read_html(page_source[[1]]) %>% html_nodes("strong.nation_name a") %>% html_text()
  
  nation_GDP<- read_html(page_source[[1]]) %>% html_nodes(".figure_text") %>% html_text()
  
  nation_GDP_info<- data.frame(국가이름= nation_name, GDP= nation_GDP)
  
  total_nation_GDP_info<- rbind(total_nation_GDP_info, nation_GDP_info)
  
  if(i!= total_page_number){
    next_page_bt<- remDr$findElement(using= "css selector", "div.paging_bx a:nth-of-type(2)")
    next_page_bt$clickElement()
    
    Sys.sleep(0.3)
    page_source<- remDr$getPageSource()
  }
}

total_nation_GDP_info


# 모든 분야 전체 페이지 스크래핑

remDr$open()
remDr$navigate("https://www.naver.com")

searching_words<- remDr$findElement(using= "css selector", "span.green_window input") # 검색창 css 정보 가져오기
searching_words$setElementAttribute("value", "세계 GDP") # 검색창에 세계 GDP 입력함

searching_bt<- remDr$findElement(using= "id", "search_btn") # 검색 버튼 css 정보 가져오기
searching_bt$clickElement() # 검색 버튼 클릭함

Sys.sleep(0.3)
page_source<- remDr$getPageSource()

catagory_url_part<- read_html(page_source[[1]]) %>% html_nodes("ul.tab_list li a") %>% html_attr("href") # 각 카테고리의 url 정보를 가져옴
url_base<- "https://search.naver.com/search.naver"
catagory_url<- paste0(url_base, catagory_url_part) # 부분만 있는 url에 앞의 공통 부분을 붙여줌

catagory_preference<- c(3,4,2,1, 5:11) # 표시할 카테고리의 순서 # 데이터를 스크래핑 할 카테고리의 순서

total_nation_df<- NULL # 모든 카테고리의 정보를 담게 될 데이터 프레임
catagory_name<- NULL

for(i in catagory_preference){ # 카테고리별로 국가별 데이터를 스크래핑 한 다음 total_nation_df에다 정렬해서 합치는 기능 # catagory_preference
  
  print(i)
  remDr$navigate(catagory_url[i])
  Sys.sleep(0.3)
  page_source<- remDr$getPageSource()
  
  total_page_number<- read_html(page_source[[1]]) %>% html_nodes("span.page_number ._total") %>% html_text()
  
  catagory_name[i]<-  read_html(page_source[[1]]) %>% html_node("span.keyword input") %>% html_attr("value")
  
  total_nation_catagory_df<- NULL
  
  for(j in 1:total_page_number){ # 해당 카테고리의 국가별 데이터를 스크래핑하는 기능
    
    nation_name<- remDr$findElements(using="css selector", ".nation_name a")
    nation_name<- read_html(page_source[[1]]) %>% html_nodes("strong.nation_name a") %>% html_text()
    nation_name<- gsub("(\\(.*\\))", "", nation_name) %>% as.character()
    
    nation_catagory_info<- read_html(page_source[[1]]) %>% html_nodes(".figure_text") %>% html_text()
    nation_catagory_info<- gsub("[, ]","", nation_catagory_info)
    
    for(k in 1:length(nation_catagory_info)){ # 숫자 단위가 들어 있는 데이터들의 숫자 단위 값을 숫자로 전부 바꿔주는 기능
      
      p1_rest<- substring(nation_catagory_info[k], regexpr("조", nation_catagory_info[k]) + 1)
      p1_only<- gsub(p1_rest, "", nation_catagory_info[k])
      p1_only<- gsub("조", "", p1_only)
      p1_only<- as.integer(p1_only)
      
      p2_rest<- substring(p1_rest, regexpr("억", p1_rest) + 1)
      p2_only<- gsub(p2_rest, "", p1_rest)
      p2_only<- gsub("억", "", p2_only)
      p2_only<- as.integer(p2_only)
      
      p3_rest<- substring(p2_rest, regexpr("만", p2_rest) + 1)
      p3_only<- gsub(p3_rest, "", p2_rest)
      p3_only<- gsub("만", "", p3_only)
      
      if(length(grep("천" , p3_only))== T){ # p3_only가 "?천"일 경우
        
        p3_only<- gsub("천", "", p3_only)
        
        if(length(grep("[1-9]", p3_only))== 0){
          
          p3_only<- 1000
        } else{
          
          p3_only<- as.integer(p3_only)
          p3_only<- p3_only*1000
        }
      }
      
      p3_only<- as.integer(p3_only)
      
      p4_rest<- substring(p3_rest, regexpr("천", p3_rest) + 1)
      p4_only<- gsub(p4_rest, "", p3_rest)
      p4_only<- gsub("천", "", p4_only)
      p4_only<- as.integer(p4_only)
      
      p4_rest<- as.numeric(p4_rest)
      
      if(is.na(p1_only)== T){ # NA 값이 계산식에 있으면 전체 결과가 NA가 되므로 NA값이 있다면 0으로 바꿔줌
        p1_only<- 0}
      if(is.na(p2_only)== T){
        p2_only<- 0}
      if(is.na(p3_only)== T){
        p3_only<- 0}
      if(is.na(p4_only)== T){
        p4_only<- 0}
      if(is.na(p4_rest)== T){
        p4_rest<- 0}
      
      nation_catagory_info[k]<- (1000000000000*p1_only) + (100000000*p2_only) + (10000*p3_only) + (1000*p4_only) + p4_rest
    }

    nation_ranking<- read_html(page_source[[1]]) %>% html_nodes("span.rank_text") %>% html_text() %>% as.integer()
    
    nation_catagory_df<- data.frame(nation_name, nation_catagory_info, nation_ranking)
    
    total_nation_catagory_df<- rbind(total_nation_catagory_df, nation_catagory_df)
    
    if(j!= total_page_number){
      
      next_page_bt<- remDr$findElement(using= "css selector", "div.paging_bx a:nth-of-type(2)")
      next_page_bt$clickElement()
      
      Sys.sleep(0.3)
      page_source<- remDr$getPageSource()
    }
  }
  
  if(i== 3){ # 국가이름 순서의 기준이 되는 GDP 순서의 데이터를 맨 처음 스크래핑하며 해당 데이터를 total_nation_df에 저장시키는 기능
    
    total_nation_df<- total_nation_catagory_df
    colnames(total_nation_df)<- c("국가이름", catagory_name[i], paste0(catagory_name[i], "_순위"))
    
    total_nation_df$국가이름<- as.character(total_nation_df$국가이름)
    total_nation_df[, catagory_name[i]]<- as.numeric(as.character(total_nation_df[, catagory_name[i]]))
    total_nation_df[, paste0(catagory_name[i], "_순위")]<- as.integer(as.character(total_nation_df[, paste0(catagory_name[i], "_순위")]))
    
  } else{
    
    total_nation_catagory_df$nation_name<- as.character(total_nation_catagory_df$nation_name)
    total_nation_catagory_df$nation_catagory_info <- as.numeric(as.character(total_nation_catagory_df$nation_catagory_info))
    total_nation_catagory_df$nation_ranking<- as.integer(as.character(total_nation_catagory_df$nation_ranking))
    
    new_info<- data.frame(국가이름= setdiff(total_nation_catagory_df$nation_name, total_nation_df$국가이름))
    total_nation_df<- bind_rows(total_nation_df, new_info)
    
    new_info2<- data.frame(nation_name= setdiff(total_nation_df$국가이름, total_nation_catagory_df$nation_name))
    total_nation_catagory_df<- bind_rows(total_nation_catagory_df, new_info2)
    
    total_nation_catagory_df$nation_name<- factor(total_nation_catagory_df$nation_name, levels= total_nation_df$국가이름)
    total_nation_catagory_df<- total_nation_catagory_df[order(total_nation_catagory_df$nation_name),]
    
    total_nation_catagory_df<- total_nation_catagory_df[,-1] # 국가 이름 열은 겹치므로 제거함
    
    colnames(total_nation_catagory_df)<- c(catagory_name[i], paste0(catagory_name[i], "_순위"))
    total_nation_df<- cbind(total_nation_df, total_nation_catagory_df) # total_nation_df에 total_nation_catagory_df의 정보를 합침(column)
  }
}

sapply(total_nation_df, class)
#sapply(total_nation_catagory_df, class)

#write.csv(total_nation_df, "네이버 국가별 정보 데이터2.csv", row.names= F)
total_nation_df<- read.csv("네이버 국가별 정보 데이터.csv", stringsAsFactors= F)
#View(total_nation_df)


# 모든 분야 전체 페이지 스크래핑에 이어서 각 변수 사이의 상관계수 구하기

colnames(total_nation_df)

all_catagory<- total_nation_df[, c(2,4,6,8,10,12,14,16,18,20,22)] # 각 카테고리의 실질적 정보를 포함한 열만 추출함
#colnames(all_catagory)

some_catagory<- all_catagory[, -c(6,8,9,10,11)] # 결측치가 많은 열들을 제거함
colnames(some_catagory)[2]<- '일인당국내총생산'
#colnames(some_catagory)


#cor_value<- cor(some_catagory$국내총생산, some_catagory$인구, use='complete.obs', method='pearson')
cor_matrix<- cor(some_catagory, use='complete.obs', method='pearson') # 상관계수 표
cor_matrix<- round(cor_matrix, 2)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
cor_matrix_plot<- corrplot(cor_matrix, method= "color", col= col(70), type= "upper", order= "hclust", tl.col= "black", addCoef.col= "black",
                           tl.srt=60, tl.cex=1, insig = "blank", diag=F)

#cor_matrix[upper.tri(cor_matrix)]<- NA

#melted_cor_matrix<- melt(cor_matrix, na.rm= T)


if(FALSE){ # 주석과 같은 기능

cor_matrix_plot<- ggplot(data= melted_cor_matrix, aes(x= Var1, y= Var2, fill= value)) + geom_tile(color= "white") +
                  scale_fill_gradient2(low= "red", high= "blue", mid= "white", midpoint= 0, limit= c(-1,1), space= "Lab", name= "Correlation") +
                  theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed() +
                  geom_text(aes(Var2, Var1, label= value), color= "black", size= 4) +
                  theme(axis.title.x= element_blank(), axis.title.y= element_blank(), panel.grid.major= element_blank(), panel.border= element_blank(),
                  panel.background= element_blank(), axis.ticks= element_blank(), legend.justification= c(1,0), legend.position= c(0.6, 0.7),
                  legend.direction= "horizontal") +
                  guides(fill= guide_colorbar(barwidth= 7, barheight= 1, title.position= "top", title.hjust= 0.5))

cor_matrix_plot

}

# 네이버 국가 정보에 다른 국가 정보들을 합치기

total_nation_df<- read.csv("네이버 국가별 정보 데이터.csv", stringsAsFactors= F)

# 언론자유지수 데이터 추가

remDr$open()
remDr$navigate("https://rsf.org/en/ranking#") # 언론 자유 지수 사이트
Sys.sleep(3)
page_source<- remDr$getPageSource()

pfi_name<- read_html(page_source[[1]]) %>% html_nodes(".ranking-map__panel-name") %>% html_text() # press_freedom_index
pfi_value<- read_html(page_source[[1]]) %>% html_nodes(".ranking-map__panel-score") %>% html_text() %>% as.numeric()
pfi_ranking<- 1:length(pfi_value)

pfi_korean_name<- NULL
1+1
for(i in 1:length(pfi_name)){
  
  remDr$navigate("https://translate.google.co.kr/?hl=ko&tab=TT")
  Sys.sleep(1)
  page_source<- remDr$getPageSource()
  
  word_to_translate_place<- remDr$findElement(using= "id", "source")
  word_to_translate_place$sendKeysToElement(list(pfi_name[i]))
  
  Sys.sleep(1.5)
  page_source<- remDr$getPageSource()
  
  pfi_korean_name[i]<- read_html(page_source[[1]]) %>% html_nodes("span.translation span") %>% html_text()
  
  print(i)
}

pfi_korean_name2<- gsub(" ","", pfi_korean_name)
pfi_korean_name2<- gsub("-","", pfi_korean_name2)
pfi_korean_name2<- gsub("아랍공화국","", pfi_korean_name2)
pfi_korean_name2<- gsub("제도","", pfi_korean_name2)
pfi_korean_name2<- gsub("/서사하라"," ", pfi_korean_name2)
pfi_korean_name2<- gsub("제국","", pfi_korean_name2)
pfi_korean_name2<- gsub("의상태","", pfi_korean_name2)
pfi_korean_name2<- gsub("아이보리코스트", "아이보리해안", pfi_korean_name2)
pfi_korean_name2<- gsub("대한민국", "한국", pfi_korean_name2)
pfi_korean_name2<- gsub("몰다비아", "몰도바", pfi_korean_name2)
pfi_korean_name2<- gsub("몽골리아", "몽골", pfi_korean_name2)
pfi_korean_name2<- gsub("호주", "오스트레일리아", pfi_korean_name2)

pfi_df<- data.frame(nation_name= pfi_korean_name2, nation_catagory_info= pfi_value, nation_ranking= pfi_ranking)

#sapply(pfi_df, class)

pfi_df$nation_name<- as.character(pfi_df$nation_name)

new_info<- data.frame(국가이름= setdiff(pfi_df$nation_name, total_nation_df$국가이름))
total_nation_df<- bind_rows(total_nation_df, new_info)

new_info2<- data.frame(nation_name= setdiff(total_nation_df$국가이름, pfi_df$nation_name))
pfi_df<- bind_rows(pfi_df, new_info2)

pfi_df$nation_name<- factor(pfi_df$nation_name, levels= total_nation_df$국가이름)
pfi_df<- pfi_df[order(pfi_df$nation_name),]

pfi_df<- pfi_df[,-1] # 국가 이름 열은 겹치므로 제거함

colnames(pfi_df)<- c("언론자유지수", "언론자유지수_순위")
total_nation_df<- cbind(total_nation_df, pfi_df) # total_nation_df에 pfi_df의 정보를 합침(column)

# 행복지수 데이터 추가

remDr$navigate("https://countryeconomy.com/demography/world-happiness-index") # 세계 행복 지수 사이트
Sys.sleep(3)
page_source<- remDr$getPageSource()

whi_name<- read_html(page_source[[1]]) %>% html_nodes("#tb1 a") %>% html_text() # world happiness index
whi_name<- gsub(" [+]", "", whi_name, fixed= T)

whi_value<- read_html(page_source[[1]]) %>% html_nodes(".numero+ .numero") %>% html_text() %>% as.numeric()

whi_ranking<- read_html(page_source[[1]]) %>% html_nodes(".numero:nth-child(2)") %>% html_text()
whi_ranking<- gsub("º", "", whi_ranking)
whi_ranking<- as.integer(whi_ranking)

whi_korean_name<- NULL
1+1
for(i in 1:length(whi_name)){
  
  remDr$navigate("https://translate.google.co.kr/?hl=ko&tab=TT")
  Sys.sleep(1)
  page_source<- remDr$getPageSource()
  
  word_to_translate_place<- remDr$findElement(using= "id", "source")
  word_to_translate_place$sendKeysToElement(list(whi_name[i]))
  
  Sys.sleep(1.5)
  page_source<- remDr$getPageSource()
  
  whi_korean_name[i]<- read_html(page_source[[1]]) %>% html_nodes("span.translation span") %>% html_text()
  
  print(i)
}

whi_korean_name2<- gsub(" ","", whi_korean_name)
whi_korean_name2<- gsub("-","", whi_korean_name2)
whi_korean_name2<- gsub("아랍공화국","", whi_korean_name2)
whi_korean_name2<- gsub("제도","", whi_korean_name2)
whi_korean_name2<- gsub("버마","", whi_korean_name2)
whi_korean_name2<- gsub("체코공화국","체코", whi_korean_name2)
whi_korean_name2<- gsub("마케도니아공화국","마케도니아", whi_korean_name2)
whi_korean_name2<- gsub("콩고공화국","콩고", whi_korean_name2)
whi_korean_name2<- gsub("제국","", whi_korean_name2)
whi_korean_name2<- gsub("의상태","", whi_korean_name2)
whi_korean_name2<- gsub("아이보리코스트","아이보리해안", whi_korean_name2)
whi_korean_name2<- gsub("대한민국", "한국", whi_korean_name2)
whi_korean_name2<- gsub("몰다비아", "몰도바", whi_korean_name2)
whi_korean_name2<- gsub("몽골리아", "몽골", whi_korean_name2)
whi_korean_name2<- gsub("호주", "오스트레일리아", whi_korean_name2)

whi_df<- data.frame(nation_name= whi_korean_name2, nation_catagory_info= whi_value, nation_ranking= whi_ranking)

#sapply(whi_df, class)

whi_df$nation_name<- as.character(whi_df$nation_name)

new_info<- data.frame(국가이름= setdiff(whi_df$nation_name, total_nation_df$국가이름))
total_nation_df<- bind_rows(total_nation_df, new_info)

new_info2<- data.frame(nation_name= setdiff(total_nation_df$국가이름, whi_df$nation_name))
whi_df<- bind_rows(whi_df, new_info2)

whi_df$nation_name<- factor(whi_df$nation_name, levels= total_nation_df$국가이름)
whi_df<- whi_df[order(whi_df$nation_name),]

whi_df<- whi_df[,-1] # 국가 이름 열은 겹치므로 제거함

colnames(whi_df)<- c("행복지수", "행복지수_순위")
total_nation_df<- cbind(total_nation_df, whi_df) # total_nation_df에 whi_df의 정보를 합침(column)
colnames(total_nation_df)

# 자살률 데이터 추가

remDr$navigate("http://worldpopulationreview.com/countries/suicide-rate-by-country/") # 국가별 자살률 정보 사이트
Sys.sleep(3)
page_source<- remDr$getPageSource()

sr_name<- read_html(page_source[[1]]) %>% html_nodes("tbody a") %>% html_text() # suicide_rate

sr_value<- read_html(page_source[[1]]) %>% html_nodes("td:nth-child(2)") %>% html_text()
sr_value<- gsub(" suicides per 100k", "", sr_value)
sr_value<- gsub(" ", "", sr_value)
sr_value<- sr_value[-1]
sr_value<- as.numeric(sr_value)

sr_ranking<- read_html(page_source[[1]]) %>% html_nodes("tbody td~ td+ td") %>% html_text() %>% as.integer()

length(sr_value)

sr_korean_name<- NULL

for(i in 1:length(sr_name)){
  
  remDr$navigate("https://translate.google.co.kr/?hl=ko&tab=TT")
  Sys.sleep(1)
  page_source<- remDr$getPageSource()
  
  word_to_translate_place<- remDr$findElement(using= "id", "source")
  word_to_translate_place$sendKeysToElement(list(sr_name[i]))
  
  Sys.sleep(1.5)
  page_source<- remDr$getPageSource()
  
  sr_korean_name[i]<- read_html(page_source[[1]]) %>% html_nodes("span.translation span") %>% html_text()
  
  print(i)
}

sr_korean_name2<- gsub(" ","", sr_korean_name)
sr_korean_name2<- gsub("-","", sr_korean_name2)
sr_korean_name2<- gsub("아랍공화국","", sr_korean_name2)
sr_korean_name2<- gsub("제도","", sr_korean_name2)
sr_korean_name2<- gsub("버마","", sr_korean_name2)
sr_korean_name2<- gsub("체코공화국","체코", sr_korean_name2)
sr_korean_name2<- gsub("마케도니아공화국","마케도니아", sr_korean_name2)
sr_korean_name2<- gsub("콩고공화국","콩고", sr_korean_name2)
sr_korean_name2<- gsub("제국","", sr_korean_name2)
sr_korean_name2<- gsub("의상태","", sr_korean_name2)
sr_korean_name2<- gsub("아이보리코스트","아이보리해안", sr_korean_name2)
sr_korean_name2<- gsub("콩고박사","콩고민주공화국", sr_korean_name2)
sr_korean_name2<- gsub("대한민국", "한국", sr_korean_name2)
sr_korean_name2<- gsub("몰다비아", "몰도바", sr_korean_name2)
sr_korean_name2<- gsub("몽골리아", "몽골", sr_korean_name2)
sr_korean_name2<- gsub("호주", "오스트레일리아", sr_korean_name2)

sr_df<- data.frame(nation_name= sr_korean_name2, nation_catagory_info= sr_value, nation_ranking= sr_ranking)

sapply(sr_df, class)

sr_df$nation_name<- as.character(sr_df$nation_name)

new_info<- data.frame(국가이름= setdiff(sr_df$nation_name, total_nation_df$국가이름))
total_nation_df<- bind_rows(total_nation_df, new_info)

new_info2<- data.frame(nation_name= setdiff(total_nation_df$국가이름, sr_df$nation_name))
sr_df<- bind_rows(sr_df, new_info2)

sr_df$nation_name<- factor(sr_df$nation_name, levels= total_nation_df$국가이름)
sr_df<- sr_df[order(sr_df$nation_name),]

sr_df<- sr_df[,-1] # 국가 이름 열은 겹치므로 제거함

colnames(sr_df)<- c("자살률", "자살률_순위")
total_nation_df<- cbind(total_nation_df, sr_df) # total_nation_df에 sr_df의 정보를 합침(column)
colnames(total_nation_df)


# write.csv(total_nation_df, "국가별 정보 데이터.csv", row.names= F)


# 1인당 국내총생산이 우리나라보다 높은 국가들의 인구 합

total_nation_df<- read.csv("국가별 정보 데이터.csv", stringsAsFactor= F)

colnames(total_nation_df)
some_catagory<- total_nation_df[, c("국가이름", "X1인당국내총생산", "국내총생산", "인구")]
#colnames(total_nation_df)
#colnames(some_catagory)

some_catagory_ordered<- some_catagory[order(some_catagory$X1인당국내총생산, decreasing= T),]

grep("한국", some_catagory_ordered$국가이름)

high_income_population<- 0

for(i in 1:(grep("한국", some_catagory_ordered$국가이름)-1)){
  high_income_population<- high_income_population + some_catagory_ordered$인구[i]
}


# 국내총생산이 우리나라보다 높은 국가들의 인구 합

some_catagory_ordered<- some_catagory[order(some_catagory$국내총생산, decreasing= T),]
View(some_catagory_ordered)
grep("한국", some_catagory_ordered$국가이름)

high_income_population2<- 0

for(i in 1:(grep("한국", some_catagory_ordered$국가이름)-1)){
  high_income_population2<- high_income_population2 + some_catagory_ordered$인구[i]
}


# 기대 수명 정보 추가

total_nation_df<- read.csv("국가별 정보 데이터.csv", stringsAsFactor= F)

remDr$open()

total_nation_name<- NULL
total_value<- NULL

for(i in 1:3){
  
  print(i)
  
  remDr$navigate(paste0("http://www.geoba.se/population.php?pc=world&type=15&year=2018&st=rank&asde=&page=", i)) # 국가별 기대수명 사이트
  
  Sys.sleep(3)
  page_source<- remDr$getPageSource()
  
  nation_name<- read_html(page_source[[1]]) %>% html_nodes(".bb .redglow") %>% html_text()
  
  value<- read_html(page_source[[1]]) %>% html_nodes(".bb td:nth-child(3)") %>% html_text()
  
  total_nation_name<- c(total_nation_name, nation_name)
  total_value<- c(total_value, value)
}

ranking<- 1:length(total_nation_name)

korean_name<- NULL

for(i in 1:length(total_nation_name)){
  
  remDr$navigate("https://translate.google.co.kr/?hl=ko&tab=TT")
  Sys.sleep(1)
  page_source<- remDr$getPageSource()
  
  word_to_translate_place<- remDr$findElement(using= "id", "source")
  word_to_translate_place$sendKeysToElement(list(total_nation_name[i]))
  
  Sys.sleep(1.5)
  page_source<- remDr$getPageSource()
  
  korean_name[i]<- read_html(page_source[[1]]) %>% html_nodes("span.translation span") %>% html_text()
  
  print(i)
}

korean_name2<- gsub(" ","", korean_name)
korean_name2<- gsub("-","", korean_name2)
korean_name2<- gsub("아랍공화국","", korean_name2)
korean_name2<- gsub("제도","", korean_name2)
korean_name2<- gsub("버마","", korean_name2)
korean_name2<- gsub("체코공화국","체코", korean_name2)
korean_name2<- gsub("마케도니아공화국","마케도니아", korean_name2)
korean_name2<- gsub("콩고공화국","콩고", korean_name2)
korean_name2<- gsub("제국","", korean_name2)
korean_name2<- gsub("의상태","", korean_name2)
korean_name2<- gsub("아이보리코스트","아이보리해안", korean_name2)
korean_name2<- gsub("콩고박사","콩고민주공화국", korean_name2)
korean_name2<- gsub("대한민국", "한국", korean_name2)
korean_name2<- gsub("몰다비아", "몰도바", korean_name2)
korean_name2<- gsub("아일오브맨", "맨섬", korean_name2)
korean_name2<- gsub("궁금", "마요트섬", korean_name2)
korean_name2<- gsub("아메리카합중국", "미국", korean_name2)
korean_name2<- gsub("페로", "페로제도", korean_name2)
korean_name2<- gsub("네덜란드령앤틸리스", "앤틸리스", korean_name2)
korean_name2<- gsub("프랑스령폴리네시아의", "프랑스령폴리네시아", korean_name2)
korean_name2<- gsub("몽골리아", "몽골", korean_name2)
korean_name2<- gsub("호주", "오스트레일리아", korean_name2)

nation_info_df<- data.frame(nation_name= korean_name2, nation_catagory_info= total_value, nation_ranking= ranking)

sapply(nation_info_df, class)

nation_info_df$nation_name<- as.character(nation_info_df$nation_name)

new_info<- data.frame(국가이름= setdiff(nation_info_df$nation_name, total_nation_df$국가이름))
total_nation_df<- bind_rows(total_nation_df, new_info)

new_info2<- data.frame(nation_name= setdiff(total_nation_df$국가이름, nation_info_df$nation_name))
nation_info_df<- bind_rows(nation_info_df, new_info2)

nation_info_df$nation_name<- factor(nation_info_df$nation_name, levels= total_nation_df$국가이름)
nation_info_df<- nation_info_df[order(nation_info_df$nation_name),]

nation_info_df<- nation_info_df[,-1] # 국가 이름 열은 겹치므로 제거함

colnames(nation_info_df)<- c("기대수명", "기대수명_순위")
total_nation_df<- cbind(total_nation_df, nation_info_df) # total_nation_df에 nation_info_df의 정보를 합침(column)
colnames(total_nation_df)

write.csv(total_nation_df, "국가별 정보 데이터2.csv", row.names= F)

# 부패인식지수 데이터 추가

nation_catagory_info_df<- read.csv("부패인식지수.csv", stringsAsFactor= F) # https://www.transparency.org/cpi2018?gclid=CjwKCAiAkrTjBRAoEiwAXpf9CX523Ko9J1TzE3T8V1ARuGUUVEZ9nSVewCyRXRCH_ba_gYienmouGhoCeH8QAvD_BwE 사이트의 엑셀 파일

nation_name<- nation_catagory_info_df$nation_name

ranking<- 1:length(nation_catagory_info_df$nation_name)

korean_name<- NULL

for(i in 1:length(nation_name)){
  
  remDr$navigate("https://translate.google.co.kr/?hl=ko&tab=TT")
  Sys.sleep(1)
  page_source<- remDr$getPageSource()
  
  word_to_translate_place<- remDr$findElement(using= "id", "source")
  word_to_translate_place$sendKeysToElement(list(nation_name[i]))
  
  Sys.sleep(1.5)
  page_source<- remDr$getPageSource()
  
  korean_name[i]<- read_html(page_source[[1]]) %>% html_nodes("span.translation span") %>% html_text()
  
  print(i)
}

korean_name2<- gsub(" ","", korean_name)
korean_name2<- gsub("-","", korean_name2)
korean_name2<- gsub("아랍공화국","", korean_name2)
korean_name2<- gsub("제도","", korean_name2)
korean_name2<- gsub("버마","", korean_name2)
korean_name2<- gsub("체코공화국","체코", korean_name2)
korean_name2<- gsub("마케도니아공화국","마케도니아", korean_name2)
korean_name2<- gsub("콩고공화국","콩고", korean_name2)
korean_name2<- gsub("제국","", korean_name2)
korean_name2<- gsub("의상태","", korean_name2)
korean_name2<- gsub("아이보리코스트","아이보리해안", korean_name2)
korean_name2<- gsub("콩고박사","콩고민주공화국", korean_name2)
korean_name2<- gsub("대한민국", "한국", korean_name2)
korean_name2<- gsub("몰다비아", "몰도바", korean_name2)
korean_name2<- gsub("아일오브맨", "맨섬", korean_name2)
korean_name2<- gsub("궁금", "마요트섬", korean_name2)
korean_name2<- gsub("아메리카합중국", "미국", korean_name2)
korean_name2<- gsub("페로", "페로제도", korean_name2)
korean_name2<- gsub("네덜란드령앤틸리스", "앤틸리스", korean_name2)
korean_name2<- gsub("프랑스령폴리네시아의", "프랑스령폴리네시아", korean_name2)
korean_name2<- gsub("몽골리아", "몽골", korean_name2)
korean_name2<- gsub("호주", "오스트레일리아", korean_name2)

nation_catagory_info_df<- data.frame(nation_name= korean_name2, nation_catagory_info_df$value, nation_ranking= ranking)

sapply(nation_catagory_info_df, class)

nation_catagory_info_df$nation_name<- as.character(nation_catagory_info_df$nation_name)

new_info<- data.frame(국가이름= setdiff(nation_catagory_info_df$nation_name, total_nation_df$국가이름))
total_nation_df<- bind_rows(total_nation_df, new_info)

new_info2<- data.frame(nation_name= setdiff(total_nation_df$국가이름, nation_catagory_info_df$nation_name))
nation_catagory_info_df<- bind_rows(nation_catagory_info_df, new_info2)

nation_catagory_info_df$nation_name<- factor(nation_catagory_info_df$nation_name, levels= total_nation_df$국가이름)
nation_catagory_info_df<- nation_catagory_info_df[order(nation_catagory_info_df$nation_name),]

nation_catagory_info_df<- nation_catagory_info_df[,-1] # 국가 이름 열은 겹치므로 제거함

colnames(nation_catagory_info_df)<- c("부패인식지수", "부패인식지수_순위")
total_nation_df<- cbind(total_nation_df, nation_catagory_info_df) # total_nation_df에 nation_catagory_info_df의 정보를 합침(column)
colnames(total_nation_df)

write.csv(total_nation_df, "국가별 정보 데이터3.csv", row.names= F)


# 순소득 지니계수, 재산 지니계수 데이터 추가

total_nation_df<- read.csv("국가별 정보 데이터.csv", stringsAsFactor= F)

remDr$open()
remDr$navigate("https://www.gfmag.com/global-data/economic-data/wealth-distribution-income-inequality") # 국가별 지니계수 사이트

Sys.sleep(3)
page_source<- remDr$getPageSource()

nation_name<- read_html(page_source[[1]]) %>% html_nodes("tr td:nth-child(1) p") %>% html_text()

value1<- read_html(page_source[[1]]) %>% html_nodes("tr td:nth-child(2) p") %>% html_text()

value2<-read_html(page_source[[1]]) %>% html_nodes("tr td:nth-child(3) p") %>% html_text()
value2<- gsub("N/A", NA, value2)

korean_name<- NULL

for(i in 1:length(nation_name)){
  
  remDr$navigate("https://translate.google.co.kr/?hl=ko&tab=TT")
  Sys.sleep(1)
  page_source<- remDr$getPageSource()
  
  word_to_translate_place<- remDr$findElement(using= "id", "source")
  word_to_translate_place$sendKeysToElement(list(nation_name[i]))
  
  Sys.sleep(1.5)
  page_source<- remDr$getPageSource()
  
  korean_name[i]<- read_html(page_source[[1]]) %>% html_nodes("span.translation span") %>% html_text()
  
  print(i)
}

korean_name2<- gsub(" ","", korean_name)
korean_name2<- gsub("-","", korean_name2)
korean_name2<- gsub("아랍공화국","", korean_name2)
korean_name2<- gsub("제도","", korean_name2)
korean_name2<- gsub("버마","", korean_name2)
korean_name2<- gsub("체코공화국","체코", korean_name2)
korean_name2<- gsub("마케도니아공화국","마케도니아", korean_name2)
korean_name2<- gsub("콩고공화국","콩고", korean_name2)
korean_name2<- gsub("제국","", korean_name2)
korean_name2<- gsub("의상태","", korean_name2)
korean_name2<- gsub("아이보리코스트","아이보리해안", korean_name2)
korean_name2<- gsub("콩고박사","콩고민주공화국", korean_name2)
korean_name2<- gsub("대한민국", "한국", korean_name2)
korean_name2<- gsub("몰다비아", "몰도바", korean_name2)
korean_name2<- gsub("아일오브맨", "맨섬", korean_name2)
korean_name2<- gsub("궁금", "마요트섬", korean_name2)
korean_name2<- gsub("아메리카합중국", "미국", korean_name2)
korean_name2<- gsub("페로", "페로제도", korean_name2)
korean_name2<- gsub("네덜란드령앤틸리스", "앤틸리스", korean_name2)
korean_name2<- gsub("프랑스령폴리네시아의", "프랑스령폴리네시아", korean_name2)
korean_name2<- gsub("몽골리아", "몽골", korean_name2)
korean_name2<- gsub("호주", "오스트레일리아", korean_name2)
korean_name2<- gsub("라오스PDR", "라오스", korean_name2)
korean_name2<- gsub("슬로바키아공화국", "슬로바키아", korean_name2)
korean_name2<- gsub("이란,이슬람공화국", "이란", korean_name2)
korean_name2<- gsub("러시아연방", "러시아", korean_name2)
korean_name2<- gsub("이란,이슬람공화국", "이란", korean_name2)
korean_name2<- gsub("키르기스공화국", "키르기스스탄", korean_name2)

nation_catagory_info_df<- data.frame(nation_name= korean_name2, nation_catagory_info1= value1, nation_catagory_info2= value2)

sapply(nation_catagory_info_df, class)

nation_catagory_info_df$nation_name<- as.character(nation_catagory_info_df$nation_name)

new_info<- data.frame(국가이름= setdiff(nation_catagory_info_df$nation_name, total_nation_df$국가이름))
total_nation_df<- bind_rows(total_nation_df, new_info)

new_info2<- data.frame(nation_name= setdiff(total_nation_df$국가이름, nation_catagory_info_df$nation_name))
nation_catagory_info_df<- bind_rows(nation_catagory_info_df, new_info2)

nation_catagory_info_df$nation_name<- factor(nation_catagory_info_df$nation_name, levels= total_nation_df$국가이름)
nation_catagory_info_df<- nation_catagory_info_df[order(nation_catagory_info_df$nation_name),]

nation_catagory_info_df<- nation_catagory_info_df[,-1] # 국가 이름 열은 겹치므로 제거함

colnames(nation_catagory_info_df)<- c("순소득지니계수", "재산지니계수")
total_nation_df<- cbind(total_nation_df, nation_catagory_info_df) # total_nation_df에 nation_catagory_info_df의 정보를 합침(column)
colnames(total_nation_df)

write.csv(total_nation_df, "국가별 정보 데이터4.csv", row.names= F)


# 합계출산율 데이터 추가

remDr$open()
remDr$navigate("https://data.worldbank.org/indicator/sp.dyn.tfrt.in") # 국가별 합계출산율 사이트

Sys.sleep(3)
page_source<- remDr$getPageSource()

nation_name<- read_html(page_source[[1]]) %>% html_nodes(".infinite div .item > div:nth-child(1)") %>% html_text()
nation_name<- gsub("Congo, Dem\\. Rep\\.", "Congo, Dem", nation_name)

value<- read_html(page_source[[1]]) %>% html_nodes(".infinite div .item div~ div+ div:nth-child(3)") %>% html_text()
value<- value[1:217]

korean_name<- NULL

for(i in 1:length(nation_name)){
  
  remDr$navigate("https://translate.google.co.kr/?hl=ko&tab=TT")
  Sys.sleep(1)
  page_source<- remDr$getPageSource()
  
  word_to_translate_place<- remDr$findElement(using= "id", "source")
  word_to_translate_place$sendKeysToElement(list(nation_name[i]))
  
  Sys.sleep(1.5)
  page_source<- remDr$getPageSource()
  
  korean_name[i]<- read_html(page_source[[1]]) %>% html_nodes("span.translation span") %>% html_text()
  
  print(i)
}

korean_name<- korean_name[1:217]

korean_name2<- gsub(" ","", korean_name)
korean_name2<- gsub("-","", korean_name2)
korean_name2<- gsub("아랍공화국","", korean_name2)
korean_name2<- gsub("제도","", korean_name2)
korean_name2<- gsub("버마","", korean_name2)
korean_name2<- gsub("체코공화국","체코", korean_name2)
korean_name2<- gsub("마케도니아공화국","마케도니아", korean_name2)
korean_name2<- gsub("콩고공화국","콩고", korean_name2)
korean_name2<- gsub("제국","", korean_name2)
korean_name2<- gsub("의상태","", korean_name2)
korean_name2<- gsub("아이보리코스트","아이보리해안", korean_name2)
korean_name2<- gsub("콩고박사","콩고민주공화국", korean_name2)
korean_name2<- gsub("대한민국", "한국", korean_name2)
korean_name2<- gsub("몰다비아", "몰도바", korean_name2)
korean_name2<- gsub("아일오브맨", "맨섬", korean_name2)
korean_name2<- gsub("궁금", "마요트섬", korean_name2)
korean_name2<- gsub("아메리카합중국", "미국", korean_name2)
korean_name2<- gsub("페로", "페로제도", korean_name2)
korean_name2<- gsub("네덜란드령앤틸리스", "앤틸리스", korean_name2)
korean_name2<- gsub("프랑스령폴리네시아의", "프랑스령폴리네시아", korean_name2)
korean_name2<- gsub("몽골리아", "몽골", korean_name2)
korean_name2<- gsub("호주", "오스트레일리아", korean_name2)
korean_name2<- gsub("라오스PDR", "라오스", korean_name2)
korean_name2<- gsub("슬로바키아공화국", "슬로바키아", korean_name2)
korean_name2<- gsub("이란,이슬람공화국", "이란", korean_name2)
korean_name2<- gsub("러시아연방", "러시아", korean_name2)
korean_name2<- gsub("이란,이슬람공화국", "이란", korean_name2)
korean_name2<- gsub("키르기스공화국", "키르기스스탄", korean_name2)
korean_name2<- gsub("마케도니아,FYR", "마케도니아", korean_name2)
korean_name2<- gsub("\\.", "", korean_name2)
korean_name2<- gsub("한국,Dem", "북한", korean_name2)
korean_name2<- gsub("아메리칸사모아", "미국령사모아", korean_name2)
korean_name2<- gsub("예멘,하원의원", "예멘", korean_name2)
korean_name2<- gsub("베네수엘라,RB", "베네수엘라", korean_name2)
korean_name2<- gsub("인류", "스와질란드", korean_name2)
korean_name2<- gsub("콩고,Rep", "콩고", korean_name2)
korean_name2<- gsub("홍콩특별행정구,중국", "홍콩", korean_name2)
korean_name2<- gsub("중국마카오SAR", "마카오", korean_name2)
korean_name2<- gsub("한국,남한", "한국", korean_name2)
korean_name2<- gsub("한국,북한", "북한", korean_name2)
korean_name2<- gsub("미크로네시아,Fed", "미크로네시아", korean_name2)
korean_name2<- gsub("버진(미국)", "미국령버진아일랜드", korean_name2)
korean_name2<- gsub("브루나이다루살람", "브루나이", korean_name2)
korean_name2<- gsub("웨스트뱅크와가자", "웨스트뱅크", korean_name2)

total_nation_df<- ("국가별 정보 데이터4.csv", stringsAsFactor= F)
nation_catagory_info_df<- data.frame(nation_name= korean_name2, nation_catagory_info= value)
1+1
sapply(nation_catagory_info_df, class)

nation_catagory_info_df$nation_name<- as.character(nation_catagory_info_df$nation_name)

new_info<- data.frame(국가이름= setdiff(nation_catagory_info_df$nation_name, total_nation_df$국가이름))
total_nation_df<- bind_rows(total_nation_df, new_info)

new_info2<- data.frame(nation_name= setdiff(total_nation_df$국가이름, nation_catagory_info_df$nation_name))
nation_catagory_info_df<- bind_rows(nation_catagory_info_df, new_info2)

nation_catagory_info_df$nation_name<- factor(nation_catagory_info_df$nation_name, levels= total_nation_df$국가이름)
nation_catagory_info_df<- nation_catagory_info_df[order(nation_catagory_info_df$nation_name),]

nation_catagory_info_df2<- nation_catagory_info_df[,-1] # 국가 이름 열은 겹치므로 제거함
nation_catagory_info_df2<- data.frame(nation_catagory_info_df2)

colnames(nation_catagory_info_df2)<- "합계출산율"
total_nation_df<- cbind(total_nation_df, nation_catagory_info_df2) # total_nation_df에 nation_catagory_info_df의 정보를 합침(column)
colnames(total_nation_df)

write.csv(total_nation_df, "국가별 정보 데이터5.csv", row.names= F)

# 범죄 지수 데이터 추가

total_nation_df<- read.csv("국가별 정보 데이터5.csv", stringsAsFactor= F)

remDr$open()
remDr$navigate("https://www.numbeo.com/crime/rankings_by_country.jsp") # 국가별 범죄지수 사이트

Sys.sleep(3)
page_source<- remDr$getPageSource()

nation_name<- read_html(page_source[[1]]) %>% html_nodes(".cityOrCountryInIndicesTable") %>% html_text()
nation_name<- gsub("Congo, Dem\\. Rep\\.", "Congo, Dem", nation_name)

value<- read_html(page_source[[1]]) %>% html_nodes(".sorting_1") %>% html_text() %>% as.numeric()

korean_name<- NULL
1+1
for(i in 1:length(nation_name)){
  
  remDr$navigate("https://translate.google.co.kr/?hl=ko&tab=TT")
  Sys.sleep(1)
  page_source<- remDr$getPageSource()
  
  word_to_translate_place<- remDr$findElement(using= "id", "source")
  word_to_translate_place$sendKeysToElement(list(nation_name[i]))
  
  Sys.sleep(1.5)
  page_source<- remDr$getPageSource()
  
  korean_name[i]<- read_html(page_source[[1]]) %>% html_nodes("span.translation span") %>% html_text()
  
  print(i)
}

korean_name2<- gsub(" ","", korean_name)
korean_name2<- gsub("-","", korean_name2)
korean_name2<- gsub("아랍공화국","", korean_name2)
korean_name2<- gsub("제도","", korean_name2)
korean_name2<- gsub("버마","", korean_name2)
korean_name2<- gsub("체코공화국","체코", korean_name2)
korean_name2<- gsub("마케도니아공화국","마케도니아", korean_name2)
korean_name2<- gsub("콩고공화국","콩고", korean_name2)
korean_name2<- gsub("제국","", korean_name2)
korean_name2<- gsub("의상태","", korean_name2)
korean_name2<- gsub("아이보리코스트","아이보리해안", korean_name2)
korean_name2<- gsub("콩고박사","콩고민주공화국", korean_name2)
korean_name2<- gsub("대한민국", "한국", korean_name2)
korean_name2<- gsub("몰다비아", "몰도바", korean_name2)
korean_name2<- gsub("아일오브맨", "맨섬", korean_name2)
korean_name2<- gsub("궁금", "마요트섬", korean_name2)
korean_name2<- gsub("아메리카합중국", "미국", korean_name2)
korean_name2<- gsub("페로", "페로제도", korean_name2)
korean_name2<- gsub("네덜란드령앤틸리스", "앤틸리스", korean_name2)
korean_name2<- gsub("프랑스령폴리네시아의", "프랑스령폴리네시아", korean_name2)
korean_name2<- gsub("몽골리아", "몽골", korean_name2)
korean_name2<- gsub("호주", "오스트레일리아", korean_name2)
korean_name2<- gsub("라오스PDR", "라오스", korean_name2)
korean_name2<- gsub("슬로바키아공화국", "슬로바키아", korean_name2)
korean_name2<- gsub("이란,이슬람공화국", "이란", korean_name2)
korean_name2<- gsub("러시아연방", "러시아", korean_name2)
korean_name2<- gsub("이란,이슬람공화국", "이란", korean_name2)
korean_name2<- gsub("키르기스공화국", "키르기스스탄", korean_name2)
korean_name2<- gsub("마케도니아,FYR", "마케도니아", korean_name2)
korean_name2<- gsub(".", "", korean_name2, fixed= T)
korean_name2<- gsub("한국,Dem", "북한", korean_name2)
korean_name2<- gsub("아메리칸사모아", "미국령사모아", korean_name2)
korean_name2<- gsub("예멘,하원의원", "예멘", korean_name2)
korean_name2<- gsub("베네수엘라,RB", "베네수엘라", korean_name2)
korean_name2<- gsub("인류", "스와질란드", korean_name2)
korean_name2<- gsub("콩고,Rep", "콩고", korean_name2)
korean_name2<- gsub("홍콩특별행정구,중국", "홍콩", korean_name2)
korean_name2<- gsub("중국마카오SAR", "마카오", korean_name2)
korean_name2<- gsub("한국,남한", "한국", korean_name2)
korean_name2<- gsub("한국,북한", "북한", korean_name2)
korean_name2<- gsub("미크로네시아,Fed", "미크로네시아", korean_name2)
korean_name2<- gsub("버진(미국)", "미국령버진아일랜드", korean_name2, fixed= T)
korean_name2<- gsub("브루나이다루살람", "브루나이", korean_name2)
korean_name2<- gsub("웨스트뱅크와가자", "웨스트뱅크", korean_name2)
korean_name2<- gsub("팔레스타인영토", "팔레스타인", korean_name2)
korean_name2<- gsub("남아프리카", "남아프리카공화국", korean_name2)
korean_name2<- gsub("체코공화국", "체코", korean_name2)

nation_catagory_info_df<- data.frame(nation_name= korean_name2, nation_catagory_info= value)
1+1
sapply(nation_catagory_info_df, class)

nation_catagory_info_df$nation_name<- as.character(nation_catagory_info_df$nation_name)

new_info<- data.frame(국가이름= setdiff(nation_catagory_info_df$nation_name, total_nation_df$국가이름))
total_nation_df<- bind_rows(total_nation_df, new_info)

new_info2<- data.frame(nation_name= setdiff(total_nation_df$국가이름, nation_catagory_info_df$nation_name))
nation_catagory_info_df<- bind_rows(nation_catagory_info_df, new_info2)

nation_catagory_info_df$nation_name<- factor(nation_catagory_info_df$nation_name, levels= total_nation_df$국가이름)
nation_catagory_info_df<- nation_catagory_info_df[order(nation_catagory_info_df$nation_name),]

nation_catagory_info_df2<- nation_catagory_info_df[,-1] # 국가 이름 열은 겹치므로 제거함
nation_catagory_info_df2<- data.frame(nation_catagory_info_df2)

colnames(nation_catagory_info_df2)<- "범죄지수"
total_nation_df<- cbind(total_nation_df, nation_catagory_info_df2) # total_nation_df에 nation_catagory_info_df의 정보를 합침(column)
colnames(total_nation_df)

sapply(total_nation_df, class)

write.csv(total_nation_df, "국가별 정보 데이터6.csv", row.names= F)


# 민주주의지수 추가

total_nation_df<- read.csv("국가별 정보 데이터6.csv", stringsAsFactor= F)

remDr$open()
remDr$navigate("https://datawrapper.dwcdn.net/jkMCN/2/?abcnewsembedheight=550") # 국가별 민주주의지수 사이트

Sys.sleep(3)
page_source<- remDr$getPageSource()

nation_name<- read_html(page_source[[1]]) %>% html_nodes(".row-divider .row-label span") %>% html_text()
nation_name<- gsub("[0-9]", "", nation_name)
nation_name<- gsub("=", "", nation_name)
nation_name<- gsub("^ ", "", nation_name)
nation_name<- gsub("Congo, Dem\\. Rep\\.", "Congo, Dem", nation_name)

value<- read_html(page_source[[1]]) %>% html_nodes(".direct-value-label span") %>% html_text() %>% as.numeric()
value<- value[-1]
value<- value[complete.cases(value)]

korean_name<- NULL
1+1
for(i in 1:length(nation_name)){
  
  remDr$navigate("https://translate.google.co.kr/?hl=ko&tab=TT")
  Sys.sleep(1)
  page_source<- remDr$getPageSource()
  
  word_to_translate_place<- remDr$findElement(using= "id", "source")
  word_to_translate_place$sendKeysToElement(list(nation_name[i]))
  
  Sys.sleep(1.5)
  page_source<- remDr$getPageSource()
  
  korean_name[i]<- read_html(page_source[[1]]) %>% html_nodes("span.translation span") %>% html_text()
  
  print(i)
}


korean_name<- korean_name[-c(21,77,117)]
korean_name2<- gsub(" ","", korean_name)
korean_name2<- gsub("-","", korean_name2)
korean_name2<- gsub("아랍공화국","", korean_name2)
korean_name2<- gsub("제도","", korean_name2)
korean_name2<- gsub("버마","", korean_name2)
korean_name2<- gsub("체코공화국","체코", korean_name2)
korean_name2<- gsub("마케도니아공화국","마케도니아", korean_name2)
korean_name2<- gsub("콩고공화국","콩고", korean_name2)
korean_name2<- gsub("제국","", korean_name2)
korean_name2<- gsub("의상태","", korean_name2)
korean_name2<- gsub("아이보리코스트","아이보리해안", korean_name2)
korean_name2<- gsub("콩고박사","콩고민주공화국", korean_name2)
korean_name2<- gsub("대한민국", "한국", korean_name2)
korean_name2<- gsub("몰다비아", "몰도바", korean_name2)
korean_name2<- gsub("아일오브맨", "맨섬", korean_name2)
korean_name2<- gsub("궁금", "마요트섬", korean_name2)
korean_name2<- gsub("아메리카합중국", "미국", korean_name2)
korean_name2<- gsub("페로", "페로제도", korean_name2)
korean_name2<- gsub("네덜란드령앤틸리스", "앤틸리스", korean_name2)
korean_name2<- gsub("프랑스령폴리네시아의", "프랑스령폴리네시아", korean_name2)
korean_name2<- gsub("몽골리아", "몽골", korean_name2)
korean_name2<- gsub("호주", "오스트레일리아", korean_name2)
korean_name2<- gsub("라오스PDR", "라오스", korean_name2)
korean_name2<- gsub("슬로바키아공화국", "슬로바키아", korean_name2)
korean_name2<- gsub("이란,이슬람공화국", "이란", korean_name2)
korean_name2<- gsub("러시아연방", "러시아", korean_name2)
korean_name2<- gsub("이란,이슬람공화국", "이란", korean_name2)
korean_name2<- gsub("키르기스공화국", "키르기스스탄", korean_name2)
korean_name2<- gsub("마케도니아,FYR", "마케도니아", korean_name2)
korean_name2<- gsub(".", "", korean_name2, fixed= T)
korean_name2<- gsub("한국,Dem", "북한", korean_name2)
korean_name2<- gsub("아메리칸사모아", "미국령사모아", korean_name2)
korean_name2<- gsub("예멘,하원의원", "예멘", korean_name2)
korean_name2<- gsub("베네수엘라,RB", "베네수엘라", korean_name2)
korean_name2<- gsub("인류", "스와질란드", korean_name2)
korean_name2<- gsub("콩고,Rep", "콩고", korean_name2)
korean_name2<- gsub("홍콩특별행정구,중국", "홍콩", korean_name2)
korean_name2<- gsub("중국마카오SAR", "마카오", korean_name2)
korean_name2<- gsub("한국,남한", "한국", korean_name2)
korean_name2<- gsub("한국,북한", "북한", korean_name2)
korean_name2<- gsub("미크로네시아,Fed", "미크로네시아", korean_name2)
korean_name2<- gsub("버진(미국)", "미국령버진아일랜드", korean_name2, fixed= T)
korean_name2<- gsub("브루나이다루살람", "브루나이", korean_name2)
korean_name2<- gsub("웨스트뱅크와가자", "웨스트뱅크", korean_name2)
korean_name2<- gsub("팔레스타인영토", "팔레스타인", korean_name2)
korean_name2<- gsub("남아프리카", "남아프리카공화국", korean_name2)
korean_name2<- gsub("체코공화국", "체코", korean_name2)

nation_catagory_info_df<- data.frame(nation_name= korean_name2, nation_catagory_info= value)
1+1
sapply(nation_catagory_info_df, class)

nation_catagory_info_df$nation_name<- as.character(nation_catagory_info_df$nation_name)

new_info<- data.frame(국가이름= setdiff(nation_catagory_info_df$nation_name, total_nation_df$국가이름))
total_nation_df<- bind_rows(total_nation_df, new_info)

new_info2<- data.frame(nation_name= setdiff(total_nation_df$국가이름, nation_catagory_info_df$nation_name))
nation_catagory_info_df<- bind_rows(nation_catagory_info_df, new_info2)

nation_catagory_info_df$nation_name<- factor(nation_catagory_info_df$nation_name, levels= total_nation_df$국가이름)
nation_catagory_info_df<- nation_catagory_info_df[order(nation_catagory_info_df$nation_name),]

nation_catagory_info_df2<- nation_catagory_info_df[,-1] # 국가 이름 열은 겹치므로 제거함
nation_catagory_info_df2<- data.frame(nation_catagory_info_df2)

colnames(nation_catagory_info_df2)<- "민주주의지수"
total_nation_df<- cbind(total_nation_df, nation_catagory_info_df2) # total_nation_df에 nation_catagory_info_df의 정보를 합침(column)
colnames(total_nation_df)

sapply(total_nation_df, class)

write.csv(total_nation_df, "국가별 정보 데이터7.csv", row.names= F)


# 정보 추가에 이어서 각 변수 사이의 상관계수 구하기

total_nation_df<- read.csv("국가별 정보 데이터7.csv", stringsAsFactor= F)

colnames(total_nation_df)

all_catagory<- total_nation_df[, c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,33,34,35,36)] # 각 카테고리의 실질적 정보를 포함한 열만 추출함
colnames(all_catagory)

some_catagory<- all_catagory[, -c(6,8,9,10,11)] # 결측치가 많은 열들을 제거함
colnames(some_catagory)[2]<- '일인당국내총생산'
colnames(some_catagory)
View(some_catagory)

cor_matrix<- cor(some_catagory, use='complete.obs', method='pearson') # 상관계수 표
cor_matrix<- round(cor_matrix, 2)

#a<- cor(some_catagory$경제성장률, some_catagory$범죄지수, use='complete.obs')

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
cor_matrix_plot<- corrplot(cor_matrix, method= "color", col= col(70), type= "upper", order= "hclust", tl.col= "black", addCoef.col= "black",
                           tl.srt=60, tl.cex=1, insig = "blank", diag=F, 
                           title= "국가 정보 데이터 간의 상관계수\n(언론자유지수, 지니계수는 높을수록 안좋음\n,부패인식지수는 반대)", mar=c(0,0,3,0))