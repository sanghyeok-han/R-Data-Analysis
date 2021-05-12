library(rvest)
library(stringr)
library(dplyr)
library(KoNLP)
library(lattice)
library(wordcloud2)
library(qgraph)

# 한 페이지 스크래핑

url_ex<- "https://www1.president.go.kr/petitions?page=1"

main_page_ex<- read_html(url_ex)

catagory_ex<- main_page_ex %>% html_nodes("div.bl_category") %>% html_text()
catagory_ex<- catagory_ex[7:length(catagory_ex)]
catagory_ex<- gsub("^분류  ", "", catagory_ex)
catagory_ex

url_title_ex<- main_page_ex %>% html_nodes(".bl_subject a") %>% html_attr("href") # 각 제목의 url 추출
url_title_ex<- url_title_ex[5:length(url_title_ex)] # 앞 부분에 쓸모 없는 부분 제거
url_title_ex

# 하나의 타이틀 url을 통한 전체 길이 타이틀 데이터 획득
#title_page_ex<- read_html(url_title_ex[1])
#title_page_ex

#title_ex<- title_page_ex %>% html_nodes(".petitionsView_title") %>% html_text()
#title_ex

total_title_ex<- NULL # 모든 타이틀의 url을 저장할 변수 선언

for(i in 1:15){                 # 모든 타이틀 url을 통한 전체 길이 타이틀 데이터 획득
  title_page_ex<- read_html(url_title_ex[i])
  title_ex<- title_page_ex %>% html_nodes(".petitionsView_title") %>% html_text()
  
  total_title_ex<- append(total_title_ex, title_ex)
}

total_title_ex


participated_ex<- main_page_ex %>% html_nodes("div.bl_agree") %>% html_text()
participated_ex<- participated_ex[7:length(participated_ex)]
participated_ex<- str_trim(participated_ex)
participated_ex<- gsub("^참여인원 ","", participated_ex)
participated_ex<- gsub("명$","", participated_ex)
participated_ex

opinion_ex<- data.frame(catagory= catagory_ex, title= total_title_ex, participated= participated_ex)
opinion_ex

# total_opinion
# 최근 2개월(11월 24일~1월 24일)(2개월) 모든 페이지 스크래핑
# i는 1:2753 예상 소요시간: 2시간 41분

# total_opinion1_5
# 11월 23일~11월 24일
# i는 2781:2861

# total_opinion2
# 최근 2개월 이후 ~최근 5개월 (8월 23일~11월 23일)(3개월) * 오류남
# i는 2764:5120

# total_opinion3
# ~최근 5개월 *  2에서 오류난 부분 이후
# i는 5120:7870



url_base<- "https://www1.president.go.kr/petitions?page="
total_opinion1_5<- NULL

for(i in 2781:2861){
  
  url<- paste(url_base, i, sep= "")
  
  value<- tryCatch(
    {
      main_page<- read_html(url)
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
      next
    }
   }
  # 분류 가져오기
  
  catagory<- main_page %>% html_nodes("div.bl_category") %>% html_text()
  catagory<- catagory[7:length(catagory)]
  catagory<- gsub("^분류  ", "", catagory)
  
  # 제목 가져오기
  
  url_title<- main_page %>% html_nodes(".bl_subject a") %>% html_attr("href") # 각 제목의 url 추출
  url_title<- url_title[5:length(url_title)] # 앞 부분에 쓸모 없는 부분 제거
  
  total_title<- NULL # 모든 타이틀의 url을 저장할 변수 선언
  
  for(j in 1:15){                 # 모든 타이틀 url을 통한 전체 길이 타이틀 데이터 획득
    title_page<- read_html(url_title[j])
    title<- title_page %>% html_nodes(".petitionsView_title") %>% html_text()
    
    total_title<- append(total_title, title)
  }
  
  # 참여인원 가져오기
  
  participated<- main_page %>% html_nodes("div.bl_agree") %>% html_text()
  participated<- participated[7:length(participated)]
  participated<- str_trim(participated)
  participated<- gsub("^참여인원 ","", participated)
  participated<- gsub("명$","", participated)
  
  opinion<- data.frame(catagory= catagory, title= total_title, participated= participated)
  
  total_opinion1_5<- rbind(total_opinion1_5, opinion)
  print(i)
}

total_opinion
head(total_opinion, 50)
tail(total_opinion, 50)

#write.csv(total_opinion, "total_opinion.csv", row.names= F)
write.csv(total_opinion1_5, "total_opinion1_5.csv", row.names= F)

#후에 total_opinion1,1_5,2,3 데이터 프레임 합치고 unique 함수 이용해서 중복값 제거함

tp1<- read.csv("total_opinion.csv", stringsAsFactors = F)
tp1_5<- read.csv("total_opinion1_5.csv", stringsAsFactors = F)
tp2<- read.csv("total_opinion2.csv", stringsAsFactors = F)
tp3<- read.csv("total_opinion3.csv", stringsAsFactors = F)

tp_all<- rbind(tp1, tp1_5, tp2, tp3)
tp_all<- unique(tp_all)

write.csv(tp_all, "total_opinion(18.8.23~19.1.24).csv", row.names= F)

tp<- read.csv("total_opinion(18.8.23~19.1.24).csv", stringsAsFactors= F)

tp[1449,]

tp$participated<- gsub(",", "", tp$participated) # 참여 인원의 반점 삭제

tp$participated<- as.integer(tp$participated)

#write.csv(tp, "total_opinion(18.8.23~19.1.24)2.csv", row.names= F)
#tp<- read.csv("total_opinion(18.8.23~19.1.24)2.csv", stringsAsFactors= F)

# 전체 청원들의 카테고리 종류 분석 

result_df<- tp %>% group_by(catagory) %>% count()
result_df
sum(result_df$n)
nrow(tp)

View(result_df)

# 각각 1만과 20만이상의 참여 인원을 가진 청원들의 카테고리 종류 분석

tp_10000<- tp[tp$participated>=10000,]
tp_200000<- tp[tp$participated>=200000,]

result_df_200000<- tp_200000 %>% group_by(catagory) %>% count()
result_df_200000
result_df_10000<- tp_10000 %>% group_by(catagory) %>% count()
result_df_10000
View(result_df_10000)

# 각 분야 별로 청원 제목에 가장 많이 쓰인 단어들 분석

tp_정치<- tp[tp$catagory=='정치개혁',]
tp_외교<- tp[tp$catagory=='외교/통일/국방',]
tp_일자리<- tp[tp$catagory=='일자리',]
tp_미래<- tp[tp$catagory=='미래',]
tp_성장<- tp[tp$catagory=='성장동력',]
tp_농산<- tp[tp$catagory=='농산어촌',]
tp_보건<- tp[tp$catagory=='보건복지',]
tp_육아<- tp[tp$catagory=='육아/교육',]
tp_안전<- tp[tp$catagory=='안전/환경',]
tp_저출산<- tp[tp$catagory=='저출산/고령화대책',]
tp_행정<- tp[tp$catagory=='행정',]
tp_반려<- tp[tp$catagory=='반려동물',]
tp_교통<- tp[tp$catagory=='교통/건축/국토',]
tp_경제<- tp[tp$catagory=='경제민주화',]
tp_인권<- tp[tp$catagory=='인권/성평등',]
tp_문화<- tp[tp$catagory=='문화/예술/체육/언론',]
tp_기타<- tp[tp$catagory=='기타',]

com_정치<- tp_정치$title
com_외교<- tp_외교$title
com_일자리<- tp_일자리$title
com_미래<- tp_미래$title
com_성장<- tp_성장$title
com_농산<- tp_농산$title
com_보건<- tp_보건$title
com_육아<- tp_육아$title
com_안전<- tp_안전$title
com_저출산<- tp_저출산$title
com_행정<- tp_행정$title
com_반려<- tp_반려$title
com_교통<- tp_교통$title
com_경제<- tp_경제$title
com_인권<- tp_인권$title
com_문화<- tp_문화$title
com_기타<- tp_기타$title

data_정치<- sapply(com_정치, extractNoun, USE.NAMES=F)
data_외교<- sapply(com_외교, extractNoun, USE.NAMES=F)
data_일자리<- sapply(com_일자리, extractNoun, USE.NAMES=F)
data_미래<- sapply(com_미래, extractNoun, USE.NAMES=F)
data_성장<- sapply(com_성장, extractNoun, USE.NAMES=F)
data_농산<- sapply(com_농산, extractNoun, USE.NAMES=F)
data_보건<- sapply(com_보건, extractNoun, USE.NAMES=F)
data_육아<- sapply(com_육아, extractNoun, USE.NAMES=F)
data_안전<- sapply(com_안전, extractNoun, USE.NAMES=F)
data_저출산<- sapply(com_저출산, extractNoun, USE.NAMES=F)
data_행정<- sapply(com_행정, extractNoun, USE.NAMES=F)
data_반려<- sapply(com_반려, extractNoun, USE.NAMES=F)
data_교통<- sapply(com_교통, extractNoun, USE.NAMES=F)
data_경제<- sapply(com_경제, extractNoun, USE.NAMES=F)
data_인권<- sapply(com_인권, extractNoun, USE.NAMES=F)
data_문화<- sapply(com_문화, extractNoun, USE.NAMES=F)
data_기타<- sapply(com_기타, extractNoun, USE.NAMES=F)

undata_정치<- unlist(data_정치); undata_정치<- Filter(function(x) {nchar(x)>=2}, undata_정치)
undata_외교<- unlist(data_외교); undata_외교<- Filter(function(x) {nchar(x)>=2}, undata_외교)
undata_일자리<- unlist(data_일자리); undata_일자리<- Filter(function(x) {nchar(x)>=2}, undata_일자리)
undata_미래<- unlist(data_미래); undata_미래<- Filter(function(x) {nchar(x)>=2}, undata_미래)
undata_성장<- unlist(data_성장); undata_성장<- Filter(function(x) {nchar(x)>=2}, undata_성장)
undata_농산<- unlist(data_농산); undata_농산<- Filter(function(x) {nchar(x)>=2}, undata_농산)
undata_보건<- unlist(data_보건); undata_보건<- Filter(function(x) {nchar(x)>=2}, undata_보건)
undata_육아<- unlist(data_육아); undata_육아<- Filter(function(x) {nchar(x)>=2}, undata_육아)
undata_안전<- unlist(data_안전); undata_안전<- Filter(function(x) {nchar(x)>=2}, undata_안전)
undata_저출산<- unlist(data_저출산); undata_저출산<- Filter(function(x) {nchar(x)>=2}, undata_저출산)
undata_행정<- unlist(data_행정); undata_행정<- Filter(function(x) {nchar(x)>=2}, undata_행정)
undata_반려<- unlist(data_반려); undata_반려<- Filter(function(x) {nchar(x)>=2}, undata_반려)
undata_교통<- unlist(data_교통); undata_교통<- Filter(function(x) {nchar(x)>=2}, undata_교통)
undata_경제<- unlist(data_경제); undata_경제<- Filter(function(x) {nchar(x)>=2}, undata_경제)
undata_인권<- unlist(data_인권); undata_인권<- Filter(function(x) {nchar(x)>=2}, undata_인권)
undata_문화<- unlist(data_문화); undata_문화<- Filter(function(x) {nchar(x)>=2}, undata_문화)
undata_기타<- unlist(data_기타); undata_기타<- Filter(function(x) {nchar(x)>=2}, undata_기타)

# _ 부분을 바꿔 가면서 하기

#undata_기타[grep("양", undata_기타)]

undata_기타<- gsub("문재","문재인", undata_기타)
undata_기타<- gsub("인대통령님","", undata_기타)
undata_기타<- gsub("인대통령","", undata_기타)
undata_기타<- gsub("인씨","", undata_기타)
undata_기타<- gsub("을|를|는|에","", undata_기타); undata_기타<- Filter(function(x) {nchar(x)>=2}, undata_기타)
undata_기타<- undata_기타[-grep("니|세요|마라|들이|하게|으로", undata_기타)]
#undata_기타

result_data_기타<- data.frame(undata_기타)
write(unlist(undata_기타), "total_opinion(18.8.23~19.1.24)_기타 분석.txt")
result_data_정치<- read.table("total_opinion(18.8.23~19.1.24)_저출산,고령화대책 분석.txt", quote = "")
#result_data_기타
rd_정치<- table(result_data_정치)
#rd_기타


#result_data_<- read.table("total_opinion(18.8.23~19.1.24)_문화,예술,체육,언론 분석.txt", quote = "")
#rd_<- table(result_data_)
barchart(tail(sort(rd_), 30), col= "lightblue", xlab= "Term frequency", main= "Words about 문화/예술/체육/언론 in Opinion")

# 전체에서 가장 많이 쓰인 단어 분석

com<- tp$title
data<- sapply(com, extractNoun, USE.NAMES=F)
undata<- unlist(data); undata<- Filter(function(x) {nchar(x)>=2}, undata)

#undata[grep("양", undata)]

undata<- gsub("문재","문재인", undata)
undata<- gsub("인대통령님","", undata)
undata<- gsub("인대통령","", undata)
undata<- gsub("인씨","", undata)
undata<- gsub("을|를|는|에","", undata); undata<- Filter(function(x) {nchar(x)>=2}, undata)
undata<- undata[-grep("니|세요|마라|들이|하게|으로", undata)]
#undata

result_data<- data.frame(undata)
write(unlist(undata), "total_opinion(18.8.23~19.1.24)분석.txt")
result_data<- read.table("total_opinion(18.8.23~19.1.24)분석.txt", quote = "")
#result_data
rd<- table(result_data)
barchart(tail(sort(rd), 100), col= "lightblue", xlab= "Term frequency", main= "Words about 전체 분야 in Opinion")