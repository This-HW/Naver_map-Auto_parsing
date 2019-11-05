



###########################################################
#        naver map - keword scrapping .     By YongJu     #
###########################################################



# 네이버 지도 V4버전에 맞추어 만들어진 지도 정보 파싱 프로그램입니다.
# 89번째 줄에 있는 search_keyword가 검색어이며 이 부분만 수정하고 돌리시면 됩니다.
# RSelenum를 사용함으로써 Seleum 머신을 구동시킨 뒤에 실행해야 정상 작동합니다.


#함수 리스트
#click_next_button #page_count #parse_data #main

install.packages("RSelenium")
install.packages("rvest")
library(RSelenium)
library(rvest)


##다음 페이지를 클릭하는 함수
click_next_button <- function(remDr){
  next_page <- remDr$findElement(using = "css", "#panel div.panel_content_flexible > div.search_result .next")
  next_page$clickElement()
  Sys.sleep(1)
}

#뒷 페이지가 더 있을 때 [1] True, 없을때 [1]: false 반환
#현재 페이지에서 보이는 페이지 수를 [2]로 반환
page_count_f <- function(remDr){
  first <- remDr$findElement(using = "css", "#panel > div.panel_content.nano.has-scrollbar > div.scroll_pane.content > div.panel_content_flexible > div.search_result > div > div > .first-child")
  first <- first$getElementText()
  first <- unlist(as.numeric(first))
  last <- remDr$findElement(using = "css", "#panel > div.panel_content.nano.has-scrollbar > div.scroll_pane.content > div.panel_content_flexible > div.search_result > div > div > .last-child")
  last <- last$getElementText()
  last <- unlist(as.numeric(last))
  page_num <- (last-first+1)
  
  
  next_btn <- remDr$findElement(using="css", " #panel > div.panel_content.nano.has-scrollbar > div.scroll_pane.content > div.panel_content_flexible > div.search_result  .next")
  btn_tag <- next_btn$getElementTagName()
  
  if(btn_tag == "a"){
    flag <- FALSE
  }else{
    flag <- TRUE}
  return(c(!flag, page_num, last))
}

## 현재 페이지의 데이터 파싱하는 함수
parse_data <- function(remDr){
  
  site_view <- remDr$findElement(using = "css", "#panel > div.panel_content.nano.has-scrollbar > div.scroll_pane.content > div.panel_content_flexible > div.search_result > ul")
  parsed_site <- site_view$getPageSource()[[1]]
  parsed_site <- read_html(parsed_site)
  
  search_lst <- parsed_site %>% html_nodes("dl.lsnx_det")
  name <- search_lst %>% html_nodes("dt > a") %>% html_text(); name
  address <- search_lst %>% html_nodes("dd.addr") %>% html_text(); address
  address <- gsub("지번", "", address)
  address <- gsub("  ","", address); address
  phone <- search_lst %>% html_nodes("dd.tel") %>% html_text();
  phone <- gsub("  ", "", phone); phone
  category <- search_lst %>% html_nodes("dd.cate") %>% html_text(); category
  
  page_list <- data.frame(name, address, category)
  return(page_list)
}

click_next_page <- function(remDr, next_page){
  next_path <- paste0("#panel > div.panel_content.nano.has-scrollbar > div.scroll_pane.content > div.panel_content_flexible > div.search_result > div > div > a:nth-child(",next_page,")")
  next_btn <- remDr$findElement(using="css", next_path)
  next_btn$clickElement()
}


############################################################################
#                                m a i n                                   #
############################################################################


remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()

search_keyword <- "검색 키워드" #검색 키워드 입력
tryCatch({
  url <- paste0("https://v4.map.naver.com/?query=", search_keyword)
  remDr$navigate(url)},
  error= function(e) print("네이버 지도 v4가 구동되지 않습니다."),
  warning = function(w) rpint("네이버 지도 v4가 정상적으로 구동되지 않습니다.")
)

#만들당시 뜨던 팝업을 제거하는 구문
try({
  webElem <- remDr$findElement(using = "css", "#dday_popup > div.popup_content.popup_link > button > span.img")
  webElem$clickElement()
}, silent=T) 

target_list <- NULL
while(1){
  
  #페이지 관련 정보 읽어오기
  page <- page_count_f(remDr);
  j <- 0
  
  #각 페이지별로 파싱하기
  for(i in 1 : page[2]){
    target_list <- rbind(target_list, parse_data(remDr))
    j <- i+1
    if(j > page[2]){
      break;
    }    #마지막 페이지 일 떄 멈춤
    else{
      Sys.sleep(0.2)
      click_next_page(remDr, j+1)
    }    #다음페이지로 넘어가는 함수
    
  }
  
  #다음 버튼 활성화 되어 있을 때 다음버튼 클릭
  if(page[1]){
    Sys.sleep(0.2)
    click_next_button(remDr)
  }else #다음페이지 버튼 활성화 되지 않았을 때 파싱 종료
  {
    break;       
  }
}

#검색결과를 csv파일로 저장 -> 저장장소는 현재 디렉토리
write.csv(target_list, paste0("./\'",search_keyword,"\' 검색결과.csv"))
