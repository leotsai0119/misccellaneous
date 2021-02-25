#the first version was written by Cai, Yun-Ting (leotsai0119) in 2015
#for the purpose of testing web crawlers
#and was for a friend Sylvia Tsai
#revised in 2018 Nov with CSS selector code by Cai, Yun-Ting(leotsai0119)

library(rvest)
library(magrittr)
library(data.table)
library(progress)

rm(list = ls())

#前置作業
#到UDN網站確認關鍵字搜尋後頁面數p #簡目 #每頁顯示數要選擇50
#keyword <- "((拼經濟OR拚經濟)+資本主義)" #keywords
keyword <- "APEC" #keywords
DateB <- 19500101 #設定開始日期
DateE <- 20151231 #設定結束日期
news <- "聯合報|經濟日報|聯合晚報|Upaper"
p <- 2  #設定爬的頁數
t <- 15 #設定間隔秒數
#設定完畢

#encoding URL #deafault in Win CP950
UDNsearch <- function(keyword, DateB, DateE, news, page = 1, sharepage = 50){

        root <- "http://udndata.com/ndapp/Searchdec?" #root
        
        paste(keyword, "+" , "日期", ">=", DateB, "+", "日期", "<=", 
              DateE, "+" , "報別", "=", news, sep="") %>% URLencode(reserved = TRUE) -> searchstring
        
        url <- paste(root, "&udndbid=udndata",  "&page=", page, "&SearchString=", 
                     searchstring, "&sharepage=", sharepage, "&select=0", "&kind=2",
                     "&showSearchString=", sep="")
        return(url)
        }

#crawler
UDNcrawler <- function(){
    
        pb <- progress_bar$new(
                format = "處理中 [:bar]:percent | 尚需: :eta | 費時: :elapsed", 
                total = p, clear = FALSE, width= 80, show_after = 0
                ) 

        start <- Sys.time() #紀錄開始時間
  
        #list
        title <- list()
        date <- list()
  
        #for.loop
        for(i in 1:p){
                url <- UDNsearch(keyword, DateB, DateE, news, page = i)
                doc <- read_html(url)
                html_nodes(doc, ".control-pic a") %>% html_text() -> title[[i]]
                html_nodes(doc, ".news span") %>% html_text() -> date[[i]]
                pb$tick()
                Sys.sleep(t)
                }
  
        #unlist
        title <- unlist(title)
        date <- unlist(date)
  
        #data.frame
        tstrsplit(date, "．") -> d1 #分割日期、報別、記者
        tstrsplit(title, ".", fixed = TRUE) -> t1 #分割標號
        d1[[1]] %>% tstrsplit("-") -> d2 #分割年月日
  
        #dataframe 合併年、月、日、標題、報別、記者
        df <- cbind.data.frame(d2[[1]], d2[[2]], d2[[3]], t1[[2]], d1[[2]], d1[[5]])
  
        #作keyword變數
        df$keyword <- keyword
  
        #variable names
        names(df) <- c("year", "month", "day", "title", "paper", "author", "keyword")
  
        #結束
        end <- Sys.time() #紀錄結束時間
        print(end - start) #輸出總花費時間
  
        return(df)
        }

#shutdown computer
shutdown <- function(wait = 0){
        Sys.sleep(wait)
        ifelse(.Platform$OS.type == "windows", shell("shutdown -s -t 0"), 
        system("shutdown -h now"))
        }


#test with keyword "APEC"
df <- UDNcrawler()