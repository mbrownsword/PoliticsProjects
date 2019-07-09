library(tm)
library(textclean)
library(data.table)
library(flexdashboard)
library(rvest)
library(googlesheets)
library(readxl)
library(dplyr)
library(raster)
library(tidyverse)
library(data.table)
library(tools)
library(httr)
library(janitor)
library(tidytext)
library(tidyr)
library(stringr)
library(vwr)
library(datasets)
library(utf8)

################################THE HOUSE################################


################################start — urls################################

url <- NA
r <- NA
status <- NA
for (i in 1:100){
  url[i] <- paste0("https://projects.propublica.org/represent/votes/115/all/house?page=",i)
  r <- GET(url[i])
  status <- status_code(r) 
  if (status == 500){
    next()
  }else{
    webpage <- read_html(url[i])
    testing<- webpage %>% html_nodes(xpath='//div[@class="margin-bottom--md"]//h3[@class="no-padding no-border"]') %>% html_text()
    if (length(testing) == 0){
      url <- head(url,-1)
      stop()
    }else{
      next()
    }
  }
}


################################names&numbers time################################

webpage <- NA
HouseBills1 <- list("vector",30)
BillNumbers <- 30
HouseBills <- matrix(0, BillNumbers, length(url))
for (i in 1:length(url)){
  r <- GET(url[i])
  status <- status_code(r) 
  if (status == 500){
    next()
  }else{
    webpage <- read_html(url[i])
    HouseBills1 <- webpage %>% html_nodes(xpath='//div[@class="margin-bottom--md"]//h3[@class]') %>% html_text()
    Billnumbers <- length(HouseBills1)
    HouseBills1 <- strsplit(HouseBills1,':',fixed=TRUE)
    for (j in 1:Billnumbers){
      HouseBills[j,i] <- HouseBills1[[j]][1]
    }
  }
}
HouseBills <- as.vector(HouseBills)
BillsonBills <- data.frame(HouseBills)
HouseBillsNos <- NA
HouseBillsNos <- strsplit(as.character(BillsonBills$HouseBills),' ',fixed=TRUE)
for (i in 1:length(HouseBills)){
  HouseBillsNos[i] <- HouseBillsNos[[i]][3]
}
BillsonBills$BillsNos <- as.numeric(HouseBillsNos)

for (i in 1:length(BillsonBills$BillsNos)){
  if (is.na(BillsonBills$BillsNos[i])==TRUE){
    if(i==1){
      BillsonBills$BillsNos[i] <- BillsonBills$BillsNos[i+1] + 1  
    }else{
      BillsonBills$BillsNos[i] <- BillsonBills$BillsNos[i-1] - 1
    }
  }else{
    next()
  }
}



##########the house divides############
url_two <- NA
for (i in 1:length(HouseBills)){
  if((BillsonBills$BillsNos==1)[i]==TRUE){
    BillsonBills_115 <- BillsonBills[1:i,]
    BillsonBills_2 <- BillsonBills[(i+1):length(BillsonBills$HouseBills),]
    stop()
  }else{
    next()
  }
}
BillsonBills_2 <-  BillsonBills_2[!BillsonBills_2$BillsNos<=0,]

BillsonBills_two <- BillsonBills_115



################################1. descriptions, totals and so much more!################################

for (i in 1:length(BillsonBills_2$BillsNos)){
  url_two[i] <- paste0("https://projects.propublica.org/represent/votes/115/house/1/",i)
}
HouseBillsDescriptions <- NA
HouseBillName <- NA
row.names <- NA
HouseTime <- NA
DemYes <- NA
DemNo <- NA
DemOther <- NA
RepYes <- NA
RepNo <- NA
RepOther <- NA
for (i in 1:length(url_two)){
  r <- GET(url_two[i])
  status <- status_code(r) 
  if (status == 500 | status == 404){
    next()
  }else{
    webpage <- read_html(url_two[i])
    HouseBillsDescriptions1 <- webpage %>% html_nodes(xpath='//td[@class="v-align-top"]') %>% html_text()
    HouseBill <- webpage %>% html_nodes(xpath='//a[@itemprop="url"]//span[@itemprop="name"]') %>% html_text()
    HouseTimes <- webpage %>% html_nodes(xpath='//time') %>% html_text()
    HouseVotes <- webpage %>% html_nodes(xpath='//td[@class="numeric numeric--mobile nowrap"]') %>% html_text()
    row.names[i] <- i
    if(length(HouseVotes)==0){
      DemYes[i] <- NA
      DemNo[i] <- NA
      DemOther[i] <- NA
      RepYes[i] <- NA
      RepNo[i] <- NA
      RepOther[i] <- NA
    }else{
      DemYes[i] <- HouseVotes[2]
      DemNo[i] <- HouseVotes[4]
      DemOther[i] <- HouseVotes[6]
      RepYes[i] <- HouseVotes[1]
      RepNo[i] <- HouseVotes[3]
      RepOther[i] <- HouseVotes[5]
    }
    if(length(HouseTimes)==0){
      HouseTime[i] <- NA
    }else{
      HouseTime[i] <- HouseTimes
    }
    if(length(HouseBill)==0){
      HouseBillName[i] <- NA
    }else{
      HouseBillName[i] <- strsplit(as.character(HouseBill),'-',fixed=TRUE)[[1]][1]
    }
    if(any(grepl("Description",HouseBillsDescriptions1))==TRUE){
      HouseBillsDescriptions[i] <- HouseBillsDescriptions1[4]
    }else{
      HouseBillsDescriptions[i] <- NA
    }
  }
  
}
HouseBillDescriptions <- data.frame(HouseBillsDescriptions,row.names,HouseBillName,HouseTime,DemYes,DemNo,DemOther,RepYes,RepNo,RepOther)
BillsonBills_2$BillsNos <- as.numeric(BillsonBills_2$BillsNos)
BillScrips_1_115 <- left_join(BillsonBills_2,HouseBillDescriptions, by = c("BillsNos" = "row.names"))



################################1. CAA sponsoors################################
BillScrips_1_115$HouseBillName_clean <- gsub("\\.","",BillScrips_1_115$HouseBillName)
BillScrips_1_115$HouseBillName_clean[is.na(BillScrips_1_115$HouseBillName_clean)] <- "poop"
BillScrips_1_115$BillSponsor <- NA
BillScrips_1_115$Billcosponsor <- NA
BillScrips_1_115$thaturl <- NA

for (i in 1:length(BillScrips_1_115$HouseBills)){
  url_bills <- paste0("https://projects.propublica.org/represent/bills/115/",BillScrips_1_115$HouseBillName_clean[BillScrips_1_115$BillsNos==i])
  r <- GET(url_bills)
  status <- status_code(r) 
  if (status == 500 | status == 404 | status == 502){
    next()
  }else{
    webpage_bills <- read_html(url_bills)
    Billsponsor <- webpage_bills %>% html_nodes(xpath='//div[@id="sponsors"]//a') %>% html_text()
    if (length(Billsponsor)==0){
      BillScrips_1_115$BillSponsor[BillScrips_1_115$BillsNos==i] <- NA
    }else{
      BillScrips_1_115$BillSponsor[BillScrips_1_115$BillsNos==i] <- trimws(strsplit(as.character(Billsponsor),' (',fixed=TRUE)[[1]][1], which=c("left"))
    }
    Billcosponsors <- webpage_bills %>% html_nodes(xpath='//div[@id="cosponsors"]//a') %>% html_text()
    if (length(Billcosponsors)==0){
      BillScrips_1_115$Billcosponsor[BillScrips_1_115$BillsNos==i] <- NA
    }else{
      for (j in 1:length(Billcosponsors)){
        Billcosponsors[j] <- trimws(strsplit(as.character(Billcosponsors[j]),' (',fixed=TRUE)[[1]][1], which=c("left"))
      }
      Billcosponsors <- paste0(Billcosponsors, collapse=",")
      BillScrips_1_115$Billcosponsor[BillScrips_1_115$BillsNos==i] <- Billcosponsors
    }
    thaturl <- webpage_bills %>% html_nodes(xpath='//div[@class="bill-summary-block"]//a//@href') %>% html_text()
    if (length(thaturl)==0){
      thaturl <- webpage_bills %>% html_nodes(xpath='//a[@class="btn btn-primary btn-medium btn-block"]//@href') %>% html_text()
      if (length(thaturl)==0){
        BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==i] <- NA
        }else{
          BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==i] <- paste0(strsplit(as.character(thaturl),'text',fixed=TRUE)[[1]][1],"summary")
        }
    }else{
      BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==i] <- thaturl
    }
    
  }
}


################################1. From Congress, with love################################

Bill_summary <- NA

for (i in 1:length(BillScrips_1_115$thaturl)){
  if (is.na(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==i])==TRUE){
    next()
  }else{
    if(length(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==i])==0){
      next()
    }else{
      r <- GET(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==i])
      status <- status_code(r) 
      if (status == 500 | status == 404 | status == 502){
        next()
      }else{
        webpage_billvanilly <- read_html(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==i])
        Bill_committee <- (webpage_billvanilly %>% html_nodes(xpath='//table[@class="standard01"]//td') %>% html_text())[2]
        Bill_subject <- (webpage_billvanilly %>% html_nodes(xpath='//ul[@class="plain"]//li[1]') %>% html_text())[2]
        Bill_summary_pgraph_bullets <- (webpage_billvanilly %>% html_nodes(xpath='//div[@id="bill-summary"]//*') %>% html_text())
        Bill_committee_report <- (webpage_billvanilly %>% html_nodes(xpath='//td//a//@href') %>% html_text())
        for (j in 1:length(Bill_summary_pgraph_bullets)){
          if (length(Bill_summary_pgraph_bullets[j])==0 | length(Bill_summary_pgraph_bullets[j-1])==0){
            next()
          }else{
            if(Bill_summary_pgraph_bullets[j] == Bill_summary_pgraph_bullets[j-1]){
              Bill_summary <- Bill_summary_pgraph_bullets[j+1:length(Bill_summary_pgraph_bullets)]
            }else{
              next()
            } 
          }
          
        }
        Bill_summary <- na.omit(Bill_summary)
        Bill_summary <- paste0(Bill_summary, collapse=" ")
        if (length(Bill_committee)==0 | is.na(Bill_committee)){
          BillScrips_1_115$Bill_committee[BillScrips_1_115$BillsNos==i] <- NA
        }else{
          if(grepl("[[:digit:]]",as.character(Bill_committee))){
            BillScrips_1_115$Bill_committee[BillScrips_1_115$BillsNos==i] <- NA 
          }else{
            BillScrips_1_115$Bill_committee[BillScrips_1_115$BillsNos==i] <- Bill_committee 
          }
        }
        if (length(Bill_committee_report)==0){
          BillScrips_1_115$Bill_committee_report[BillScrips_1_115$BillsNos==i] <- NA
        }else{
          if (is.na(Bill_committee_report)==TRUE){
            BillScrips_1_115$Bill_committee_report[BillScrips_1_115$BillsNos==i] <- NA
          }else{
            for (j in 1:length(Bill_committee_report)){
              if(grepl("house-report",as.character(Bill_committee_report[j]))){
                Bill_committee_report[j] <- Bill_committee_report[j]
              }else{
                Bill_committee_report[j] <- NA
              }
            }
            }
          Bill_committee_report <- na.omit(as.character(Bill_committee_report))
          if(length(Bill_committee_report)==0){
            BillScrips_1_115$Bill_committee_report[BillScrips_1_115$BillsNos==i] <- NA
          }else{
            if (is.na(Bill_committee_report)==TRUE){
              BillScrips_1_115$Bill_committee_report[BillScrips_1_115$BillsNos==i] <- NA
            }else{
            BillScrips_1_115$Bill_committee_report[BillScrips_1_115$BillsNos==i] <- Bill_committee_report 
            }
            }
          
          }
        BillScrips_1_115$Bill_subject[BillScrips_1_115$BillsNos==i] <- Bill_subject
        BillScrips_1_115$Bill_summary[BillScrips_1_115$BillsNos==i] <- Bill_summary
      }
    }
  }
  
}

################################1. just textin Bill################################
for (i in 1:length(BillScrips_1_115$HouseBills)){
  if (is.na(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==i])==TRUE){
    next()
  }else{
    if(length(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==i])==0){
      next()
    }else{
      url_bills_text <- paste0(strsplit(as.character(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==i]),'summary',fixed=TRUE)[[1]][1],"text/rfh")
      r <- GET(url_bills_text)
      status <- status_code(r) 
      if (status == 500 | status == 404 | status == 502){
        next()
      }else{
        webpage_bills_text <- read_html(url_bills_text)
        thatbill_text <- webpage_bills_text %>% html_nodes(xpath='//p') %>% html_text()
        if (length(thatbill_text)<=5){
          url_bills_text <- paste0(strsplit(as.character(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==i]),'summary',fixed=TRUE)[[1]][1],"text/eh")
          webpage_bills_text <- read_html(url_bills_text)
          thatbill_text <- webpage_bills_text %>% html_nodes(xpath='//p') %>% html_text()
          thatbill_text <- thatbill_text[thatbill_text != ""]
          thatbill_text <- thatbill_text[thatbill_text != " "]
        }else{
          thatbill_text <- thatbill_text[thatbill_text != ""]
          thatbill_text <- thatbill_text[thatbill_text != " "]
        }
        if (length(thatbill_text)==0){
          BillScrips_1_115$thatbill_text[BillScrips_1_115$BillsNos==i] <- NA
        }else{
          thatbill_text <- paste0(thatbill_text, collapse=" ")
          BillScrips_1_115$thatbill_text[BillScrips_1_115$BillsNos==i] <- thatbill_text
        }
      }
    }
  }
  
  
}




cheese <- BillScrips_1_115$thatbill_text
for (i in 1:length(BillScrips_1_115$thatbill_text)){
  if(duplicated(BillScrips_1_115$thatbill_text)[i]==TRUE){
    BillScrips_1_115$thatbill_text[i] <- NA
  }else{
    BillScrips_1_115$thatbill_text[i] <- cheese[i]
  }
  
}


################################1. In the amendment city!!!################################

amendment_city <- replicate(length(BillScrips_1_115$HouseBills), list()) 


for (j in 1:length(BillScrips_1_115$HouseBills)){
  if (is.na(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==j])==TRUE){
    amendment_city_test <- NA
    amendment_city[[j]] <- amendment_city_test
    next()
  }else{
    if(length(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==j])==0){
      amendment_city_test <- NA
      amendment_city[[j]] <- amendment_city_test
      next()
    }else{
      thatbill_amendment_links <- NA
      thatbill_amendment_sponsorlinks <- NA
      thatbill_amendment_RepCode <- NA
      RepName <- NA
      name_state <- NA
      url_bills_amendments <- paste0(strsplit(as.character(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==j]),'summary',fixed=TRUE)[[1]][1],"amendments?pageSize=250")
      r <- GET(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==j])
      status <- status_code(r) 
      if (status == 500 | status == 404 | status == 502){
        amendment_city_test <- NA
        amendment_city[[j]] <- amendment_city_test
        next()
      }else{
        webpage_bills_amendments <- read_html(url_bills_amendments)
        thatbill_amendment_links <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-heading amendment-heading"]//a//@href') %>% html_text()
        if (length(thatbill_amendment_links)==0){
          amendment_city_test <- NA
          amendment_city[[j]] <- amendment_city_test
          next()
        }else{
          thatbill_amendment_links <- thatbill_amendment_links[seq(1, length(thatbill_amendment_links), 2)] 
          for (i in 1:length(thatbill_amendment_links)){
            if (i == 1){
              next()
            }else{
              if (thatbill_amendment_links[i] == thatbill_amendment_links[i-1]){
                thatbill_amendment_links[i] <- thatbill_amendment_links[i]
                thatbill_amendment_links[i-1] <- NA 
              }else{
                next()
              }
            }
          }
          thatbill_amendment_links <- na.omit(thatbill_amendment_links)
          thatbill_amendment_sponsorlinks <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-item"]//a//@href') %>% html_text()
          thatbill_amendment_rollcalllinks <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-item"]//a//@href') %>% html_text()
          if (length(thatbill_amendment_sponsorlinks)==0){
            amendment_city_test <- NA
            amendment_city[[j]] <- amendment_city_test
            next()
          }else{
            if(length(thatbill_amendment_rollcalllinks)==0){
              amendment_city_test <- NA
              amendment_city[[j]] <- amendment_city_test
              next()
            }else{
            for (i in 1:length(thatbill_amendment_rollcalllinks)){
              if(duplicated(thatbill_amendment_rollcalllinks)[i]==TRUE){
                thatbill_amendment_rollcalllinks[i] <- NA
              }else{
                thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
              }
            }
            thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
            for (i in 1:length(thatbill_amendment_rollcalllinks)){
              if(grepl("congressional-record",thatbill_amendment_rollcalllinks)[i]==TRUE){
                thatbill_amendment_rollcalllinks[i] <- NA
              }else{
                thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
              }
            }
            for (i in 1:length(thatbill_amendment_rollcalllinks)){
              if(grepl("member",thatbill_amendment_rollcalllinks)[i]==TRUE){
                thatbill_amendment_rollcalllinks[i] <- NA
              }else{
                thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
              }
            }
            thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
            if(length(thatbill_amendment_rollcalllinks)==0){
              amendment_city_test <- NA
              amendment_city[[j]] <- amendment_city_test
              next()
            }else{
              thatbill_amendment_rollcalllinks_placeholder <- matrix(0, length(thatbill_amendment_rollcalllinks),2)
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                thatbill_amendment_rollcalllinks_placeholder[i,1] <-  thatbill_amendment_rollcalllinks[i]
                thatbill_amendment_rollcalllinks_placeholder[i,2] <-  strsplit(as.character(thatbill_amendment_rollcalllinks[i]),"/",fixed=TRUE)[[1]][5]
              }
              thatbill_amendment_rollcalllinks <- thatbill_amendment_rollcalllinks_placeholder
              for (i in 1:length(thatbill_amendment_rollcalllinks[,1])){
                if(grepl("evs",thatbill_amendment_rollcalllinks[i,1])==TRUE){
                  thatbill_amendment_rollcalllinks[i,1] <- thatbill_amendment_rollcalllinks[i,1]
                  thatbill_amendment_rollcalllinks[i,2] <- thatbill_amendment_rollcalllinks[i+1,2]
                }else{
                  thatbill_amendment_rollcalllinks[i,1] <-  NA
                  thatbill_amendment_rollcalllinks[i,2] <-  NA
                }
              }
            }
            
            thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
            
            }
          }
            for (i in 1:length(thatbill_amendment_sponsorlinks)){
              if(grepl("member",thatbill_amendment_sponsorlinks[i])==TRUE){
                thatbill_amendment_sponsorlinks[i] <- thatbill_amendment_sponsorlinks[i]
              }else{
                thatbill_amendment_sponsorlinks[i] <- NA
              }
            }
            thatbill_amendment_sponsorlinks <- na.omit(thatbill_amendment_sponsorlinks)
            thatbill_amendment_sponsorlinks <- thatbill_amendment_sponsorlinks[seq(1, length(thatbill_amendment_sponsorlinks), 2)]
            for (i in 1:length(thatbill_amendment_sponsorlinks)){
              thatbill_amendment_RepCode[i] <- strsplit(as.character(thatbill_amendment_sponsorlinks[i]),'/',fixed=TRUE)[[1]][4]
            }
            for (i in 1:length(thatbill_amendment_RepCode)){
              if (length(HouseResidents_115$HouseResidents_name[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
                if(length(SenateResidents_115$SenateResidents_name[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
                  RepName[i] <- "former Congressional member"
                }else{
                  RepName[i] <- SenateResidents_115$SenateResidents_name[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]]
                }
              }else{
                RepName[i] <- HouseResidents_115$HouseResidents_name[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]]
              }
            }
            for (i in 1:length(thatbill_amendment_RepCode)){
                if (length(HouseResidents_115$name_state[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
                  if(length(SenateResidents_115$name_state[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
                    name_state[i] <- "former Congressional member"
                  }else{
                    name_state[i] <- SenateResidents_115$name_state[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]]
                  }
                }else{
                  name_state[i] <- HouseResidents_115$name_state[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]]
                }
              
              
            }
            amendment_city_test <- matrix(0,length(thatbill_amendment_links),6)
            for (i in 1:length(thatbill_amendment_links)){
              amendment_city_test[i,1] <- thatbill_amendment_links[i]
              amendment_city_test[i,2] <- thatbill_amendment_RepCode[i]
              amendment_city_test[i,3] <- RepName[i]
              amendment_city_test[i,4] <- name_state[i]
              amendment_city_test[i,5] <- strsplit(strsplit(as.character(thatbill_amendment_links[i]),"/",fixed=TRUE)[[1]][7],"?",fixed=TRUE)[[1]][1]
              
            } 
            for (i in 1:length(amendment_city_test[,5])){
              if(length(thatbill_amendment_rollcalllinks[amendment_city_test[i,5]==thatbill_amendment_rollcalllinks[,2]][1])==0){
                amendment_city_test[i,6] <- NA
              }else{
                amendment_city_test[i,6] <- thatbill_amendment_rollcalllinks[amendment_city_test[i,5]==thatbill_amendment_rollcalllinks[,2]][1]
              }
              
            } 
            for (i in 1:length(amendment_city_test[,5])){
              if(is.na(amendment_city_test[i,6])){
                amendment_city_test[i,1] <- NA
                amendment_city_test[i,2] <- NA
                amendment_city_test[i,3] <- NA
                amendment_city_test[i,4] <- NA
                amendment_city_test[i,5] <- NA
              }else{
                next()
              }
              
            } 
            amendment_city_test <- na.omit(amendment_city_test)
            amendment_city[[j]] <- amendment_city_test
          }
          
          
        }
      }
    }
  }





################################1. In the amendment city TOOOO!!!################################

amendment_city_too <- replicate(length(BillScrips_1_115$HouseBills), list()) 


for (j in 1:length(BillScrips_1_115$HouseBills)){
  if (is.na(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==j])==TRUE){
    amendment_city_test <- NA
    amendment_city_too[[j]] <- amendment_city_test
    next()
  }else{
    if(length(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==j])==0){
      amendment_city_test <- NA
      amendment_city_too[[j]] <- amendment_city_test
      next()
    }else{
      thatbill_amendment_links <- NA
      thatbill_amendment_sponsorlinks <- NA
      thatbill_amendment_RepCode <- NA
      RepName <- NA
      name_state <- NA
      url_bills_amendments <- paste0(strsplit(as.character(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==j]),'summary',fixed=TRUE)[[1]][1],"amendments?pageSize=250&page=2")
      r <- GET(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==j])
      status <- status_code(r) 
      if (status == 500 | status == 404 | status == 502){
        amendment_city_test <- NA
        amendment_city_too[[j]] <- amendment_city_test
        next()
      }else{
        webpage_bills_amendments <- read_html(url_bills_amendments)
        thatbill_amendment_links <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-heading amendment-heading"]//a//@href') %>% html_text()
        if (length(thatbill_amendment_links)==0){
          amendment_city_test <- NA
          amendment_city_too[[j]] <- amendment_city_test
          next()
        }else{
          thatbill_amendment_links <- thatbill_amendment_links[seq(1, length(thatbill_amendment_links), 2)] 
          for (i in 1:length(thatbill_amendment_links)){
            if (i == 1){
              next()
            }else{
              if (thatbill_amendment_links[i] == thatbill_amendment_links[i-1]){
                thatbill_amendment_links[i] <- thatbill_amendment_links[i]
                thatbill_amendment_links[i-1] <- NA 
              }else{
                next()
              }
            }
          }
          thatbill_amendment_links <- na.omit(thatbill_amendment_links)
          thatbill_amendment_sponsorlinks <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-item"]//a//@href') %>% html_text()
          thatbill_amendment_rollcalllinks <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-item"]//a//@href') %>% html_text()
          if (length(thatbill_amendment_sponsorlinks)==0){
            amendment_city_test <- NA
            amendment_city_too[[j]] <- amendment_city_test
            next()
          }else{
            if(length(thatbill_amendment_rollcalllinks)==0){
              amendment_city_test <- NA
              amendment_city_too[[j]] <- amendment_city_test
              next()
            }else{
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(duplicated(thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(grepl("congressional-record",thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(grepl("member",thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              if(length(thatbill_amendment_rollcalllinks)==0){
                amendment_city_test <- NA
                amendment_city_too[[j]] <- amendment_city_test
                next()
              }else{
                thatbill_amendment_rollcalllinks_placeholder <- matrix(0, length(thatbill_amendment_rollcalllinks),2)
                for (i in 1:length(thatbill_amendment_rollcalllinks)){
                  thatbill_amendment_rollcalllinks_placeholder[i,1] <-  thatbill_amendment_rollcalllinks[i]
                  thatbill_amendment_rollcalllinks_placeholder[i,2] <-  strsplit(as.character(thatbill_amendment_rollcalllinks[i]),"/",fixed=TRUE)[[1]][5]
                }
                thatbill_amendment_rollcalllinks <- thatbill_amendment_rollcalllinks_placeholder
                for (i in 1:length(thatbill_amendment_rollcalllinks[,1])){
                  if(grepl("evs",thatbill_amendment_rollcalllinks[i,1])==TRUE){
                    thatbill_amendment_rollcalllinks[i,1] <- thatbill_amendment_rollcalllinks[i,1]
                    thatbill_amendment_rollcalllinks[i,2] <- thatbill_amendment_rollcalllinks[i+1,2]
                  }else{
                    thatbill_amendment_rollcalllinks[i,1] <-  NA
                    thatbill_amendment_rollcalllinks[i,2] <-  NA
                  }
                }
              }
              
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              
            }
          }
          for (i in 1:length(thatbill_amendment_sponsorlinks)){
            if(grepl("member",thatbill_amendment_sponsorlinks[i])==TRUE){
              thatbill_amendment_sponsorlinks[i] <- thatbill_amendment_sponsorlinks[i]
            }else{
              thatbill_amendment_sponsorlinks[i] <- NA
            }
          }
          thatbill_amendment_sponsorlinks <- na.omit(thatbill_amendment_sponsorlinks)
          thatbill_amendment_sponsorlinks <- thatbill_amendment_sponsorlinks[seq(1, length(thatbill_amendment_sponsorlinks), 2)]
          for (i in 1:length(thatbill_amendment_sponsorlinks)){
            thatbill_amendment_RepCode[i] <- strsplit(as.character(thatbill_amendment_sponsorlinks[i]),'/',fixed=TRUE)[[1]][4]
          }
          for (i in 1:length(thatbill_amendment_RepCode)){
            if (length(HouseResidents_115$HouseResidents_name[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
              if(length(SenateResidents_115$SenateResidents_name[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
                RepName[i] <- "former Congressional member"
              }else{
                RepName[i] <- SenateResidents_115$SenateResidents_name[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]]
              }
            }else{
              RepName[i] <- HouseResidents_115$HouseResidents_name[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]]
            }
          }
          for (i in 1:length(thatbill_amendment_RepCode)){
            if (length(HouseResidents_115$name_state[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
              if(length(SenateResidents_115$name_state[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
                name_state[i] <- "former Congressional member"
              }else{
                name_state[i] <- SenateResidents_115$name_state[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]]
              }
            }else{
              name_state[i] <- HouseResidents_115$name_state[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]]
            }
            
            
          }
          amendment_city_test <- matrix(0,length(thatbill_amendment_links),6)
          for (i in 1:length(thatbill_amendment_links)){
            amendment_city_test[i,1] <- thatbill_amendment_links[i]
            amendment_city_test[i,2] <- thatbill_amendment_RepCode[i]
            amendment_city_test[i,3] <- RepName[i]
            amendment_city_test[i,4] <- name_state[i]
            amendment_city_test[i,5] <- strsplit(strsplit(as.character(thatbill_amendment_links[i]),"/",fixed=TRUE)[[1]][7],"?",fixed=TRUE)[[1]][1]
            
          } 
          for (i in 1:length(amendment_city_test[,5])){
            if(length(thatbill_amendment_rollcalllinks[amendment_city_test[i,5]==thatbill_amendment_rollcalllinks[,2]][1])==0){
              amendment_city_test[i,6] <- NA
            }else{
              amendment_city_test[i,6] <- thatbill_amendment_rollcalllinks[amendment_city_test[i,5]==thatbill_amendment_rollcalllinks[,2]][1]
            }
            
          } 
          for (i in 1:length(amendment_city_test[,5])){
            if(is.na(amendment_city_test[i,6])){
              amendment_city_test[i,1] <- NA
              amendment_city_test[i,2] <- NA
              amendment_city_test[i,3] <- NA
              amendment_city_test[i,4] <- NA
              amendment_city_test[i,5] <- NA
            }else{
              next()
            }
            
          } 
          amendment_city_test <- na.omit(amendment_city_test)
          amendment_city_too[[j]] <- amendment_city_test
        }
        
        
      }
    }
  }
}

for (j in 1:length(amendment_city)){
  if (length(amendment_city_too[[j]])==0){
    next()
  }else{
    if (is.na(amendment_city_too[[j]])){
      next()
    }else{
      if (length(amendment_city[[j]])==0){
        amendment_city[[j]] <- amendment_city_too[[j]]
      }else{
        if (is.na(amendment_city[[j]])){
          amendment_city[[j]] <- amendment_city_too[[j]]
        }else{
        if(sum(amendment_city_too[[j]][1,]==amendment_city[[j]][1,])==6){
          next()
        }else{
          amendment_city[[j]] <- rbind(amendment_city[[j]],amendment_city_too[[j]])
        }  
      }
      }
  }
  }
}

################################1. In the amendment city REMIXXXXX!!!################################


amendment_city_three <- replicate(length(BillScrips_1_115$HouseBills), list()) 


for (j in 1:length(BillScrips_1_115$HouseBills)){
  if (is.na(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==j])==TRUE){
    amendment_city_test <- NA
    amendment_city_three[[j]] <- amendment_city_test
    next()
  }else{
    if(length(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==j])==0){
      amendment_city_test <- NA
      amendment_city_three[[j]] <- amendment_city_test
      next()
    }else{
      thatbill_amendment_links <- NA
      thatbill_amendment_sponsorlinks <- NA
      thatbill_amendment_RepCode <- NA
      RepName <- NA
      name_state <- NA
      url_bills_amendments <- paste0(strsplit(as.character(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==j]),'summary',fixed=TRUE)[[1]][1],"amendments?pageSize=250&page=3")
      r <- GET(BillScrips_1_115$thaturl[BillScrips_1_115$BillsNos==j])
      status <- status_code(r) 
      if (status == 500 | status == 404 | status == 502){
        amendment_city_test <- NA
        amendment_city_three[[j]] <- amendment_city_test
        next()
      }else{
        webpage_bills_amendments <- read_html(url_bills_amendments)
        thatbill_amendment_links <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-heading amendment-heading"]//a//@href') %>% html_text()
        if (length(thatbill_amendment_links)==0){
          amendment_city_test <- NA
          amendment_city_three[[j]] <- amendment_city_test
          next()
        }else{
          thatbill_amendment_links <- thatbill_amendment_links[seq(1, length(thatbill_amendment_links), 2)] 
          for (i in 1:length(thatbill_amendment_links)){
            if (i == 1){
              next()
            }else{
              if (thatbill_amendment_links[i] == thatbill_amendment_links[i-1]){
                thatbill_amendment_links[i] <- thatbill_amendment_links[i]
                thatbill_amendment_links[i-1] <- NA 
              }else{
                next()
              }
            }
          }
          thatbill_amendment_links <- na.omit(thatbill_amendment_links)
          thatbill_amendment_sponsorlinks <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-item"]//a//@href') %>% html_text()
          thatbill_amendment_rollcalllinks <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-item"]//a//@href') %>% html_text()
          if (length(thatbill_amendment_sponsorlinks)==0){
            amendment_city_test <- NA
            amendment_city_three[[j]] <- amendment_city_test
            next()
          }else{
            if(length(thatbill_amendment_rollcalllinks)==0){
              amendment_city_test <- NA
              amendment_city_three[[j]] <- amendment_city_test
              next()
            }else{
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(duplicated(thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(grepl("congressional-record",thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(grepl("member",thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              if(length(thatbill_amendment_rollcalllinks)==0){
                amendment_city_test <- NA
                amendment_city_three[[j]] <- amendment_city_test
                next()
              }else{
                thatbill_amendment_rollcalllinks_placeholder <- matrix(0, length(thatbill_amendment_rollcalllinks),2)
                for (i in 1:length(thatbill_amendment_rollcalllinks)){
                  thatbill_amendment_rollcalllinks_placeholder[i,1] <-  thatbill_amendment_rollcalllinks[i]
                  thatbill_amendment_rollcalllinks_placeholder[i,2] <-  strsplit(as.character(thatbill_amendment_rollcalllinks[i]),"/",fixed=TRUE)[[1]][5]
                }
                thatbill_amendment_rollcalllinks <- thatbill_amendment_rollcalllinks_placeholder
                for (i in 1:length(thatbill_amendment_rollcalllinks[,1])){
                  if(grepl("evs",thatbill_amendment_rollcalllinks[i,1])==TRUE){
                    thatbill_amendment_rollcalllinks[i,1] <- thatbill_amendment_rollcalllinks[i,1]
                    thatbill_amendment_rollcalllinks[i,2] <- thatbill_amendment_rollcalllinks[i+1,2]
                  }else{
                    thatbill_amendment_rollcalllinks[i,1] <-  NA
                    thatbill_amendment_rollcalllinks[i,2] <-  NA
                  }
                }
              }
              
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              
            }
          }
          for (i in 1:length(thatbill_amendment_sponsorlinks)){
            if(grepl("member",thatbill_amendment_sponsorlinks[i])==TRUE){
              thatbill_amendment_sponsorlinks[i] <- thatbill_amendment_sponsorlinks[i]
            }else{
              thatbill_amendment_sponsorlinks[i] <- NA
            }
          }
          thatbill_amendment_sponsorlinks <- na.omit(thatbill_amendment_sponsorlinks)
          thatbill_amendment_sponsorlinks <- thatbill_amendment_sponsorlinks[seq(1, length(thatbill_amendment_sponsorlinks), 2)]
          for (i in 1:length(thatbill_amendment_sponsorlinks)){
            thatbill_amendment_RepCode[i] <- strsplit(as.character(thatbill_amendment_sponsorlinks[i]),'/',fixed=TRUE)[[1]][4]
          }
          for (i in 1:length(thatbill_amendment_RepCode)){
            if (length(HouseResidents_115$HouseResidents_name[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
              if(length(SenateResidents_115$SenateResidents_name[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
                RepName[i] <- "former Congressional member"
              }else{
                RepName[i] <- SenateResidents_115$SenateResidents_name[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]]
              }
            }else{
              RepName[i] <- HouseResidents_115$HouseResidents_name[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]]
            }
          }
          for (i in 1:length(thatbill_amendment_RepCode)){
            if (length(HouseResidents_115$name_state[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
              if(length(SenateResidents_115$name_state[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
                name_state[i] <- "former Congressional member"
              }else{
                name_state[i] <- SenateResidents_115$name_state[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]]
              }
            }else{
              name_state[i] <- HouseResidents_115$name_state[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]]
            }
            
            
          }
          amendment_city_test <- matrix(0,length(thatbill_amendment_links),6)
          for (i in 1:length(thatbill_amendment_links)){
            amendment_city_test[i,1] <- thatbill_amendment_links[i]
            amendment_city_test[i,2] <- thatbill_amendment_RepCode[i]
            amendment_city_test[i,3] <- RepName[i]
            amendment_city_test[i,4] <- name_state[i]
            amendment_city_test[i,5] <- strsplit(strsplit(as.character(thatbill_amendment_links[i]),"/",fixed=TRUE)[[1]][7],"?",fixed=TRUE)[[1]][1]
            
          } 
          for (i in 1:length(amendment_city_test[,5])){
            if(length(thatbill_amendment_rollcalllinks[amendment_city_test[i,5]==thatbill_amendment_rollcalllinks[,2]][1])==0){
              amendment_city_test[i,6] <- NA
            }else{
              amendment_city_test[i,6] <- thatbill_amendment_rollcalllinks[amendment_city_test[i,5]==thatbill_amendment_rollcalllinks[,2]][1]
            }
            
          } 
          for (i in 1:length(amendment_city_test[,5])){
            if(is.na(amendment_city_test[i,6])){
              amendment_city_test[i,1] <- NA
              amendment_city_test[i,2] <- NA
              amendment_city_test[i,3] <- NA
              amendment_city_test[i,4] <- NA
              amendment_city_test[i,5] <- NA
            }else{
              next()
            }
            
          } 
          amendment_city_test <- na.omit(amendment_city_test)
          amendment_city_three[[j]] <- amendment_city_test
        }
        
        
      }
    }
  }
}

for (j in 1:length(amendment_city)){
  if (length(amendment_city_three[[j]])==0){
    next()
  }else{
    if (is.na(amendment_city_three[[j]])){
      next()
    }else{
      if (length(amendment_city[[j]])==0){
        amendment_city[[j]] <- amendment_city_three[[j]]
      }else{
        if (is.na(amendment_city[[j]])){
          amendment_city[[j]] <- amendment_city_three[[j]]
        }else{
          if(sum(amendment_city_three[[j]][1,]==amendment_city[[j]][1,])==6){
            next()
          }else{
            amendment_city[[j]] <- rbind(amendment_city[[j]],amendment_city_three[[j]])
          }  
        }
      }
    }
  }
}


################################1. amendment link################################


BillScrips_1_115$amendmentlink <- NA
BillScrips_1_115$amendmentsponsorcode <- NA
numbers_amendments <- list("vector",length(BillScrips_1_115$HouseBillName))


for (i in 1:length(BillScrips_1_115$HouseBillName)){
  numbers_amendments_placeholder <- NA
  if(length(amendment_city[[i]])==0){
    numbers_amendments[[i]] <- NA
    next()
  }else{
    if(is.na(amendment_city[[i]])){
      numbers_amendments[[i]] <- NA
      next()
    }else{
      for (j in 1:length(amendment_city[[i]][,1])){
        hi_america <- amendment_city[[i]]
        hi_america[,4] <- gsub('[[:punct:]]',' ',hi_america[,4])
        if(length(BillScrips_1_115$HouseBillsDescriptions[BillScrips_1_115$BillsNos==i])==0){
          numbers_amendments_placeholder[j] <- NA
          next()
        }else{
          descrip_placeholder <- gsub('[[:punct:]]',' ',as.character(BillScrips_1_115$HouseBillsDescriptions[BillScrips_1_115$BillsNos==i]))
          if(grepl(as.character(hi_america[j,4]),as.character(descrip_placeholder))==TRUE){
            numbers_amendments_placeholder[j] <- j
          }else{
            numbers_amendments_placeholder[j] <- NA
          }
        }
      }
      numbers_amendments[[i]] <- numbers_amendments_placeholder
    }
  }
}

for(i in 1:length(numbers_amendments)){
  if(length(numbers_amendments[[i]])==1){
    next()
  }else{
    numbers_amendments[[i]] <- na.omit(numbers_amendments[[i]])
  }
}

for(i in 1:length(numbers_amendments)){
  if(length(numbers_amendments[[i]])==0){
    BillScrips_1_115$amendmentlink[BillScrips_1_115$BillsNos==i] <- NA
    BillScrips_1_115$amendmentsponsorcode[BillScrips_1_115$BillsNos==i] <- NA
  }else{
    if(length(numbers_amendments[[i]])==1){
      if(is.na(numbers_amendments[[i]])){
        BillScrips_1_115$amendmentlink[BillScrips_1_115$BillsNos==i] <- NA
        BillScrips_1_115$amendmentsponsorcode[BillScrips_1_115$BillsNos==i] <- NA
        next()
      }else{
        BillScrips_1_115$amendmentlink[BillScrips_1_115$BillsNos==i] <- amendment_city[[i]][numbers_amendments[[i]][1],1]
        BillScrips_1_115$amendmentsponsorcode[BillScrips_1_115$BillsNos==i] <- amendment_city[[i]][numbers_amendments[[i]][1],2]
      }
    }else{
      keep_track_1 <- BillScrips_1_115$BillsNos[BillScrips_1_115$HouseBillName==BillScrips_1_115$HouseBillName[BillScrips_1_115$BillsNos==i]]
      keep_track_1 <- na.omit(keep_track_1)
      keep_track <- BillScrips_1_115$BillsNos[grepl(amendment_city[[i]][numbers_amendments[[i]][1],4],BillScrips_1_115$HouseBillsDescriptions)]
      keep_track <- intersect(keep_track_1,keep_track)
      hi_america_2 <- numbers_amendments[[i]]
      data_frame <- data.frame(keep_track,hi_america_2)
      number_i_want <- data_frame$`hi_america_2`[data_frame$keep_track==i]
      BillScrips_1_115$amendmentlink[BillScrips_1_115$BillsNos==i] <- amendment_city[[i]][number_i_want,1]
      BillScrips_1_115$amendmentsponsorcode[BillScrips_1_115$BillsNos==i] <- amendment_city[[i]][number_i_want,2]
    }
  }
}



amendment_check <- data.frame(BillScrips_1_115$amendmentlink[grepl("Amendment",BillScrips_1_115$HouseBillsDescriptions)],BillScrips_1_115$HouseBillsDescriptions[grepl("Amendment",BillScrips_1_115$HouseBillsDescriptions)])



################################2. descriptions, totals and so much more!################################
url_two_2 <- NA
for (i in 1:length(BillsonBills_two$BillsNos)){
  url_two_2[i] <- paste0("https://projects.propublica.org/represent/votes/115/house/2/",i)
}
HouseBillsDescriptions_2 <- NA
HouseBillName_2 <- NA
row.names_2 <- NA
HouseTime_2 <- NA
DemYes_2 <- NA
DemNo_2 <- NA
DemOther_2 <- NA
RepYes_2 <- NA
RepNo_2 <- NA
RepOther_2 <- NA
for (i in 1:length(url_two_2)){
  r_2 <- GET(url_two_2[i])
  status_2 <- status_code(r_2) 
  if (status_2 == 500 | status_2 == 404){
    next()
  }else{
    webpage <- read_html(url_two_2[i])
    HouseBillsDescriptions1 <- webpage %>% html_nodes(xpath='//td[@class="v-align-top"]') %>% html_text()
    HouseBill <- webpage %>% html_nodes(xpath='//a[@itemprop="url"]//span[@itemprop="name"]') %>% html_text()
    HouseTimes <- webpage %>% html_nodes(xpath='//time') %>% html_text()
    HouseVotes <- webpage %>% html_nodes(xpath='//td[@class="numeric numeric--mobile nowrap"]') %>% html_text()
    row.names_2[i] <- i
    if(length(HouseVotes)==0){
      DemYes_2[i] <- NA
      DemNo_2[i] <- NA
      DemOther_2[i] <- NA
      RepYes_2[i] <- NA
      RepNo_2[i] <- NA
      RepOther_2[i] <- NA
    }else{
      DemYes_2[i] <- HouseVotes[2]
      DemNo_2[i] <- HouseVotes[4]
      DemOther_2[i] <- HouseVotes[6]
      RepYes_2[i] <- HouseVotes[1]
      RepNo_2[i] <- HouseVotes[3]
      RepOther_2[i] <- HouseVotes[5]
    }
    if(length(HouseTimes)==0){
      HouseTime_2[i] <- NA
    }else{
      HouseTime_2[i] <- HouseTimes
    }
    if(length(HouseBill)==0){
      HouseBillName_2[i] <- NA
    }else{
      HouseBillName_2[i] <- strsplit(as.character(HouseBill),'-',fixed=TRUE)[[1]][1]
    }
    if(any(grepl("Description",HouseBillsDescriptions1))==TRUE){
      HouseBillsDescriptions_2[i] <- HouseBillsDescriptions1[4]
    }else{
      HouseBillsDescriptions_2[i] <- NA
    }
  }
  
}
HouseBillDescriptions_2 <- data.frame(HouseBillsDescriptions_2,row.names_2,HouseBillName_2,HouseTime_2,DemYes_2,DemNo_2,DemOther_2,RepYes_2,RepNo_2,RepOther_2)
BillsonBills_two$BillsNos <- as.numeric(BillsonBills_two$BillsNos)
BillScrips_2_115 <- left_join(BillsonBills_two,HouseBillDescriptions_2, by = c("BillsNos" = "row.names_2"))

################################2. CAA sponsoors################################
BillScrips_2_115$HouseBillName_clean <- gsub("\\.","",BillScrips_2_115$HouseBillName)
BillScrips_2_115$HouseBillName_clean[is.na(BillScrips_2_115$HouseBillName_clean)] <- "poop"
BillScrips_2_115$BillSponsor <- NA
BillScrips_2_115$Billcosponsor <- NA
BillScrips_2_115$thaturl <- NA

for (i in 1:length(BillScrips_2_115$HouseBills)){
  url_bills <- paste0("https://projects.propublica.org/represent/bills/115/",BillScrips_2_115$HouseBillName_clean[BillScrips_2_115$BillsNos==i])
  r <- GET(url_bills)
  status <- status_code(r) 
  if (status == 500 | status == 404 | status == 502){
    next()
  }else{
    webpage_bills <- read_html(url_bills)
    Billsponsor <- webpage_bills %>% html_nodes(xpath='//div[@id="sponsors"]//a') %>% html_text()
    if (length(Billsponsor)==0){
      BillScrips_2_115$BillSponsor[BillScrips_2_115$BillsNos==i] <- NA
    }else{
      BillScrips_2_115$BillSponsor[BillScrips_2_115$BillsNos==i] <- trimws(strsplit(as.character(Billsponsor),' (',fixed=TRUE)[[1]][1], which=c("left"))
    }
    Billcosponsors <- webpage_bills %>% html_nodes(xpath='//div[@id="cosponsors"]//a') %>% html_text()
    if (length(Billcosponsors)==0){
      BillScrips_2_115$Billcosponsor[BillScrips_2_115$BillsNos==i] <- NA
    }else{
      for (j in 1:length(Billcosponsors)){
        Billcosponsors[j] <- trimws(strsplit(as.character(Billcosponsors[j]),' (',fixed=TRUE)[[1]][1], which=c("left"))
      }
      Billcosponsors <- paste0(Billcosponsors, collapse=",")
      BillScrips_2_115$Billcosponsor[BillScrips_2_115$BillsNos==i] <- Billcosponsors
    }
    thaturl <- webpage_bills %>% html_nodes(xpath='//div[@class="bill-summary-block"]//a//@href') %>% html_text()
    if (length(thaturl)==0){
      thaturl <- webpage_bills %>% html_nodes(xpath='//a[@class="btn btn-primary btn-medium btn-block"]//@href') %>% html_text()
      if (length(thaturl)==0){
        BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==i] <- NA
      }else{
        BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==i] <- paste0(strsplit(as.character(thaturl),'text',fixed=TRUE)[[1]][1],"summary")
      }
    }else{
      BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==i] <- thaturl
    }
    
  }
}
################################2. From Congress, with love################################

Bill_summary <- NA

for (i in 1:length(BillScrips_2_115$thaturl)){
  if (is.na(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==i])==TRUE){
    next()
  }else{
    if(length(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==i])==0){
      next()
    }else{
      r <- GET(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==i])
      status <- status_code(r) 
      if (status == 500 | status == 404 | status == 502){
        next()
      }else{
        webpage_billvanilly <- read_html(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==i])
        Bill_committee <- (webpage_billvanilly %>% html_nodes(xpath='//table[@class="standard01"]//td') %>% html_text())[2]
        Bill_subject <- (webpage_billvanilly %>% html_nodes(xpath='//ul[@class="plain"]//li[1]') %>% html_text())[2]
        Bill_summary_pgraph_bullets <- (webpage_billvanilly %>% html_nodes(xpath='//div[@id="bill-summary"]//*') %>% html_text())
        Bill_committee_report <- (webpage_billvanilly %>% html_nodes(xpath='//td//a//@href') %>% html_text())
        for (j in 1:length(Bill_summary_pgraph_bullets)){
          if (length(Bill_summary_pgraph_bullets[j])==0 | length(Bill_summary_pgraph_bullets[j-1])==0){
            next()
          }else{
            if(Bill_summary_pgraph_bullets[j] == Bill_summary_pgraph_bullets[j-1]){
              Bill_summary <- Bill_summary_pgraph_bullets[j+1:length(Bill_summary_pgraph_bullets)]
            }else{
              next()
            } 
          }
          
        }
        Bill_summary <- na.omit(Bill_summary)
        Bill_summary <- paste0(Bill_summary, collapse=" ")
        if (length(Bill_committee)==0 | is.na(Bill_committee)){
          BillScrips_2_115$Bill_committee[BillScrips_2_115$BillsNos==i] <- NA
        }else{
          if(grepl("[[:digit:]]",as.character(Bill_committee))){
            BillScrips_2_115$Bill_committee[BillScrips_2_115$BillsNos==i] <- NA 
          }else{
            BillScrips_2_115$Bill_committee[BillScrips_2_115$BillsNos==i] <- Bill_committee 
          }
        }
        if (length(Bill_committee_report)==0){
          BillScrips_2_115$Bill_committee_report[BillScrips_2_115$BillsNos==i] <- NA
        }else{
          if (is.na(Bill_committee_report)==TRUE){
            BillScrips_2_115$Bill_committee_report[BillScrips_2_115$BillsNos==i] <- NA
          }else{
            for (j in 1:length(Bill_committee_report)){
              if(grepl("house-report",as.character(Bill_committee_report[j]))){
                Bill_committee_report[j] <- Bill_committee_report[j]
              }else{
                Bill_committee_report[j] <- NA
              }
            }
          }
          Bill_committee_report <- na.omit(as.character(Bill_committee_report))
          if(length(Bill_committee_report)==0){
            BillScrips_2_115$Bill_committee_report[BillScrips_2_115$BillsNos==i] <- NA
          }else{
            if (is.na(Bill_committee_report)==TRUE){
              BillScrips_2_115$Bill_committee_report[BillScrips_2_115$BillsNos==i] <- NA
            }else{
              BillScrips_2_115$Bill_committee_report[BillScrips_2_115$BillsNos==i] <- Bill_committee_report 
            }
          }
          
        }
        BillScrips_2_115$Bill_subject[BillScrips_2_115$BillsNos==i] <- Bill_subject
        BillScrips_2_115$Bill_summary[BillScrips_2_115$BillsNos==i] <- Bill_summary
      }
    }
  }
  
}
unique(BillScrips_2_115$Bill_summary)


################################2. just textin Bill################################
for (i in 1:length(BillScrips_2_115$HouseBills)){
  if (is.na(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==i])==TRUE){
    next()
  }else{
    if(length(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==i])==0){
      next()
    }else{
      url_bills_text <- paste0(strsplit(as.character(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==i]),'summary',fixed=TRUE)[[1]][1],"text/rfh")
      r <- GET(url_bills_text)
      status <- status_code(r) 
      if (status == 500 | status == 404 | status == 502){
        next()
      }else{
        webpage_bills_text <- read_html(url_bills_text)
        thatbill_text <- webpage_bills_text %>% html_nodes(xpath='//p') %>% html_text()
        if (length(thatbill_text)<=5){
          url_bills_text <- paste0(strsplit(as.character(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==i]),'summary',fixed=TRUE)[[1]][1],"text/eh")
          webpage_bills_text <- read_html(url_bills_text)
          thatbill_text <- webpage_bills_text %>% html_nodes(xpath='//p') %>% html_text()
          thatbill_text <- thatbill_text[thatbill_text != ""]
          thatbill_text <- thatbill_text[thatbill_text != " "]
        }else{
          thatbill_text <- thatbill_text[thatbill_text != ""]
          thatbill_text <- thatbill_text[thatbill_text != " "]
        }
        if (length(thatbill_text)==0){
          BillScrips_2_115$thatbill_text[BillScrips_2_115$BillsNos==i] <- NA
        }else{
          thatbill_text <- paste0(thatbill_text, collapse=" ")
          BillScrips_2_115$thatbill_text[BillScrips_2_115$BillsNos==i] <- thatbill_text
        }
      }
    }
  }
  
  
}




cheese_2 <- BillScrips_2_115$thatbill_text
for (i in 1:length(BillScrips_2_115$thatbill_text)){
  if(duplicated(BillScrips_2_115$thatbill_text)[i]==TRUE){
    BillScrips_2_115$thatbill_text[i] <- NA
  }else{
    BillScrips_2_115$thatbill_text[i] <- cheese_2[i]
  }
  
}


################################2. In the amendment city!!!################################

amendment_city_2 <- replicate(length(BillScrips_2_115$HouseBills), list()) 


for (j in 1:length(BillScrips_2_115$HouseBills)){
  if (is.na(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==j])==TRUE){
    amendment_city_test <- NA
    amendment_city_2[[j]] <- amendment_city_test
    next()
  }else{
    if(length(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==j])==0){
      amendment_city_test <- NA
      amendment_city_2[[j]] <- amendment_city_test
      next()
    }else{
      thatbill_amendment_links <- NA
      thatbill_amendment_sponsorlinks <- NA
      thatbill_amendment_RepCode <- NA
      RepName <- NA
      name_state <- NA
      url_bills_amendments <- paste0(strsplit(as.character(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==j]),'summary',fixed=TRUE)[[1]][1],"amendments?pageSize=250")
      r <- GET(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==j])
      status <- status_code(r) 
      if (status == 500 | status == 404 | status == 502){
        amendment_city_test <- NA
        amendment_city_2[[j]] <- amendment_city_test
        next()
      }else{
        webpage_bills_amendments <- read_html(url_bills_amendments)
        thatbill_amendment_links <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-heading amendment-heading"]//a//@href') %>% html_text()
        if (length(thatbill_amendment_links)==0){
          amendment_city_test <- NA
          amendment_city_2[[j]] <- amendment_city_test
          next()
        }else{
          thatbill_amendment_links <- thatbill_amendment_links[seq(1, length(thatbill_amendment_links), 2)] 
          for (i in 1:length(thatbill_amendment_links)){
            if (i == 1){
              next()
            }else{
              if (thatbill_amendment_links[i] == thatbill_amendment_links[i-1]){
                thatbill_amendment_links[i] <- thatbill_amendment_links[i]
                thatbill_amendment_links[i-1] <- NA 
              }else{
                next()
              }
            }
          }
          thatbill_amendment_links <- na.omit(thatbill_amendment_links)
          for (k in 1:length(thatbill_amendment_links)){
            if(grepl("http",as.character(thatbill_amendment_links[k]))==TRUE){
              thatbill_amendment_links[k] <- thatbill_amendment_links[k]
            }else{
              thatbill_amendment_links[k] <- NA
            }
          }
          thatbill_amendment_links <- na.omit(thatbill_amendment_links)
          thatbill_amendment_sponsorlinks <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-item"]//a//@href') %>% html_text()
          thatbill_amendment_rollcalllinks <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-item"]//a//@href') %>% html_text()
          if (length(thatbill_amendment_sponsorlinks)==0){
            amendment_city_test <- NA
            amendment_city_2[[j]] <- amendment_city_test
            next()
          }else{
            if(length(thatbill_amendment_rollcalllinks)==0){
              amendment_city_test <- NA
              amendment_city_2[[j]] <- amendment_city_test
              next()
            }else{
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(duplicated(thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(grepl("congressional-record",thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(grepl("member",thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              if(length(thatbill_amendment_rollcalllinks)==0){
                amendment_city_test <- NA
                amendment_city_2[[j]] <- amendment_city_test
                next()
              }else{
                thatbill_amendment_rollcalllinks_placeholder <- matrix(0, length(thatbill_amendment_rollcalllinks),2)
                for (i in 1:length(thatbill_amendment_rollcalllinks)){
                  thatbill_amendment_rollcalllinks_placeholder[i,1] <-  thatbill_amendment_rollcalllinks[i]
                  thatbill_amendment_rollcalllinks_placeholder[i,2] <-  strsplit(as.character(thatbill_amendment_rollcalllinks[i]),"/",fixed=TRUE)[[1]][5]
                }
                thatbill_amendment_rollcalllinks <- thatbill_amendment_rollcalllinks_placeholder
                for (i in 1:length(thatbill_amendment_rollcalllinks[,1])){
                  if(grepl("evs",thatbill_amendment_rollcalllinks[i,1])==TRUE){
                    thatbill_amendment_rollcalllinks[i,1] <- thatbill_amendment_rollcalllinks[i,1]
                    thatbill_amendment_rollcalllinks[i,2] <- thatbill_amendment_rollcalllinks[i+1,2]
                  }else{
                    thatbill_amendment_rollcalllinks[i,1] <-  NA
                    thatbill_amendment_rollcalllinks[i,2] <-  NA
                  }
                }
              }
              
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              
            }
          }
          for (i in 1:length(thatbill_amendment_sponsorlinks)){
            if(grepl("member",thatbill_amendment_sponsorlinks[i])==TRUE){
              thatbill_amendment_sponsorlinks[i] <- thatbill_amendment_sponsorlinks[i]
            }else{
              thatbill_amendment_sponsorlinks[i] <- NA
            }
          }
          thatbill_amendment_sponsorlinks <- na.omit(thatbill_amendment_sponsorlinks)
          thatbill_amendment_sponsorlinks <- thatbill_amendment_sponsorlinks[seq(1, length(thatbill_amendment_sponsorlinks), 2)]
          for (i in 1:length(thatbill_amendment_sponsorlinks)){
            thatbill_amendment_RepCode[i] <- strsplit(as.character(thatbill_amendment_sponsorlinks[i]),'/',fixed=TRUE)[[1]][4]
          }
          for (i in 1:length(thatbill_amendment_RepCode)){
            if (length(HouseResidents_115$HouseResidents_name[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
              if(length(SenateResidents_115$SenateResidents_name[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
                RepName[i] <- "former Congressional member"
              }else{
                RepName[i] <- SenateResidents_115$SenateResidents_name[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]]
              }
            }else{
              RepName[i] <- HouseResidents_115$HouseResidents_name[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]]
            }
          }
          for (i in 1:length(thatbill_amendment_RepCode)){
            if (length(HouseResidents_115$name_state[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
              if(length(SenateResidents_115$name_state[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
                name_state[i] <- "former Congressional member"
              }else{
                name_state[i] <- SenateResidents_115$name_state[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]]
              }
            }else{
              name_state[i] <- HouseResidents_115$name_state[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]]
            }
            
            
          }
          amendment_city_test <- matrix(0,length(thatbill_amendment_links),6)
          for (i in 1:length(thatbill_amendment_links)){
            amendment_city_test[i,1] <- thatbill_amendment_links[i]
            amendment_city_test[i,2] <- thatbill_amendment_RepCode[i]
            amendment_city_test[i,3] <- RepName[i]
            amendment_city_test[i,4] <- name_state[i]
            amendment_city_test[i,5] <- strsplit(strsplit(as.character(thatbill_amendment_links[i]),"/",fixed=TRUE)[[1]][7],"?",fixed=TRUE)[[1]][1]
            
          } 
          for (i in 1:length(amendment_city_test[,5])){
            if(length(thatbill_amendment_rollcalllinks[amendment_city_test[i,5]==thatbill_amendment_rollcalllinks[,2]][1])==0){
              amendment_city_test[i,6] <- NA
            }else{
              amendment_city_test[i,6] <- thatbill_amendment_rollcalllinks[amendment_city_test[i,5]==thatbill_amendment_rollcalllinks[,2]][1]
            }
            
          } 
          for (i in 1:length(amendment_city_test[,5])){
            if(is.na(amendment_city_test[i,6])){
              amendment_city_test[i,1] <- NA
              amendment_city_test[i,2] <- NA
              amendment_city_test[i,3] <- NA
              amendment_city_test[i,4] <- NA
              amendment_city_test[i,5] <- NA
            }else{
              next()
            }
            
          } 
          amendment_city_test <- na.omit(amendment_city_test)
          amendment_city_2[[j]] <- amendment_city_test
        }
        
        
      }
    }
  }
}





################################2. In the amendment city TOOOO!!!################################

amendment_city_too_2 <- replicate(length(BillScrips_2_115$HouseBills), list()) 


for (j in 1:length(BillScrips_2_115$HouseBills)){
  if (is.na(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==j])==TRUE){
    amendment_city_test <- NA
    amendment_city_too_2[[j]] <- amendment_city_test
    next()
  }else{
    if(length(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==j])==0){
      amendment_city_test <- NA
      amendment_city_too_2[[j]] <- amendment_city_test
      next()
    }else{
      thatbill_amendment_links <- NA
      thatbill_amendment_sponsorlinks <- NA
      thatbill_amendment_RepCode <- NA
      RepName <- NA
      name_state <- NA
      url_bills_amendments <- paste0(strsplit(as.character(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==j]),'summary',fixed=TRUE)[[1]][1],"amendments?pageSize=250&page=2")
      r <- GET(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==j])
      status <- status_code(r) 
      if (status == 500 | status == 404 | status == 502){
        amendment_city_test <- NA
        amendment_city_too_2[[j]] <- amendment_city_test
        next()
      }else{
        webpage_bills_amendments <- read_html(url_bills_amendments)
        thatbill_amendment_links <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-heading amendment-heading"]//a//@href') %>% html_text()
        if (length(thatbill_amendment_links)==0){
          amendment_city_test <- NA
          amendment_city_too_2[[j]] <- amendment_city_test
          next()
        }else{
          thatbill_amendment_links <- thatbill_amendment_links[seq(1, length(thatbill_amendment_links), 2)] 
          for (i in 1:length(thatbill_amendment_links)){
            if (i == 1){
              next()
            }else{
              if (thatbill_amendment_links[i] == thatbill_amendment_links[i-1]){
                thatbill_amendment_links[i] <- thatbill_amendment_links[i]
                thatbill_amendment_links[i-1] <- NA 
              }else{
                next()
              }
            }
          }
          thatbill_amendment_links <- na.omit(thatbill_amendment_links)
          thatbill_amendment_sponsorlinks <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-item"]//a//@href') %>% html_text()
          thatbill_amendment_rollcalllinks <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-item"]//a//@href') %>% html_text()
          if (length(thatbill_amendment_sponsorlinks)==0){
            amendment_city_test <- NA
            amendment_city_too_2[[j]] <- amendment_city_test
            next()
          }else{
            if(length(thatbill_amendment_rollcalllinks)==0){
              amendment_city_test <- NA
              amendment_city_too_2[[j]] <- amendment_city_test
              next()
            }else{
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(duplicated(thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(grepl("congressional-record",thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(grepl("member",thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(grepl("legislative/LIS",thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              if(length(thatbill_amendment_rollcalllinks)==0){
                amendment_city_test <- NA
                amendment_city_too_2[[j]] <- amendment_city_test
                next()
              }else{
                thatbill_amendment_rollcalllinks_placeholder <- matrix(0, length(thatbill_amendment_rollcalllinks),2)
                for (i in 1:length(thatbill_amendment_rollcalllinks)){
                  thatbill_amendment_rollcalllinks_placeholder[i,1] <-  thatbill_amendment_rollcalllinks[i]
                  thatbill_amendment_rollcalllinks_placeholder[i,2] <-  strsplit(as.character(thatbill_amendment_rollcalllinks[i]),"/",fixed=TRUE)[[1]][5]
                }
                thatbill_amendment_rollcalllinks <- thatbill_amendment_rollcalllinks_placeholder
                for (i in 1:length(thatbill_amendment_rollcalllinks[,1])){
                  if(grepl("evs",thatbill_amendment_rollcalllinks[i,1])==TRUE){
                    thatbill_amendment_rollcalllinks[i,1] <- thatbill_amendment_rollcalllinks[i,1]
                    thatbill_amendment_rollcalllinks[i,2] <- thatbill_amendment_rollcalllinks[i+1,2]
                  }else{
                    thatbill_amendment_rollcalllinks[i,1] <-  NA
                    thatbill_amendment_rollcalllinks[i,2] <-  NA
                  }
                }
              }
              
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              
            }
          }
          for (i in 1:length(thatbill_amendment_sponsorlinks)){
            if(grepl("member",thatbill_amendment_sponsorlinks[i])==TRUE){
              thatbill_amendment_sponsorlinks[i] <- thatbill_amendment_sponsorlinks[i]
            }else{
              thatbill_amendment_sponsorlinks[i] <- NA
            }
          }
          thatbill_amendment_sponsorlinks <- na.omit(thatbill_amendment_sponsorlinks)
          thatbill_amendment_sponsorlinks <- thatbill_amendment_sponsorlinks[seq(1, length(thatbill_amendment_sponsorlinks), 2)]
          for (i in 1:length(thatbill_amendment_sponsorlinks)){
            thatbill_amendment_RepCode[i] <- strsplit(as.character(thatbill_amendment_sponsorlinks[i]),'/',fixed=TRUE)[[1]][4]
          }
          for (i in 1:length(thatbill_amendment_RepCode)){
            if (length(HouseResidents_115$HouseResidents_name[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
              if(length(SenateResidents_115$SenateResidents_name[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
                RepName[i] <- "former Congressional member"
              }else{
                RepName[i] <- SenateResidents_115$SenateResidents_name[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]]
              }
            }else{
              RepName[i] <- HouseResidents_115$HouseResidents_name[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]]
            }
          }
          for (i in 1:length(thatbill_amendment_RepCode)){
            if (length(HouseResidents_115$name_state[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
              if(length(SenateResidents_115$name_state[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
                name_state[i] <- "former Congressional member"
              }else{
                name_state[i] <- SenateResidents_115$name_state[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]]
              }
            }else{
              name_state[i] <- HouseResidents_115$name_state[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]]
            }
            
            
          }
          amendment_city_test <- matrix(0,length(thatbill_amendment_links),6)
          for (i in 1:length(thatbill_amendment_links)){
            amendment_city_test[i,1] <- thatbill_amendment_links[i]
            amendment_city_test[i,2] <- thatbill_amendment_RepCode[i]
            amendment_city_test[i,3] <- RepName[i]
            amendment_city_test[i,4] <- name_state[i]
            amendment_city_test[i,5] <- strsplit(strsplit(as.character(thatbill_amendment_links[i]),"/",fixed=TRUE)[[1]][7],"?",fixed=TRUE)[[1]][1]
            
          } 
          for (i in 1:length(amendment_city_test[,5])){
            if(length(thatbill_amendment_rollcalllinks[amendment_city_test[i,5]==thatbill_amendment_rollcalllinks[,2]][1])==0){
              amendment_city_test[i,6] <- NA
            }else{
              amendment_city_test[i,6] <- thatbill_amendment_rollcalllinks[amendment_city_test[i,5]==thatbill_amendment_rollcalllinks[,2]][1]
            }
            
          } 
          for (i in 1:length(amendment_city_test[,5])){
            if(is.na(amendment_city_test[i,6])){
              amendment_city_test[i,1] <- NA
              amendment_city_test[i,2] <- NA
              amendment_city_test[i,3] <- NA
              amendment_city_test[i,4] <- NA
              amendment_city_test[i,5] <- NA
            }else{
              next()
            }
            
          } 
          amendment_city_test <- na.omit(amendment_city_test)
          amendment_city_too_2[[j]] <- amendment_city_test
        }
        
        
      }
    }
  }
}

for (j in 1:length(amendment_city_2)){
  if (length(amendment_city_too_2[[j]])==0){
    next()
  }else{
    if (is.na(amendment_city_too_2[[j]])){
      next()
    }else{
      if (length(amendment_city_2[[j]])==0){
        amendment_city_2[[j]] <- amendment_city_too_2[[j]]
      }else{
        if (is.na(amendment_city_2[[j]])){
          amendment_city_2[[j]] <- amendment_city_too_2[[j]]
        }else{
          if(sum(amendment_city_too_2[[j]][1,]==amendment_city_2[[j]][1,])==6){
            next()
          }else{
            amendment_city_2[[j]] <- rbind(amendment_city_2[[j]],amendment_city_too_2[[j]])
          }  
        }
      }
    }
  }
}

################################2. In the amendment city REMIXXXXX!!!################################


amendment_city_three_2 <- replicate(length(BillScrips_2_115$HouseBills), list()) 


for (j in 1:length(BillScrips_2_115$HouseBills)){
  if (is.na(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==j])==TRUE){
    amendment_city_test <- NA
    amendment_city_three_2[[j]] <- amendment_city_test
    next()
  }else{
    if(length(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==j])==0){
      amendment_city_test <- NA
      amendment_city_three_2[[j]] <- amendment_city_test
      next()
    }else{
      thatbill_amendment_links <- NA
      thatbill_amendment_sponsorlinks <- NA
      thatbill_amendment_RepCode <- NA
      RepName <- NA
      name_state <- NA
      url_bills_amendments <- paste0(strsplit(as.character(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==j]),'summary',fixed=TRUE)[[1]][1],"amendments?pageSize=250&page=3")
      r <- GET(BillScrips_2_115$thaturl[BillScrips_2_115$BillsNos==j])
      status <- status_code(r) 
      if (status == 500 | status == 404 | status == 502){
        amendment_city_test <- NA
        amendment_city_three_2[[j]] <- amendment_city_test
        next()
      }else{
        webpage_bills_amendments <- read_html(url_bills_amendments)
        thatbill_amendment_links <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-heading amendment-heading"]//a//@href') %>% html_text()
        if (length(thatbill_amendment_links)==0){
          amendment_city_test <- NA
          amendment_city_three_2[[j]] <- amendment_city_test
          next()
        }else{
          thatbill_amendment_links <- thatbill_amendment_links[seq(1, length(thatbill_amendment_links), 2)] 
          for (i in 1:length(thatbill_amendment_links)){
            if (i == 1){
              next()
            }else{
              if (thatbill_amendment_links[i] == thatbill_amendment_links[i-1]){
                thatbill_amendment_links[i] <- thatbill_amendment_links[i]
                thatbill_amendment_links[i-1] <- NA 
              }else{
                next()
              }
            }
          }
          thatbill_amendment_links <- na.omit(thatbill_amendment_links)
          thatbill_amendment_sponsorlinks <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-item"]//a//@href') %>% html_text()
          thatbill_amendment_rollcalllinks <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-item"]//a//@href') %>% html_text()
          if (length(thatbill_amendment_sponsorlinks)==0){
            amendment_city_test <- NA
            amendment_city_three_2[[j]] <- amendment_city_test
            next()
          }else{
            if(length(thatbill_amendment_rollcalllinks)==0){
              amendment_city_test <- NA
              amendment_city_three_2[[j]] <- amendment_city_test
              next()
            }else{
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(duplicated(thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(grepl("congressional-record",thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(grepl("member",thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              for (i in 1:length(thatbill_amendment_rollcalllinks)){
                if(grepl("legislative/LIS",thatbill_amendment_rollcalllinks)[i]==TRUE){
                  thatbill_amendment_rollcalllinks[i] <- NA
                }else{
                  thatbill_amendment_rollcalllinks[i] <- thatbill_amendment_rollcalllinks[i]
                }
              }
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              if(length(thatbill_amendment_rollcalllinks)==0){
                amendment_city_test <- NA
                amendment_city_three_2[[j]] <- amendment_city_test
                next()
              }else{
                thatbill_amendment_rollcalllinks_placeholder <- matrix(0, length(thatbill_amendment_rollcalllinks),2)
                for (i in 1:length(thatbill_amendment_rollcalllinks)){
                  thatbill_amendment_rollcalllinks_placeholder[i,1] <-  thatbill_amendment_rollcalllinks[i]
                  thatbill_amendment_rollcalllinks_placeholder[i,2] <-  strsplit(as.character(thatbill_amendment_rollcalllinks[i]),"/",fixed=TRUE)[[1]][5]
                }
                thatbill_amendment_rollcalllinks <- thatbill_amendment_rollcalllinks_placeholder
                for (i in 1:length(thatbill_amendment_rollcalllinks[,1])){
                  if(grepl("evs",thatbill_amendment_rollcalllinks[i,1])==TRUE){
                    thatbill_amendment_rollcalllinks[i,1] <- thatbill_amendment_rollcalllinks[i,1]
                    thatbill_amendment_rollcalllinks[i,2] <- thatbill_amendment_rollcalllinks[i+1,2]
                  }else{
                    thatbill_amendment_rollcalllinks[i,1] <-  NA
                    thatbill_amendment_rollcalllinks[i,2] <-  NA
                  }
                }
              }
              
              thatbill_amendment_rollcalllinks <- na.omit(thatbill_amendment_rollcalllinks)
              
            }
          }
          for (i in 1:length(thatbill_amendment_sponsorlinks)){
            if(grepl("member",thatbill_amendment_sponsorlinks[i])==TRUE){
              thatbill_amendment_sponsorlinks[i] <- thatbill_amendment_sponsorlinks[i]
            }else{
              thatbill_amendment_sponsorlinks[i] <- NA
            }
          }
          thatbill_amendment_sponsorlinks <- na.omit(thatbill_amendment_sponsorlinks)
          thatbill_amendment_sponsorlinks <- thatbill_amendment_sponsorlinks[seq(1, length(thatbill_amendment_sponsorlinks), 2)]
          for (i in 1:length(thatbill_amendment_sponsorlinks)){
            thatbill_amendment_RepCode[i] <- strsplit(as.character(thatbill_amendment_sponsorlinks[i]),'/',fixed=TRUE)[[1]][4]
          }
          for (i in 1:length(thatbill_amendment_RepCode)){
            if (length(HouseResidents_115$HouseResidents_name[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
              if(length(SenateResidents_115$SenateResidents_name[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
                RepName[i] <- "former Congressional member"
              }else{
                RepName[i] <- SenateResidents_115$SenateResidents_name[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]]
              }
            }else{
              RepName[i] <- HouseResidents_115$HouseResidents_name[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]]
            }
          }
          for (i in 1:length(thatbill_amendment_RepCode)){
            if (length(HouseResidents_115$name_state[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
              if(length(SenateResidents_115$name_state[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]])==0){
                name_state[i] <- "former Congressional member"
              }else{
                name_state[i] <- SenateResidents_115$name_state[SenateResidents_115$code_link==thatbill_amendment_RepCode[i]]
              }
            }else{
              name_state[i] <- HouseResidents_115$name_state[HouseResidents_115$code_link==thatbill_amendment_RepCode[i]]
            }
            
            
          }
          amendment_city_test <- matrix(0,length(thatbill_amendment_links),6)
          for (i in 1:length(thatbill_amendment_links)){
            amendment_city_test[i,1] <- thatbill_amendment_links[i]
            amendment_city_test[i,2] <- thatbill_amendment_RepCode[i]
            amendment_city_test[i,3] <- RepName[i]
            amendment_city_test[i,4] <- name_state[i]
            amendment_city_test[i,5] <- strsplit(strsplit(as.character(thatbill_amendment_links[i]),"/",fixed=TRUE)[[1]][7],"?",fixed=TRUE)[[1]][1]
            
          } 
          for (i in 1:length(amendment_city_test[,5])){
            if(length(thatbill_amendment_rollcalllinks[amendment_city_test[i,5]==thatbill_amendment_rollcalllinks[,2]][1])==0){
              amendment_city_test[i,6] <- NA
            }else{
              amendment_city_test[i,6] <- thatbill_amendment_rollcalllinks[amendment_city_test[i,5]==thatbill_amendment_rollcalllinks[,2]][1]
            }
            
          } 
          for (i in 1:length(amendment_city_test[,5])){
            if(is.na(amendment_city_test[i,6])){
              amendment_city_test[i,1] <- NA
              amendment_city_test[i,2] <- NA
              amendment_city_test[i,3] <- NA
              amendment_city_test[i,4] <- NA
              amendment_city_test[i,5] <- NA
            }else{
              next()
            }
            
          } 
          amendment_city_test <- na.omit(amendment_city_test)
          amendment_city_three_2[[j]] <- amendment_city_test
        }
        
        
      }
    }
  }
}

for (j in 1:length(amendment_city_2)){
  if (length(amendment_city_three_2[[j]])==0){
    next()
  }else{
    if (is.na(amendment_city_three_2[[j]])){
      next()
    }else{
      if (length(amendment_city_2[[j]])==0){
        amendment_city_2[[j]] <- amendment_city_three_2[[j]]
      }else{
        if (is.na(amendment_city_2[[j]])){
          amendment_city_2[[j]] <- amendment_city_three_2[[j]]
        }else{
          if(sum(amendment_city_three_2[[j]][1,]==amendment_city_2[[j]][1,])==6){
            next()
          }else{
            amendment_city_2[[j]] <- rbind(amendment_city_2[[j]],amendment_city_three_2[[j]])
          }  
        }
      }
    }
  }
}






################################2. amendment link################################


BillScrips_2_115$amendmentlink <- NA
BillScrips_2_115$amendmentsponsorcode <- NA
numbers_amendments <- vector("list",length(BillScrips_2_115$HouseBillName_2))


for (i in 1:length(BillScrips_2_115$HouseBillName_2)){
  numbers_amendments_placeholder <- NA
  if(length(amendment_city_2[[i]])==0){
    numbers_amendments[[i]] <- NA
    next()
  }else{
    if(is.na(amendment_city_2[[i]])){
      numbers_amendments[[i]] <- NA
      next()
    }else{
      for (j in 1:length(amendment_city_2[[i]][,1])){
        hi_america <- amendment_city_2[[i]]
        hi_america[,4] <- gsub('[[:punct:]]',' ',hi_america[,4])
        if(length(BillScrips_2_115$HouseBillsDescriptions[BillScrips_2_115$BillsNos==i])==0){
          numbers_amendments_placeholder[j] <- NA
          next()
        }else{
          descrip_placeholder <- gsub('[[:punct:]]',' ',as.character(BillScrips_2_115$HouseBillsDescriptions_2[BillScrips_2_115$BillsNos==i]))
          if(grepl(as.character(hi_america[j,4]),as.character(descrip_placeholder))==TRUE){
            numbers_amendments_placeholder[j] <- j
          }else{
            numbers_amendments_placeholder[j] <- NA
          }
        }
      }
      numbers_amendments[[i]] <- numbers_amendments_placeholder
    }
  }
}

for(i in 1:length(numbers_amendments)){
  if(length(numbers_amendments[[i]])==1){
    next()
  }else{
    numbers_amendments[[i]] <- na.omit(numbers_amendments[[i]])
  }
}

BillScrips_2_115$HouseBillsDescriptions_2 <- gsub('[[:punct:]]',' ',as.character(BillScrips_2_115$HouseBillsDescriptions_2))
for(i in 1:length(amendment_city_2)){
  if(length(amendment_city_2[[i]])==0){
    next()
  }else{
    if(is.na(amendment_city_2[[i]])){
      next()
    }else{
      amendment_city_2[[i]][,4] <- gsub('[[:punct:]]',' ',as.character(amendment_city_2[[i]][,4]))
    }
    }
}

for(i in 1:length(numbers_amendments)){
  if(length(numbers_amendments[[i]])==0){
    BillScrips_2_115$amendmentlink[BillScrips_2_115$BillsNos==i] <- NA
    BillScrips_2_115$amendmentsponsorcode[BillScrips_2_115$BillsNos==i] <- NA
  }else{
    if(length(numbers_amendments[[i]])==1){
      if(is.na(numbers_amendments[[i]])){
        BillScrips_2_115$amendmentlink[BillScrips_2_115$BillsNos==i] <- NA
        BillScrips_2_115$amendmentsponsorcode[BillScrips_2_115$BillsNos==i] <- NA
        next()
      }else{
        BillScrips_2_115$amendmentlink[BillScrips_2_115$BillsNos==i] <- amendment_city_2[[i]][numbers_amendments[[i]][1],1]
        BillScrips_2_115$amendmentsponsorcode[BillScrips_2_115$BillsNos==i] <- amendment_city_2[[i]][numbers_amendments[[i]][1],2]
      }
    }else{
      keep_track_1 <- BillScrips_2_115$BillsNos[BillScrips_2_115$HouseBillName_2==BillScrips_2_115$HouseBillName_2[BillScrips_2_115$BillsNos==i]]
      keep_track_1 <- na.omit(keep_track_1)
      keep_track <- BillScrips_2_115$BillsNos[grepl(amendment_city_2[[i]][numbers_amendments[[i]][1],4],BillScrips_2_115$HouseBillsDescriptions_2)]
      keep_track <- intersect(keep_track_1,keep_track)
      hi_america_2 <- numbers_amendments[[i]]
      data_frame <- data.frame(keep_track,hi_america_2)
      number_i_want <- data_frame$`hi_america_2`[data_frame$keep_track==i]
      BillScrips_2_115$amendmentlink[BillScrips_2_115$BillsNos==i] <- amendment_city_2[[i]][number_i_want,1]
      BillScrips_2_115$amendmentsponsorcode[BillScrips_2_115$BillsNos==i] <- amendment_city_2[[i]][number_i_want,2]
    }
  }
}



amendment_check_2 <- data.frame(BillScrips_2_115$amendmentlink[grepl("Amendment",BillScrips_2_115$HouseBillsDescriptions)],BillScrips_2_115$HouseBillsDescriptions[grepl("Amendment",BillScrips_2_115$HouseBillsDescriptions)],BillScrips_2_115$BillsNos[grepl("Amendment",BillScrips_2_115$HouseBillsDescriptions)])


################################1. report to the office at once################################




for (k in 1:length(BillScrips_1_115$HouseBills)){
  if (length(BillScrips_1_115$Bill_committee_report[k])==0 || is.na(BillScrips_1_115$Bill_committee_report[k])){
    BillScrips_1_115$Bill_committee_report_text[k] <- NA
  }else{
    url_committeereport <- paste0("https://www.congress.gov",as.character(BillScrips_1_115$Bill_committee_report[k]))
    webpage_committeereport <- read_html(url_committeereport)
    that_committeereport <- webpage_committeereport %>% html_nodes(xpath='//pre') %>% html_text()
    that_committeereport <- str_replace_all(that_committeereport,"[[:punct:]]"," ")
    BillScrips_1_115$Bill_committee_report_text[k] <- that_committeereport
  }
}

for (k in 1:length(BillScrips_1_115$HouseBills)){
  if (length(BillScrips_1_115$amendmentlink[k])==0 || is.na(BillScrips_1_115$amendmentlink[k])){
    BillScrips_1_115$that_amendment_text[k] <- NA
  }else{
    url_amendment <- as.character(BillScrips_1_115$amendmentlink[k])
    webpage_amendment <- read_html(url_amendment)
    that_amendment_text <- webpage_amendment %>% html_nodes(xpath='//div[@class="main-wrapper"]') %>% html_text()
    that_amendment_text <- str_replace_all(that_amendment_text,"[[:punct:]]"," ")
    BillScrips_1_115$that_amendment_text[k] <- that_amendment_text
  }
}













################################Bill's text analyzer################################
for (em in 1:length(BillScrips_1_115$thatbill_text)){
  if (is.na(BillScrips_1_115$thatbill_text[em])){
    next()
  }else{
BillScrips_1_115$thatbill_text[em] <- str_replace_all(BillScrips_1_115$thatbill_text[em],"[[:punct:]]"," ")
thatbill_text <- tibble(line = 1, text = BillScrips_1_115$thatbill_text[em])

thisbill_text <- thatbill_text %>%
  unnest_tokens(quigram, text, token = "ngrams", n = 5) %>%
  separate(quigram, c("word1", "word2", "word3","word4","word5"), sep = " ")


phrase_out <- NA
for (i in 1:length(thisbill_text$word1)){
  if (sum(is.element(thisbill_text$word1[i],stop_words$word),is.element(thisbill_text$word2[i],stop_words$word),is.element(thisbill_text$word3[i],stop_words$word),is.element(thisbill_text$word4[i],stop_words$word),is.element(thisbill_text$word5[i],stop_words$word))>=2){
    phrase_out[i] <- NA
  }else{
    phrase_out[i] <- paste0(thisbill_text$word1[i]," ",thisbill_text$word2[i]," ",thisbill_text$word3[i]," ",thisbill_text$word4[i]," ",thisbill_text$word5[i])
  }
}

phase_out <- which(is.na(phrase_out),arr.ind = T)

thisbill_text <- thisbill_text[-phase_out,]
phrase_out <- phrase_out[-phase_out]

phraZee <- NA
for (i in 1:length(thisbill_text$word1)){
  phraZee[i] <- paste0(thisbill_text$word1[i]," ",thisbill_text$word2[i]," ",thisbill_text$word3[i]," ",thisbill_text$word4[i]," ",thisbill_text$word5[i])
}
for (i in 1:length(phraZee)){
  if(duplicated(phraZee)[i]==TRUE){
    phraZee[i] <- NA
  }else{
    phraZee[i] <- phraZee[i]
  }
}


thisbill_text <- thisbill_text[-which(is.na(phraZee),arr.ind = T),]
phraZee <- na.omit(phraZee)


america <- vector("list",length(thisbill_text$word1))
for (i in 1:length(thisbill_text$word1)){
  america[[i]] <- matrix(0,5,length(thisbill_text$word1))
}
america_data <- vector("list",length(thisbill_text$word1))
for (i in 1:length(thisbill_text$word1)){
  america_data[[i]] <- matrix(0,5,length(thisbill_text$word1))
}


for (j in 1:length(thisbill_text$word1)){
  for(i in 1:length(phraZee)){
    for(k in 1:(ncol(thisbill_text)-1)){
      america[[j]][k,i] <- max(1-(levenshtein.distance(paste0(as.character(thisbill_text[i,k+1])),c(strsplit(phraZee[j]," ")[[1]][1],strsplit(phraZee[j]," ")[[1]][2],strsplit(phraZee[j]," ")[[1]][3],strsplit(phraZee[j]," ")[[1]][4],strsplit(phraZee[j]," ")[[1]][5]))/(nchar(phraZee[j]))))
    }
  }
}

for (j in 1:length(america)){
  america_data_1 <- data.frame(america[[j]])
  acirema <- which(america_data_1>=.99,arr.ind = T)
  acirema <- data.frame(acirema)
  for (i in 1:length(america_data_1)){
    if(sum(acirema$col == i)==5){
      next()
    }else{
      if(sum(acirema$col == i)>=3){
        america_data_1[,i] <- 1
      }else{
        america_data_1[,i] <- 0
      }
      
    }
    
  }
  america_data[[j]] <- america_data_1 
}






keep_away <- matrix(1:length(america_data),length(america_data),2)
for(j in 1:length(america_data)){
  if(sum(america_data[[j]]==5)){
    keep_away[j,2] <- "keep"
  }else{
      acirema_test <- which(america_data[[j]]==1,arr.ind = T)
      numbers <- unique(acirema_test[,2])
      big_small <- vector("list",length(numbers))
      for (k in 1:length(numbers)){
        big_small[[k]] <- nchar(paste0(thisbill_text$word1[numbers[k]],thisbill_text$word2[numbers[k]],thisbill_text$word3[numbers[k]],thisbill_text$word4[numbers[k]],thisbill_text$word5[numbers[k]]))
      }
      big_small <- unlist(big_small)
      ifoundu_1 <- which(big_small==max(big_small),arr.ind = T)
      ifoundu_2 <- which(big_small!=max(big_small),arr.ind = T)
      if (length(ifoundu_1)>1){
        big_pot <- vector("list",length(ifoundu_1))
        for (k in 1:length(ifoundu_1)){
          big_pot[[k]] <- sum(is.element(thisbill_text$word1[numbers[ifoundu_1[k]]],stop_words$word),is.element(thisbill_text$word2[numbers[ifoundu_1[k]]],stop_words$word),is.element(thisbill_text$word3[numbers[ifoundu_1[k]]],stop_words$word),is.element(thisbill_text$word4[numbers[ifoundu_1[k]]],stop_words$word),is.element(thisbill_text$word5[numbers[ifoundu_1[k]]],stop_words$word))
        }
        big_pot <- unlist(big_pot)
        ifoundu_master <- ifoundu_1
        ifoundu_1_1 <- which(big_pot==min(big_pot),arr.ind = T)
        if(length(ifoundu_1_1)>1){
          ifoundu_1_1 <- ifoundu_1_1[1]
          ifoundu_1 <- ifoundu_master[ifoundu_1_1]
          ifoundu_2 <- append(ifoundu_2,ifoundu_master[-ifoundu_1_1[1]])
        }else{
          ifoundu_2_1 <- which(big_pot!=min(big_pot),arr.ind = T) 
          ifoundu_1 <- ifoundu_master[ifoundu_1_1]
          ifoundu_2 <- append(ifoundu_2,ifoundu_master[ifoundu_2_1])
        }
        ifoundu_keep <- numbers[ifoundu_1]
        ifoundu_away <- numbers[ifoundu_2]
      }else{
        ifoundu_keep <- numbers[ifoundu_1]
        ifoundu_away <- numbers[ifoundu_2]
      }
      if(keep_away[ifoundu_keep,2]=="away"){
        keep_away[ifoundu_keep,2] <- "away"
      }else{
        keep_away[ifoundu_keep,2] <- "keep"
      }
      keep_away[ifoundu_away,2] <- "away"
    } 
    
  }





keep_away

thisbill_text <- thisbill_text[-which(keep_away[,2]=="away",arr.ind = T),]

phraZee <- NA
for (i in 1:length(thisbill_text$word1)){
  phraZee[i] <- paste0(thisbill_text$word1[i]," ",thisbill_text$word2[i]," ",thisbill_text$word3[i]," ",thisbill_text$word4[i]," ",thisbill_text$word5[i])
}


america <- vector("list",length(thisbill_text$word1))
for (i in 1:length(thisbill_text$word1)){
  america[[i]] <- matrix(0,5,length(thisbill_text$word1))
}
america_data <- vector("list",length(thisbill_text$word1))
for (i in 1:length(thisbill_text$word1)){
  america_data[[i]] <- matrix(0,5,length(thisbill_text$word1))
}


for (j in 1:length(thisbill_text$word1)){
  for(i in 1:length(phraZee)){
    for(k in 1:(ncol(thisbill_text)-1)){
      america[[j]][k,i] <- max(1-(levenshtein.distance(paste0(as.character(thisbill_text[i,k+1])),c(strsplit(phraZee[j]," ")[[1]][1],strsplit(phraZee[j]," ")[[1]][2],strsplit(phraZee[j]," ")[[1]][3],strsplit(phraZee[j]," ")[[1]][4],strsplit(phraZee[j]," ")[[1]][5]))/(nchar(phraZee[j]))))
    }
  }
}

for (j in 1:length(america)){
  america_data_1 <- data.frame(america[[j]])
  acirema <- which(america_data_1>=.99,arr.ind = T)
  acirema <- data.frame(acirema)
  for (i in 1:length(america_data_1)){
    if(sum(acirema$col == i)==5){
      next()
    }else{
      if(sum(acirema$col == i)>=2){
        america_data_1[,i] <- 1
      }else{
        america_data_1[,i] <- 0
      }
      
    }
    
  }
  america_data[[j]] <- america_data_1 
}






keep_away <- matrix(1:length(america_data),length(america_data),2)
for(j in 1:length(america_data)){
  if(sum(america_data[[j]]==5)){
    keep_away[j,2] <- "keep"
  }else{
      acirema_test <- which(america_data[[j]]==1,arr.ind = T)
      numbers <- unique(acirema_test[,2])
      big_small <- vector("list",length(numbers))
      for (k in 1:length(numbers)){
        big_small[[k]] <- nchar(paste0(thisbill_text$word1[numbers[k]],thisbill_text$word2[numbers[k]],thisbill_text$word3[numbers[k]],thisbill_text$word4[numbers[k]],thisbill_text$word5[numbers[k]]))
      }
      big_small <- unlist(big_small)
      ifoundu_1 <- which(big_small==max(big_small),arr.ind = T)
      ifoundu_2 <- which(big_small!=max(big_small),arr.ind = T)
      if (length(ifoundu_1)>1){
        big_pot <- vector("list",length(ifoundu_1))
        for (k in 1:length(ifoundu_1)){
          big_pot[[k]] <- sum(is.element(thisbill_text$word1[numbers[ifoundu_1[k]]],stop_words$word),is.element(thisbill_text$word2[numbers[ifoundu_1[k]]],stop_words$word),is.element(thisbill_text$word3[numbers[ifoundu_1[k]]],stop_words$word),is.element(thisbill_text$word4[numbers[ifoundu_1[k]]],stop_words$word),is.element(thisbill_text$word5[numbers[ifoundu_1[k]]],stop_words$word))
        }
        big_pot <- unlist(big_pot)
        ifoundu_master <- ifoundu_1
        ifoundu_1_1 <- which(big_pot==min(big_pot),arr.ind = T)
        if(length(ifoundu_1_1)>1){
          ifoundu_1_1 <- ifoundu_1_1[1]
          ifoundu_1 <- ifoundu_master[ifoundu_1_1]
          ifoundu_2 <- append(ifoundu_2,ifoundu_master[-ifoundu_1_1[1]])
        }else{
          ifoundu_2_1 <- which(big_pot!=min(big_pot),arr.ind = T) 
          ifoundu_1 <- ifoundu_master[ifoundu_1_1]
          ifoundu_2 <- append(ifoundu_2,ifoundu_master[ifoundu_2_1])
        }
        ifoundu_keep <- numbers[ifoundu_1]
        ifoundu_away <- numbers[ifoundu_2]
      }else{
        ifoundu_keep <- numbers[ifoundu_1]
        ifoundu_away <- numbers[ifoundu_2]
      }
      if(keep_away[ifoundu_keep,2]=="away"){
        keep_away[ifoundu_keep,2] <- "away"
      }else{
        keep_away[ifoundu_keep,2] <- "keep"
      }
      keep_away[ifoundu_away,2] <- "away"
    } 
    
  }





keep_away

thisbill_text <- thisbill_text[-which(keep_away[,2]=="away",arr.ind = T),]


  }

}


.





















################################the lobby of the House################################

url_replib <- "https://projects.propublica.org/represent/members/115/house"
webpage_replib <- read_html(url_replib)
HouseResidents <- webpage_replib %>% html_nodes(xpath='//td//a[@href]') %>% html_text()
HouseResidents <- trimws(HouseResidents, which = "left")
HouseResident_Links <- webpage_replib %>% html_nodes(xpath='//td//a[@href]//@href') %>% html_text()
HousePositions <- webpage_replib %>% html_nodes(xpath='//small[@class="block gray uppercase"]') %>% html_text()
sample_library <- read_html(url_two[1])
sample_house <- sample_library %>% html_nodes(xpath='//div[@id="roll-call-tables"]//a[@href]') %>% html_text()
sample_house_district <- sample_library %>% html_nodes(xpath='//div[@id="roll-call-tables"]//td[@class="nowrap text-center"]') %>% html_text()
sample_house <- as.character(sample_house)
sample_house_lib <- data.frame(sample_house, sample_house_district)
sample_house_lib$sample_house <- as.character(sample_house_lib$sample_house)
HouseResidents_name <- NA
HouseResidents_sponsoredbills <- NA
HouseResidents_cosponsoredbills <- NA
HouseResidents_votesagainstparty <- NA
HouseResidents_missedvotes <- NA
for (i in 1:length(HouseResidents)){
  if(grepl(" ",HouseResidents[i])==TRUE){
    if(grepl(" ",HouseResidents[i+3])==TRUE){
      HouseResidents_name[i] <- HouseResidents[i]
      HouseResidents_sponsoredbills[i] <- HouseResidents[i+1]
      HouseResidents_cosponsoredbills[i] <- HouseResidents[i+2]
      HouseResidents_votesagainstparty[i] <- "I am not"
      HouseResidents_missedvotes[i] <- "allowed to vote"
    }else{
      HouseResidents_name[i] <- HouseResidents[i]
      HouseResidents_sponsoredbills[i] <- HouseResidents[i+1]
      HouseResidents_cosponsoredbills[i] <- HouseResidents[i+2]
      HouseResidents_votesagainstparty[i] <- HouseResidents[i+3]
      HouseResidents_missedvotes[i] <- HouseResidents[i+4]
    }
  }else{
    next()
  }
}

HouseResidents_name <- na.omit(HouseResidents_name)
HouseResidents_sponsoredbills <- na.omit(HouseResidents_sponsoredbills)
HouseResidents_cosponsoredbills <- na.omit(HouseResidents_cosponsoredbills)
HouseResidents_votesagainstparty <- na.omit(HouseResidents_votesagainstparty)
HouseResidents_missedvotes <- na.omit(HouseResidents_missedvotes)
HouseResidents_115 <- data.frame(HouseResidents_name,HouseResidents_sponsoredbills,HouseResidents_cosponsoredbills,HouseResidents_votesagainstparty,HouseResidents_missedvotes)

HouseResidents_115$HouseResidents_name <- as.character(HouseResidents_115$HouseResidents_name)
HouseResidents_115 <- left_join(HouseResidents_115,sample_house_lib, by = c("HouseResidents_name" = "sample_house"))

HouseResidents_name_links <- NA
HouseResidents_sponsoredbills_links <- NA
HouseResidents_cosponsoredbills_links <- NA
HouseResidents_votesagainstparty_links <- NA
HouseResidents_missedvotes_links <- NA
for (i in 1:length(HouseResident_Links)){
  if(grepl("bills-sponsored",HouseResident_Links[i])==TRUE){
    if(grepl("bills-sponsored",HouseResident_Links[i+3])==TRUE){
      HouseResidents_name_links[i] <- HouseResident_Links[i-1]
      HouseResidents_sponsoredbills_links[i] <- HouseResident_Links[i]
      HouseResidents_cosponsoredbills_links[i] <- HouseResident_Links[i+1]
      HouseResidents_votesagainstparty_links[i] <- "I am not"
      HouseResidents_missedvotes_links[i] <- "allowed to vote"
    }else{
      HouseResidents_name_links[i] <- HouseResident_Links[i-1]
      HouseResidents_sponsoredbills_links[i] <- HouseResident_Links[i]
      HouseResidents_cosponsoredbills_links[i] <- HouseResident_Links[i+1]
      HouseResidents_votesagainstparty_links[i] <- HouseResident_Links[i+2]
      HouseResidents_missedvotes_links[i] <- HouseResident_Links[i+3]
    }
  }else{
    next()
  }
}

HouseResidents_name_links <- na.omit(HouseResidents_name_links)
HouseResidents_sponsoredbills_links <- na.omit(HouseResidents_sponsoredbills_links)
HouseResidents_cosponsoredbills_links <- na.omit(HouseResidents_cosponsoredbills_links)
HouseResidents_votesagainstparty_links <- na.omit(HouseResidents_votesagainstparty_links)
HouseResidents_missedvotes_links <- na.omit(HouseResidents_missedvotes_links)
HouseResidents_115_links <- data.frame(HouseResidents_name_links,HouseResidents_sponsoredbills_links,HouseResidents_cosponsoredbills_links,HouseResidents_votesagainstparty_links,HouseResidents_missedvotes_links)
HouseResidents_115 <- data.frame(HouseResidents_115,HouseResidents_115_links)
HouseResidents_115$code_link <- NA
for (i in 1:length(HouseResidents_115$HouseResidents_name_links)){
  HouseResidents_115$code_link[i] <- strsplit(as.character(HouseResidents_115$HouseResidents_name_links[i]),'/',fixed=TRUE)[[1]][4]
  HouseResidents_115$code_link[i] <- strsplit(as.character(HouseResidents_115$code_link[i]),'-',fixed=TRUE)[[1]][1]
}

HouseResidents_115$HouseResidents_name[is.na(HouseResidents_115$sample_house_district)]
HouseResidents_115$sample_house_district <- as.character(HouseResidents_115$sample_house_district)

HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Alma Adams"] <- "NC-12"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Troy Balderson"] <- "OH-12"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Madeleine Z. Bordallo"] <- "Guam-1"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Bradley Byrne"] <- "AL-1"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Tony Cárdenas"] <- "CA-29"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="André Carson"] <- "IN-7"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Katherine Clark"] <- "MA-5"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Michael Cloud"] <- "TX-27"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Tom Cole"] <- "OK-4"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Joseph Crowley"] <- "NY-14"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="John Curtis"] <- "UT-3"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Ron Estes"] <- "KS-4"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Dwight Evans"] <- "PA-2"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Greg Gianforte"] <- "MT-1"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Jimmy Gomez"] <- "CA-34"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Jenniffer González-Colón"] <- "Puerto Rico"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Karen Handel"] <- "GA-6"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Kevin Hern"] <- "OK-1"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Sheila Jackson Lee"] <- "TX-18"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Brenda Jones"] <- "MI-13"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Trent Kelly"] <- "MS-1"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Robin Kelly"] <- "IL-2"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Conor Lamb"] <- "PA-18"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Debbie Lesko"] <- "AZ-8"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="John Lewis"] <- "GA-5"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Joe Morelle"] <- "NY-25"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Jerrold Nadler"] <- "NY-10"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Ralph Norman"] <- "SC-5"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Eleanor Holmes Norton"] <- "D.C.-1"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Stacey Plaskett"] <- "U.S. Virgin Islands-1"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Amata Coleman Radewagen"] <- "American Samoa-1"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Edward Royce"] <- "CA-39"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Gregorio Kilili Camacho Sablan"] <- "Northern Mariana Islands-1"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Mary Gay Scanlon"] <- "PA-5"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Jason Smith"] <- "MO-8"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Mark Walker"] <- "NC-6"
HouseResidents_115$sample_house_district[HouseResidents_115$HouseResidents_name=="Susan Wild"] <- "PA-7"






for (i in 1:length(HouseResidents_115$sample_house_district)){
  if (is.na(HouseResidents_115$sample_house_district[i])){
    next()
  }else{
    HouseResidents_115$state[i] <- strsplit(as.character(HouseResidents_115$sample_house_district[i]),'-',fixed=TRUE)[[1]][1]
    HouseResidents_115$district[i] <- strsplit(as.character(HouseResidents_115$sample_house_district[i]),'-',fixed=TRUE)[[1]][2] 
    
  }
}




state_code <- data.frame(state.abb,state.name)
for (i in 1:length(HouseResidents_115$sample_house_district)){
  if (is.na(HouseResidents_115$sample_house_district[i])){
    next()
  }else{
    if(length(as.character(state_code$state.name[HouseResidents_115$state[i]==state_code$state.abb]))==0){
      HouseResidents_115$state_name[i] <- HouseResidents_115$state[i]
      }else{
    HouseResidents_115$state_name[i] <- as.character(state_code$state.name[HouseResidents_115$state[i]==state_code$state.abb])
    }
  }
}

for (i in 1:length(HouseResidents_115$state_name)){
  if(strsplit(as.character(HouseResidents_115$HouseResidents_name[i]),' ',fixed=TRUE)[[1]][length(unlist(strsplit(as.character(HouseResidents_115$HouseResidents_name[i]),' ',fixed=TRUE)[[1]]))]=="Jr." || strsplit(as.character(HouseResidents_115$HouseResidents_name[i]),' ',fixed=TRUE)[[1]][length(unlist(strsplit(as.character(HouseResidents_115$HouseResidents_name[i]),' ',fixed=TRUE)[[1]]))]=="II"){
    HouseResidents_115$name_state[i] <- paste0(strsplit(as.character(HouseResidents_115$HouseResidents_name[i]),' ',fixed=TRUE)[[1]][length(unlist(strsplit(as.character(HouseResidents_115$HouseResidents_name[i]),' ',fixed=TRUE)[[1]]))-1]," ","of"," ",as.character(HouseResidents_115$state_name[i]))
  }else{
    HouseResidents_115$name_state[i] <- paste0(strsplit(as.character(HouseResidents_115$HouseResidents_name[i]),' ',fixed=TRUE)[[1]][length(unlist(strsplit(as.character(HouseResidents_115$HouseResidents_name[i]),' ',fixed=TRUE)[[1]]))]," ","of"," ",as.character(HouseResidents_115$state_name[i]))
  }
}







.



















################################Mitch McConnell is a Bitch################################

url_senators <- "https://projects.propublica.org/represent/members/115/senate"
webpage_senators <- read_html(url_senators)
SenateResidents <- webpage_senators %>% html_nodes(xpath='//td//a[@href]') %>% html_text()
SenateResidents <- trimws(SenateResidents, which = "left")
SenateResident_Links <- webpage_senators %>% html_nodes(xpath='//td//a[@href]//@href') %>% html_text()
SenatePositions <- webpage_senators %>% html_nodes(xpath='//small[@class="block gray uppercase"]') %>% html_text()
sample_library <- read_html("https://projects.propublica.org/represent/votes/115/senate/1/100")
sample_Senate <- sample_library %>% html_nodes(xpath='//div[@id="roll-call-tables"]//a[@href]') %>% html_text()
sample_Senate_district <- sample_library %>% html_nodes(xpath='//div[@id="roll-call-tables"]//td[@class="nowrap text-center"]') %>% html_text()
sample_Senate <- as.character(sample_Senate)
sample_Senate_lib <- data.frame(sample_Senate, sample_Senate_district)
sample_Senate_lib$sample_Senate <- as.character(sample_Senate_lib$sample_Senate)
SenateResidents_name <- NA
SenateResidents_sponsoredbills <- NA
SenateResidents_cosponsoredbills <- NA
SenateResidents_votesagainstparty <- NA
SenateResidents_missedvotes <- NA
for (i in 1:length(SenateResidents)){
  if(grepl(" ",SenateResidents[i])==TRUE){
    if(grepl(" ",SenateResidents[i+3])==TRUE){
      SenateResidents_name[i] <- SenateResidents[i]
      SenateResidents_sponsoredbills[i] <- SenateResidents[i+1]
      SenateResidents_cosponsoredbills[i] <- SenateResidents[i+2]
      SenateResidents_votesagainstparty[i] <- "I am not"
      SenateResidents_missedvotes[i] <- "allowed to vote"
    }else{
      SenateResidents_name[i] <- SenateResidents[i]
      SenateResidents_sponsoredbills[i] <- SenateResidents[i+1]
      SenateResidents_cosponsoredbills[i] <- SenateResidents[i+2]
      SenateResidents_votesagainstparty[i] <- SenateResidents[i+3]
      SenateResidents_missedvotes[i] <- SenateResidents[i+4]
    }
  }else{
    next()
  }
}

SenateResidents_name <- na.omit(SenateResidents_name)
SenateResidents_sponsoredbills <- na.omit(SenateResidents_sponsoredbills)
SenateResidents_cosponsoredbills <- na.omit(SenateResidents_cosponsoredbills)
SenateResidents_votesagainstparty <- na.omit(SenateResidents_votesagainstparty)
SenateResidents_missedvotes <- na.omit(SenateResidents_missedvotes)
SenateResidents_115 <- data.frame(SenateResidents_name,SenateResidents_sponsoredbills,SenateResidents_cosponsoredbills,SenateResidents_votesagainstparty,SenateResidents_missedvotes)

SenateResidents_115$SenateResidents_name <- as.character(SenateResidents_115$SenateResidents_name)
SenateResidents_115 <- left_join(SenateResidents_115,sample_Senate_lib, by = c("SenateResidents_name" = "sample_Senate"))

SenateResidents_name_links <- NA
SenateResidents_sponsoredbills_links <- NA
SenateResidents_cosponsoredbills_links <- NA
SenateResidents_votesagainstparty_links <- NA
SenateResidents_missedvotes_links <- NA
for (i in 1:length(SenateResident_Links)){
  if(grepl("bills-sponsored",SenateResident_Links[i])==TRUE){
    if(grepl("bills-sponsored",SenateResident_Links[i+3])==TRUE){
      SenateResidents_name_links[i] <- SenateResident_Links[i-1]
      SenateResidents_sponsoredbills_links[i] <- SenateResident_Links[i]
      SenateResidents_cosponsoredbills_links[i] <- SenateResident_Links[i+1]
      SenateResidents_votesagainstparty_links[i] <- "I am not"
      SenateResidents_missedvotes_links[i] <- "allowed to vote"
    }else{
      SenateResidents_name_links[i] <- SenateResident_Links[i-1]
      SenateResidents_sponsoredbills_links[i] <- SenateResident_Links[i]
      SenateResidents_cosponsoredbills_links[i] <- SenateResident_Links[i+1]
      SenateResidents_votesagainstparty_links[i] <- SenateResident_Links[i+2]
      SenateResidents_missedvotes_links[i] <- SenateResident_Links[i+3]
    }
  }else{
    next()
  }
}

SenateResidents_name_links <- na.omit(SenateResidents_name_links)
SenateResidents_sponsoredbills_links <- na.omit(SenateResidents_sponsoredbills_links)
SenateResidents_cosponsoredbills_links <- na.omit(SenateResidents_cosponsoredbills_links)
SenateResidents_votesagainstparty_links <- na.omit(SenateResidents_votesagainstparty_links)
SenateResidents_missedvotes_links <- na.omit(SenateResidents_missedvotes_links)
SenateResidents_115_links <- data.frame(SenateResidents_name_links,SenateResidents_sponsoredbills_links,SenateResidents_cosponsoredbills_links,SenateResidents_votesagainstparty_links,SenateResidents_missedvotes_links)
SenateResidents_115 <- data.frame(SenateResidents_115,SenateResidents_115_links)
SenateResidents_115$code_link <- NA
for (i in 1:length(SenateResidents_115$SenateResidents_name_links)){
  SenateResidents_115$code_link[i] <- strsplit(as.character(SenateResidents_115$SenateResidents_name_links[i]),'/',fixed=TRUE)[[1]][4]
  SenateResidents_115$code_link[i] <- strsplit(as.character(SenateResidents_115$code_link[i]),'-',fixed=TRUE)[[1]][1]
}


SenateResidents_115

state_code <- data.frame(state.abb,state.name)
for (i in 1:length(SenateResidents_115$sample_Senate_district)){
      SenateResidents_115$state_name[i] <- as.character(state_code$state.name[SenateResidents_115$sample_Senate_district[i]==state_code$state.abb])
    }


for (i in 1:length(SenateResidents_115$state_name)){
  if(strsplit(as.character(SenateResidents_115$SenateResidents_name[i]),' ',fixed=TRUE)[[1]][length(unlist(strsplit(as.character(SenateResidents_115$SenateResidents_name[i]),' ',fixed=TRUE)[[1]]))]=="Jr." || strsplit(as.character(SenateResidents_115$SenateResidents_name[i]),' ',fixed=TRUE)[[1]][length(unlist(strsplit(as.character(SenateResidents_115$SenateResidents_name[i]),' ',fixed=TRUE)[[1]]))]=="II"){
    SenateResidents_115$name_state[i] <- paste0(strsplit(as.character(SenateResidents_115$SenateResidents_name[i]),' ',fixed=TRUE)[[1]][length(unlist(strsplit(as.character(SenateResidents_115$SenateResidents_name[i]),' ',fixed=TRUE)[[1]]))-1]," ","of"," ",as.character(SenateResidents_115$state_name[i]))
  }else{
    SenateResidents_115$name_state[i] <- paste0(strsplit(as.character(SenateResidents_115$SenateResidents_name[i]),' ',fixed=TRUE)[[1]][length(unlist(strsplit(as.character(SenateResidents_115$SenateResidents_name[i]),' ',fixed=TRUE)[[1]]))]," ","of"," ",as.character(SenateResidents_115$state_name[i]))
  }
}





























################################DON'T TOUCH THE BOAT 's m115################################
m_115 <- matrix(0, ncol = 439, nrow = length(url_two))
m_115 <- data.frame(m_115)
colnames(m_115) <- HouseResidents_115$HouseResidents_name
m_115 <- read.csv("m_115.csv", header=TRUE, colClasses = "character")
sum(m_115[,1]!=0)
for (k in 691:length(url_two)){
  for (i in 1:length(colnames(m_115))){
    r <- GET(url_two[k])
    status <- status_code(r) 
    if (status == 500){
      next()
    }else{
      sample_library_test <- read_html(url_two[k])
      HouseVotes_test <- sample_library_test %>% html_nodes(xpath='//td[@class="numeric numeric--mobile nowrap"]') %>% html_text()
      HouseVotes_test <- as.numeric(HouseVotes_test)
      YesNumbers <- HouseVotes_test[1] + HouseVotes_test[2]
      NoNumbers <- HouseVotes_test[3] + HouseVotes_test[4]
      OtherNumbers <- HouseVotes_test[5] + HouseVotes_test[6]
      Votes <- sample_library_test %>% html_nodes(xpath='//div[@id="roll-call-tables"]//a[@href]') %>% html_text()
      YesVoters <- Votes[1:YesNumbers]
      NoVoters <- Votes[(1+YesNumbers):(NoNumbers+YesNumbers)]
      OtherVoters <- Votes[(YesNumbers+NoNumbers+1):(YesNumbers+NoNumbers+OtherNumbers)]
      Reps <- colnames(m_115)
      if (is.element(Reps[i],YesVoters)==TRUE){
        m_115[k,i] <- "Yes"
      }else{
        if (is.element(Reps[i],NoVoters)==TRUE){
          m_115[k,i] <- "No"
        }else{
          m_115[k,i] <- "Other"
        }
      }
    }
    
    
    
  } 
}

write.csv(m_115,"m_115.csv",row.names = FALSE)


##################END###############################
BillScrips <- left_join(BillScrips, m_115, by = c("BillsNos" = "number"))