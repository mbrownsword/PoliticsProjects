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

##getting the URLS

url <- NA
r <- NA
status <- NA
for (i in 1:100){
url[i] <- paste0("https://projects.propublica.org/represent/votes/116/all/house?page=",i)
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


##getting the bill names & numbers

webpage <- NA
HouseBills1 <- NA
HouseBills <- NA
BillNumbers <- 30
HouseBills <- matrix(0, Billnumbers, length(url))
for (i in 1:length(url)){
  r <- GET(url[i])
  status <- status_code(r) 
  if (status == 500){
    next()
  }else{
    webpage <- read_html(url[i])
    HouseBills1<- webpage %>% html_nodes(xpath='//div[@class="margin-bottom--md"]//h3[@class]') %>% html_text()
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

##adding the descriptions, bill names, vote totals (via ProPublica)
url_two <- NA
for (i in 1:length(HouseBills)){
  url_two[i] <- paste0("https://projects.propublica.org/represent/votes/116/house/1/",i)
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
HouseBillDescriptions <- data.frame(HouseBillsDescriptions,row.names,HouseBillName,HouseTime,DemYes,DemNo,DemOther,RepYes,RepNo,RepOther)
BillsonBills$BillsNos <- as.numeric(BillsonBills$BillsNos)
BillScrips <- left_join(BillsonBills,HouseBillDescriptions, by = c("BillsNos" = "row.names"))


###Sponsors
BillScrips$HouseBillName_clean <- gsub("\\.","",BillScrips$HouseBillName)
BillScrips$HouseBillName_clean[is.na(BillScrips$HouseBillName_clean)] <- "poop"

for (i in 1:length(BillScrips)){
  url_bills <- paste0("https://projects.propublica.org/represent/bills/116/",BillScrips$HouseBillName_clean[BillScrips$BillsNos==i])
  r <- GET(url_bills)
  status <- status_code(r) 
  if (status == 500 | status == 404 | status == 502){
    next()
  }else{
    webpage_bills <- read_html(url_bills)
    Billsponsor <- webpage_bills %>% html_nodes(xpath='//div[@id="sponsors"]//a') %>% html_text()
    if (length(Billsponsor)==0){
      BillScrips$BillSponsor[BillScrips$BillsNos==i] <- NA
    }else{
      BillScrips$BillSponsor[BillScrips$BillsNos==i] <- trimws(strsplit(as.character(Billsponsor),' (',fixed=TRUE)[[1]][1], which=c("left"))
    }
    Billcosponsors <- webpage_bills %>% html_nodes(xpath='//div[@id="cosponsors"]//a') %>% html_text()
    if (length(Billcosponsors)==0){
      BillScrips$Billcosponsor[BillScrips$BillsNos==i] <- NA
    }else{
      for (j in 1:length(Billcosponsors)){
        Billcosponsors[j] <- trimws(strsplit(as.character(Billcosponsors[j]),' (',fixed=TRUE)[[1]][1], which=c("left"))
      }
      Billcosponsors <- paste0(Billcosponsors, collapse=",")
      BillScrips$Billcosponsor[BillScrips$BillsNos==i] <- Billcosponsors
    }
    thaturl <- webpage_bills %>% html_nodes(xpath='//div[@class="bill-summary-block"]//a//@href') %>% html_text()
    if (length(thaturl)==0){
      BillScrips$thaturl[BillScrips$BillsNos==i] <- NA
    }else{
      BillScrips$thaturl[BillScrips$BillsNos==i] <- thaturl
    }
    
  }
  
}

###From Congress with love

for (i in 1:length(BillScrips$thaturl)){
  if (is.na(BillScrips$thaturl[BillScrips$BillsNos==i])==TRUE){
    next()
  }else{
    if(length(BillScrips$thaturl[BillScrips$BillsNos==i])==0){
      next()
    }else{
      r <- GET(BillScrips$thaturl[BillScrips$BillsNos==i])
      status <- status_code(r) 
      if (status == 500 | status == 404 | status == 502){
        next()
      }else{
        webpage_billvanilly <- read_html(BillScrips$thaturl[BillScrips$BillsNos==i])
        Bill_committee <- (webpage_billvanilly %>% html_nodes(xpath='//table[@class="standard01"]//td') %>% html_text())[2]
        Bill_subject <- (webpage_billvanilly %>% html_nodes(xpath='//ul[@class="plain"]//li[1]') %>% html_text())[2]
        Bill_summary_pgraph_bullets <- (webpage_billvanilly %>% html_nodes(xpath='//div[@id="bill-summary"]//*') %>% html_text())
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
          BillScrips$Bill_committee[BillScrips$BillsNos==i] <- NA
        }else{
          if(grepl("[[:digit:]]",as.character(Bill_committee))){
            BillScrips$Bill_committee[BillScrips$BillsNos==i] <- NA 
          }else{
            BillScrips$Bill_committee[BillScrips$BillsNos==i] <- Bill_committee 
          }
        }
        BillScrips$Bill_subject[BillScrips$BillsNos==i] <- Bill_subject
        BillScrips$Bill_summary[BillScrips$BillsNos==i] <- Bill_summary
      }
    }
  }
 
}


###just textin
for (i in 1:length(BillScrips)){
  if (is.na(BillScrips$thaturl[BillScrips$BillsNos==i])==TRUE){
    next()
  }else{
    if(length(BillScrips$thaturl[BillScrips$BillsNos==i])==0){
      next()
    }else{
  url_bills_text <- paste0(strsplit(as.character(BillScrips$thaturl[BillScrips$BillsNos==i]),'summary',fixed=TRUE)[[1]][1],"text")
  url_bills_amendments <- paste0(strsplit(as.character(BillScrips$thaturl[BillScrips$BillsNos==294]),'summary',fixed=TRUE)[[1]][1],"amendments")
  r <- GET(url_bills_text)
  status <- status_code(r) 
      if (status == 500 | status == 404 | status == 502){
        next()
      }else{
    webpage_bills_text <- read_html(url_bills_text)
    thatbill_text <- webpage_bills_text %>% html_nodes(xpath='//p') %>% html_text()
    thatbill_text <- thatbill_text[thatbill_text != ""]
    thatbill_text <- thatbill_text[thatbill_text != " "]
        if (length(thatbill_text)==0){
      BillScrips$thatbill_text[BillScrips$BillsNos==i] <- NA
        }else{
      thatbill_text <- paste0(thatbill_text, collapse=" ")
      BillScrips$thatbill_text[BillScrips$BillsNos==i] <- thatbill_text
        }
      }
    }
  }
  
  
}

url_bills_amendments <- paste0(strsplit(as.character(BillScrips$thaturl[BillScrips$BillsNos==294]),'summary',fixed=TRUE)[[1]][1],"amendments")
webpage_bills_amendments <- read_html(url_bills_amendments)
thatbill_amendment_links <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-heading amendment-heading"]//a//@href') %>% html_text()
thatbill_amendment_links <- thatbill_amendment_links[seq(1, length(thatbill_amendment_links), 2)] 
thatbill_amendment_sponsorlinks <- webpage_bills_amendments %>% html_nodes(xpath='//div[@id="main"]//span[@class="result-item"]//a//@href') %>% html_text()
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


amendment_city <- data.frame(thatbill_amendment_links,thatbill_amendment_RepCode)
for (i in 1:length(amendment_city$thatbill_amendment_RepCode)){
  amendment_city$RepName[i] <- HouseResidents$V1[HouseResidents$code_link==amendment_city$thatbill_amendment_RepCode[i]]
  
}

##################END###############################
BillScrips <- left_join(BillScrips, m, by = c("BillsNos" = "number"))





























##Building the live Representative lobby

url_replib <- "https://projects.propublica.org/represent/members/116/house"
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
HouseResidents <- data.frame(as.data.frame(matrix(HouseResidents, ncol = 5,  byrow = TRUE), stringsAsFactors = FALSE),HousePositions,as.data.frame(matrix(HouseResident_Links, ncol = 5,  byrow = TRUE), stringsAsFactors = FALSE))
HouseResidents$V1 <- as.character(HouseResidents$V1)
HouseResidents <- left_join(HouseResidents,sample_house_lib, by = c("V1" = "sample_house"))
for (i in 1:length(HouseResidents$code_link)){
  HouseResidents$code_link[i] <- strsplit(as.character(HouseResidents$V2.1[i]),'/',fixed=TRUE)[[1]][4]
}


































##DON'T TOUCH THE BOAT
m <- matrix(0, ncol = 439, nrow = length(url_two))
m <- data.frame(m)
colnames(m) <- HouseResidents$V1
for (k in 1:length(url_two)){
  for (i in 1:length(colnames(m))){
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
      Reps <- colnames(m)
      if (is.element(Reps[i],YesVoters)==TRUE){
        m[k,i] <- "Yes"
      }else{
        if (is.element(Reps[i],NoVoters)==TRUE){
          m[k,i] <- "No"
        }else{
          m[k,i] <- "Other"
        }
      }
    }
    
    
    
  } 
}

m$number <- 1:300
write.csv(m, "m.csv")














