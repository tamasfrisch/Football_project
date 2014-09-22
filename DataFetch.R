require(scrapeR)
require(stringr)

## Transfermarkt.de page structure:
## </div id=yw1> ... </div> : Table xpath constants

CTableHead = "//div[@id='yw1']//th"
CTableRows = "//div[@id='yw1']//tbody//td"
CIndivRows ="//div[@id='yw1']//tbody//td[@class='rechts hauptlink']"
CTMktBaseURL = "http://www.transfermarkt.de/en/@leaguename/marktwerteverein/wettbewerb/@leagueid?stichtag=@date"
CTMktIndivURL="http://www.transfermarkt.de/en/barmi/kader/verein/@teamid/saison_id/@season"
##Football-data.co.uk file structure
## football-data.co.uk/mmz4281/1314/leaguename.csv

CFDatBaseURL ="http://football-data.co.uk/mmz4281/@season/@leagueid.csv"

##Fetching match scores from football-data.co.uk

FetchAllDataFromTmkt <- function() {
      Result <- NULL
      for (i in 1:nrow(LeagueTable)) {
            for (j in 1:nrow(YearValuations)) {
                  toprocess <- GetValueData(leaguename=LeagueTable$leaguename[i],leagueid=LeagueTable$leagueid[i],date=YearValuations$Date[j])
                  numberrows <- nrow(toprocess)
                  dftoadd <- data.frame(Country=rep(LeagueTable$country[i],numberrows),Division=rep(LeagueTable$div[i],numberrows),Season=rep(YearValuations$Season[j],numberrows),Team=toprocess$Club,Value=toprocess$Value,stringsAsFactors=F)
                  if (!is.null(Result)) Result <- merge(Result,dftoadd,all=T) else Result <-dftoadd
                  message (paste("Processing ",LeagueTable$country[i],", Div. ",str_trim(LeagueTable$div[i]),", Season: ", str_trim(YearValuations$Season[j])))
            }
      }
      Result
}

##Fetching championship team valuations from transfermarkt.de

FetchAllDataFromFData <- function() {
      Result <- NULL
      for (i in 1:nrow(LeagueTable)) {
            if (!LeagueTable$scoreneeded[i]) next
            for (j in 1:nrow(YearValuations)) {
                  ScoreURL <- gsub("@leagueid",LeagueTable$scoreleague[i], CFDatBaseURL)
                  ScoreURL <- gsub("@season",gsub("-","",substr(YearValuations$Season[j],3,7)),ScoreURL)
                  toprocess <- read.csv(ScoreURL,stringsAsFactors=F) 
                  numberrows <- nrow(toprocess)?
                  dftoadd <- data.frame(country=rep(LeagueTable$country[i],numberrows),div=rep(LeagueTable$div[i],numberrows),season=rep(YearValuations$Season[j],numberrows),toprocess[,2:7],stringsAsFactors=F)
                  if (!is.null(Result)) Result <- merge(Result,dftoadd,all=T) else Result <-dftoadd
                  message (paste("Processing ",LeagueTable$country[i],", Div. ",str_trim(LeagueTable$div[i]),", Season: ", str_trim(YearValuations$Season[j])))
            }
      }
      Result
}      
## Getting individual team values from transfermarkt.de for irregural teams

FetchIndivQuery <- function () {
      Result<-NULL
      Season1 <- c("2009-10","2010-11","2011-12","2012-13","2013-14") 
      for (i in 1:nrow(IndivQuery)) {
            for (j in Season1) {
                  TeamValue <- GetTeamData(teamid=IndivQuery$ID[i],season=substr(j,1,4))
                  dftoadd <-data.frame (Country=IndivQuery$Country[i],Division=as.character(IndivQuery$Div[i]),Season = j, Team =IndivQuery$TmktTeam[i], Value=TeamValue, stringsAsFactors=F)
                  if (!is.null(Result)) Result <- merge(Result,dftoadd,all=T) else Result <-dftoadd
                  message (paste("Processing ",IndivQuery$TmktTeam[i],", Season:", str_trim(j)))      
            }
      }
      Result
}


## Final dataframe creation

MakeFinalDataFrame <-function () {
      Result <-MatchScores
      message("Substituting team names ...")
      Result <- merge(MatchScores,CorrectionTable[,c("TmktTeam","FdataTeam")],by.x="HomeTeam",by.y="FdataTeam",all.x=T,all.y=F)
      HomeSelector <- !is.na(Result$TmktTeam)
      Result[HomeSelector,]$HomeTeam <- Result[HomeSelector,]$TmktTeam
      Result$TmktTeam <- NULL
      Result <- merge(Result,CorrectionTable[,c("TmktTeam","FdataTeam")],by.x="AwayTeam",by.y="FdataTeam",all.x=T,all.y=F)
      AwaySelector <- !is.na(Result$TmktTeam)
      Result[AwaySelector,]$AwayTeam <- Result[AwaySelector,]$TmktTeam
      Result$TmktTeam <- NULL
      message("Matching team values ...")
      Result <- merge(Result,TeamValuations[,c("Team","Season","Value")],by.x=c("HomeTeam","season"),by.y=c("Team","Season"),all.x=T,all.y=F)
      colnames(Result)[colnames(Result)=="Value"] <-"HomeValue"
      Result <- merge(Result,TeamValuations[,c("Team","Season","Value")],by.x=c("AwayTeam","season"),by.y=c("Team","Season"),all.x=T,all.y=F)
      colnames(Result)[colnames(Result)=="Value"] <-"AwayValue"
      Result <- cbind(Result,ValueDifference=Result$HomeValue-Result$AwayValue)
      Result <- cbind(Result,ResultText=factor(mapply(Goals2Result,Result$FTHG,Result$FTAG),levels=c("big loss","loss","draw","win","big win"),ordered=T))
      Result
}


## Helper function for a single transfermarkt.de query for championships
GetValueData <- function (leaguename="1-bundesliga",leagueid="L1",date="2010-11-01") {
      ##set the URL here
      ValueURL <- gsub("@leaguename",leaguename, CTMktBaseURL)
      ValueURL <- gsub("@leagueid",leagueid,ValueURL)
      ValueURL <- gsub("@date",date,ValueURL)
      parsedHTML <- scrape(ValueURL)[[1]]
      colNames <- xpathSApply(parsedHTML,CTableHead,xmlValue)
      txtData <- matrix(xpathSApply(parsedHTML,CTableRows,xmlValue),ncol=length(colNames),byrow=T)
      dimnames(txtData) [[2]] =colNames
      data.frame(Club=txtData[,3],Date=rep(date,dim(txtData)[1]),Value=Convert2Number(txtData[,5]))
}
## Helper function for a single transfermarkt.de query for teams
GetTeamData <- function (teamid="566",season="2011") {
      ValueURL <- gsub("@teamid",teamid, CTMktIndivURL)
      ValueURL <- gsub("@season",season,ValueURL)
      parsedHTML <- scrape(ValueURL)[[1]]
      osszeg <- sum(Convert2Number(xpathSApply(parsedHTML,CIndivRows,xmlValue),strangeeuro=T),na.rm=T)
      osszeg
}
## Helper function for transfermarkt.de to convert formatted strings to numbers
Convert2Number <- function(String,strangeeuro=F) {
      Value <- gsub(",","",String)
      Value <- gsub(" \u20ac","",Value,perl=T)
      Value <- gsub(" Mrd.","0000000",Value)
      Value <- gsub(" Mill.","0000", Value)
      Value <- gsub(" Mio.","0000", Value)
      Value <- gsub(" Tsd.","000", Value)
      if (strangeeuro) {
            Value <- gsub("\\D","",Value)
      }
      
      as.numeric(Value)
}

##Helper function for calculating results
Goals2Result <-function(homegoals=numeric(), awaygoals=numeric()) {
      if (homegoals==awaygoals) {
            return("draw")
      }
      if ((homegoals-awaygoals)>2) {
            return("big win")
      }
      if ((homegoals-awaygoals)>0) {
            return("win")
      }
      if ((homegoals-awaygoals)< -2) {
            return("big loss")
      }
      if ((homegoals-awaygoals)<0) {
            return("loss")
      }
     invisible("") 
}

unfactorize <- function(df){
      for(i in which(sapply(df, class) == "factor")) df[[i]] = as.character(df[[i]])
      return(df)
}