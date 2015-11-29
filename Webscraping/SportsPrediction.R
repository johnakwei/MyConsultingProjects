########
#
# SportsPrediction.R
#
# Programming by John Akwei, ECMp ERMp Data Scientist
# ContextBase, contextbase.github.io
# Customer Service: johnakwei1@gmail.com
#
# This R Programming Language script extracts daily prediction data from
# soccer match prediction websites. Then the predictions are combined for
# the purpose of creating a more accurate match prediction than the individual
# websites.
#
########

########
#
# Table of Contents
# 1) Processing Requirements Section
# 2) Web Scraping Section
# 3) Combined Prediction Section
#
########

########################################
# 1) Processing Requirements Section:
########################################

# Set the "Working Directory" to the folder
# containing "SporstsPrediction.R":
# setwd("C:/Users/johnakwei/Dropbox/Programming/SportsPrediction")
setwd("C:/Users/...")
# Finds the current Working Directory:
getwd()

# Required R library packages:
library(RCurl)
library(rvest)
library(magrittr)
library(XML)

# Append date to output file names:
IncludeDateTime <- function(str, sep='_', date.format="%Y_%m_%d_%H_%M") {
  stopifnot(is.character(str))
  return(paste(str, format(Sys.time(), date.format), sep=sep))  
}

########################################
# 2) Web Scraping Section
########################################

# List of websites for data extraction:
websiteList <- c("http://www.soccervista.com/England-Premier_League-2015_2016-840449.html",
                 "http://www.predictz.com/predictions/england/premier-league",
                 "http://www.windrawwin.com/predictionsnew.asp?l=PRM",
                 "http://www.betexplorer.com/soccer/england/premier-league/",
                 "http://www.betshoot.com/betting-previews/football/england/",
                 "http://www.statarea.com/predictions",
                 "http://predict.7msport.com/en/engpr/",
                 "http://en.scometix.com",
                 "http://www.forebet.com/en/football-tips-and-predictions-for-england/premier-league",
                 "http://www.zulubet.com",
                 "http://www.vitibet.com/index.php?clanek=analyzy&sekce=fotbal&liga=anglie&lang=en",
                 "http://www.prosoccer.gr",
                 "http://clubelo.com/ENG",
                 "http://www.oddschecker.com/football/english/premier-league")

# Get Soccervista data
soccervista <- html(websiteList[1])
soccervistaTable <- readHTMLTable(soccervista,header=T,which=2,stringsAsFactors=F)
soccervistaTable <- soccervistaTable[3,]
names(soccervistaTable) <- c("date", "round", "home", "away", "one", "X", "two", "prediction", "# goals",
                             "tip", "A")
write.csv(soccervistaTable,
          file=paste(IncludeDateTime("SoccerVistaTable"), ".csv"))
rm(soccervista)
########

########
# Get Predictz data
predictz <- html(websiteList[2])
predictzTable <- readHTMLTable(predictz,header=T,which=1,stringsAsFactors=F)
predictzTable <- predictzTable[1,]
write.csv(predictzTable,
          file=paste(IncludeDateTime("PredictzTable"), ".csv"))
rm(predictz)
########

########
# Get WinDrawWin data
windrawwin <- html(websiteList[3])
windrawwinTable <- readHTMLTable(windrawwin,header=F,which=1,stringsAsFactors=F)
windrawwinTable <- windrawwinTable[1:2,]
write.csv(windrawwinTable,
          file=paste(IncludeDateTime("WinDrawWinTable"), ".csv"))
rm(windrawwin)
########

########
# Get betexplorer data
betexplorer <- html(websiteList[4])
betexplorerTable <- readHTMLTable(betexplorer,header=T,which=1,stringsAsFactors=F)
betexplorerTable <- betexplorerTable[2,]
write.csv(betexplorerTable,
          file=paste(IncludeDateTime("BetexplorerTable"), ".csv"))
rm(betexplorer)
########

########
# Get BetShoot data
betshoot <- html(websiteList[5])
betshootTable <- betshoot %>%
  html_nodes(".bettingpick2") %>%
  html_text()
betshootTable <- betshootTable[1] %>%
  gsub("\n", " ",.)
betshootTable <- as.character(unlist(betshootTable)) %>%
  strsplit(split = "\ ")
betshootTable <- betshootTable[[1]]
betshootTable <- data.frame(betshootTable)
m <- matrix(nrow=1, ncol=11); m <- data.frame(m)
m$X1 <- betshootTable[1,1]; m$X2 <- betshootTable[2,1]
m$X3 <- betshootTable[3,1]; m$X4 <- betshootTable[4,1]
m$X5 <- betshootTable[5,1]; m$X6 <- betshootTable[6,1]
m$X7 <- betshootTable[7,1]; m$X8 <- betshootTable[8,1]
m$X9 <- betshootTable[9,1]; m$X10 <- betshootTable[10,1]
m$X11 <- betshootTable[11,1]; m$X12 <- betshootTable[12,1]
names(m) <- c("Month", "Day", "Home", "X1", "Away1", "Away2",
                          "X2", "Winner", "Prediction", "Predicted_Score",
              "Predicter", "X3")
betshootTable <- m
write.csv(betshootTable,
          file=paste(IncludeDateTime("BetshootTable"), ".csv"))
rm(betshoot, m)
########

########
# Get statarea data
# statarea <- html(websiteList[6])
# statareaTable <- statarea %>% 
#     html_nodes(".prdleague") %>% 
#     html_nodes(".prdleaguebody") %>% 
#     html_nodes(".prdmatch") %>%
#   .[1] %>%
#   html_text() %>%
#   data.frame(., stringsasfactors=T)
# 
# %>% strsplit(split=" ")
# 
# %>% html_table()
# write.csv(statareaTable,
#           file=paste(IncludeDateTime("StatareaTable"), ".csv"))
# rm(statarea)
########

########
# Get predict.7msport data
# predict.7msport <- html(websiteList[7])
# predict.7msportTable <- predict.7msport %>%
#   html_nodes(xpath='//*[@id="divList"]/div[1]') %>%
#   html_text() %>%
#   strsplit(split="\ |VS|,")
# predict.7msportTable <- predict.7msportTable[[1]]
# predict.7msportTable <- data.frame(predict.7msportTable)
# write.csv(predict.7msportTable,
#           file=paste(IncludeDateTime("Predict7msportTable"), ".csv"))
# rm(predict.7msport)
########

########
# Get scometix data
# scometix <- html(websiteList[8])
# scometixTable <- scometix %>%
#   html_nodes(".Cufon") %>%
#   html_text()
# scometixTable <- data.frame(scometixTable)
# write.csv(scometixTable,
#           file=paste(IncludeDateTime("ScometixTable"), ".csv"))
# rm(scometix)
########

########
# Get forebet data
forebet <- html(websiteList[9])
forebetTable <- readHTMLTable(forebet,header=T,which=1,stringsAsFactors=F)
forebetTable <- forebetTable[14,]
forebetTable$V1 <- gsub("[\n|\t|0-9|/:]", "", forebetTable$V1)
names(forebetTable) <- c("Teams", "one", "X", "two", "Forebet", "Correct_Score",
              "Avg_Goals", "Weather", "Odds", "Kelly_Criterion",
              "Final_Score")
write.csv(forebetTable,
          file=paste(IncludeDateTime("ForebetTable"), ".csv"))
rm(forebet)
########

########
# Get zulubet data
zulubet <- html(websiteList[10])
zulubetTable <- readHTMLTable(zulubet,header=T,which=1,stringsAsFactors=F)
zulubetTable <- zulubetTable[6,]
write.csv(zulubetTable,
          file=paste(IncludeDateTime("ZulubetTable"), ".csv"))
rm(zulubet)
########

########
# Get vitibet data
vitibet <- html(websiteList[11])
vitibetTable <- readHTMLTable(vitibet,header=T,which=4,stringsAsFactors=F)
vitibetTable <- vitibetTable[4,]
vitibetTable$V9 <- gsub("[ %]", "", vitibetTable$V9)
vitibetTable$V10 <- gsub("[ %]", "", vitibetTable$V10)
vitibetTable$V11 <- gsub("[ %]", "", vitibetTable$V11)
vitibetTable$V9 <- as.numeric(vitibetTable$V9) / 100
vitibetTable$V10 <- as.numeric(vitibetTable$V10) / 100
vitibetTable$V11 <- as.numeric(vitibetTable$V11) / 100
write.csv(vitibetTable,
          file=paste(IncludeDateTime("VitibetTable"), ".csv"))
rm(vitibet)
########

########
# Get prosoccer data
prosoccer <- html(websiteList[12])
prosoccerTable <- readHTMLTable(prosoccer,header=T,which=1,stringsAsFactors=F)
prosoccerTable <- prosoccerTable[16,]
write.csv(prosoccerTable,
          file=paste(IncludeDateTime("ProsoccerTable"), ".csv"))
rm(prosoccer)
########

########
# Get clubelo data
clubelo <- html(websiteList[13])
clubeloTable <- clubelo %>%
  html_nodes(xpath='/html/body/div/div[7]/svg') %>%
  html_table()
write.csv(clubeloTable,
          file=paste(IncludeDateTime("ClubeloTable"), ".csv"))
rm(clubelo)
########

########
# Get oddschecker data
oddschecker <- html(websiteList[14])
oddscheckerTable <- readHTMLTable(oddschecker,header=T,which=1,stringsAsFactors=F)
oddscheckerTable <- oddscheckerTable[2,]
oddscheckerTable$Draw <- gsub("[a-z|A-Z|()]", "", oddscheckerTable$Draw)
oddscheckerTable$Away <- gsub("[a-z|A-Z|()]", "", oddscheckerTable$Away)
write.csv(oddscheckerTable,
          file=paste(IncludeDateTime("OddscheckerTable"), ".csv"))
rm(oddschecker)
########

########################################
# 3) Combined Prediction Section
########################################

# Set 1 X 2 values to numeric
soccervistaTable$one <- as.numeric(soccervistaTable$one)
soccervistaTable$X <- as.numeric(soccervistaTable$X)
soccervistaTable$two <- as.numeric(soccervistaTable$two)

predictzTable$V4 <- as.numeric(predictzTable$V4)
predictzTable$V5 <- as.numeric(predictzTable$V5)
predictzTable$V6 <- as.numeric(predictzTable$V6)

betexplorerTable$V4 <- as.numeric(betexplorerTable$V4)
betexplorerTable$V5 <- as.numeric(betexplorerTable$V5)
betexplorerTable$V6 <- as.numeric(betexplorerTable$V6)

forebetTable$one <- as.numeric(forebetTable$one)
forebetTable$X <- as.numeric(forebetTable$X)
forebetTable$two <- as.numeric(forebetTable$two)

zulubetTable$V9 <- as.numeric(zulubetTable$V9)
zulubetTable$V10 <- as.numeric(zulubetTable$V10)
zulubetTable$V11 <- as.numeric(zulubetTable$V11)

vitibetTable$V9 <- as.numeric(vitibetTable$V9)
vitibetTable$V10 <- as.numeric(vitibetTable$V10)
vitibetTable$V11 <- as.numeric(vitibetTable$V10)

prosoccerTable$V4 <- as.numeric(prosoccerTable$V4)
prosoccerTable$V5 <- as.numeric(prosoccerTable$V5)
prosoccerTable$V6 <- as.numeric(prosoccerTable$V6)

oddscheckerTable$Home <- as.numeric(oddscheckerTable$Home)
oddscheckerTable$Draw <- as.numeric(oddscheckerTable$Draw)
oddscheckerTable$Away <- as.numeric(oddscheckerTable$Away)

# Verify 1 X 2 values are within range:
if (soccervistaTable$one<0.1 | soccervistaTable$one>9.50) {soccervistaTable$one<-NA}
if (soccervistaTable$X<0.1 | soccervistaTable$X>9.50) {soccervistaTable$X<-NA}
if (soccervistaTable$two<0.1 | soccervistaTable$two>9.50) {soccervistaTable$one<-NA}

if (predictzTable$V4<0.1 | predictzTable$V4>9.50) {predictzTable$V4<-NA}
if (predictzTable$V5<0.1 | predictzTable$V5>9.50) {predictzTable$V5<-NA}
if (predictzTable$V6<0.1 | predictzTable$V6>9.50) {predictzTable$V6<-NA}

if (betexplorerTable$V4<0.1 | betexplorerTable$V4>9.50) {betexplorerTable$V4<-NA}
if (betexplorerTable$V5<0.1 | betexplorerTable$V5>9.50) {betexplorerTable$V5<-NA}
if (betexplorerTable$V6<0.1 | betexplorerTable$V6>9.50) {betexplorerTable$V6<-NA}

if (forebetTable$one<0.1 | forebetTable$one>9.50) {forebetTable$one<-NA}
if (forebetTable$X<0.1 | forebetTable$X>9.50) {forebetTable$X<-NA}
if (forebetTable$two<0.1 | forebetTable$two>9.50) {forebetTable$two<-NA}

if (zulubetTable$V9<0.1 | zulubetTable$V9>9.50) {zulubetTable$V9<-NA}
if (zulubetTable$V10<0.1 | zulubetTable$V10>9.50) {zulubetTable$V10<-NA}
if (zulubetTable$V11<0.1 | zulubetTable$V11>9.50) {zulubetTable$V11<-NA}

if (vitibetTable$V9<0.1 | vitibetTable$V9>9.50) {vitibetTable$V9<-NA}
if (vitibetTable$V10<0.1 | vitibetTable$V10>9.50) {vitibetTable$V10<-NA}
if (vitibetTable$V11<0.1 | vitibetTable$V11>9.50) {vitibetTable$V11<-NA}

if (prosoccerTable$V4<0.1 | prosoccerTable$V4>9.50) {prosoccerTable$V4<-NA}
if (prosoccerTable$V5<0.1 | prosoccerTable$V5>9.50) {prosoccerTable$V5<-NA}
if (prosoccerTable$V6<0.1 | prosoccerTable$V6>9.50) {prosoccerTable$V6<-NA}

if (oddscheckerTable$Home<0.1 | oddscheckerTable$Home>9.50)
{oddscheckerTable$Home<-NA}
if (oddscheckerTable$Draw<0.1 | oddscheckerTable$Draw>9.50)
{oddscheckerTable$Draw<-NA}
if (oddscheckerTable$Away<0.1 | oddscheckerTable$Away>9.50)
{oddscheckerTable$Away<-NA}

# Create Combined Table
combinedTable <- matrix(nrow=9, ncol=7)
combinedTable <- data.frame(combinedTable)
colnames(combinedTable) <- c("Website", "Teams", "one",	"X", "two",
                             "Odds", "Prediction")
combinedTable[1,] <- c("soccervistaTable", soccervistaTable$home,
                       soccervistaTable$one, soccervistaTable$X,
                       soccervistaTable$two, "", soccervistaTable$prediction)
combinedTable[2,] <- c("predictzTable", predictzTable$V2, predictzTable$V4, predictzTable$V5,
                       predictzTable$V6, "", predictzTable$V7)
combinedTable[3,] <- c("betexplorerTable", betexplorerTable$V2,
                       betexplorerTable$V4, betexplorerTable$V5,
                       betexplorerTable$V6,"","")
combinedTable[4,] <- c("betshootTable", betshootTable$Home,
                       "","","","", betshootTable$Prediction)
combinedTable[5,] <- c("forebetTable", forebetTable$Teams, forebetTable$one,
                       forebetTable$X, forebetTable$two, "",
                       forebetTable$Correct_Score)
combinedTable[6,] <- c("zulubetTable", zulubetTable$V2, zulubetTable$V9,
                       zulubetTable$V10, zulubetTable$V11, "", zulubetTable$V12)
combinedTable[7,] <- c("vitibetTable", vitibetTable$V3, vitibetTable$V9,
                       vitibetTable$V10, vitibetTable$V11,"","")
combinedTable[8,] <- c("prosoccerTable", prosoccerTable$V3, prosoccerTable$V4,
                       prosoccerTable$V5, prosoccerTable$V6, "", "")
combinedTable[9,] <- c("oddscheckerTable", oddscheckerTable$Home,
                        oddscheckerTable$Home, oddscheckerTable$Draw,
                        oddscheckerTable$Away, "", "")
write.csv(combinedTable,
          file=paste(IncludeDateTime("CombinedTable"), ".csv"))

# 1 X 2 Median
Home <- mean(complete.cases(combinedTable$one))
Home <- format(round(Home, 2), nsmall=2)
Away <- mean(complete.cases(combinedTable$two))
Away <- format(round(Away, 2), nsmall=2)
Draw <- mean(complete.cases(combinedTable$X))
Draw <- format(round(Draw, 2), nsmall=2)
HighScore <- max(Home, Away, Draw)
HighScore <- format(round(HighScore, 2), nsmall=2)

# Margin Formula = "Home Odds"+"Away Odds"+"Draw Odds"- "1"
Margin <- mean(complete.cases(combinedTable$one)) +
  mean(complete.cases(combinedTable$two)) +
  mean(complete.cases(combinedTable$X)) - 1
Margin <- format(round(Margin, 2), nsmall=2)

# Print Winning Team, and Margin
if (HighScore == Home) {win <- "Home"} else
  if (HighScore == Away) {win <- "Away"} else
    if (HighScore == Draw) {win <- "Draw"}

print(paste("Team predicted to win - ", win, ".", sep=""))
print(paste("The Combined Margin is ", Margin, "%.", sep=""))