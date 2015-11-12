library(RMySQL)
library(dbConnect)
library(magrittr)

# Data import options:

# Get data from MySQL db:
# dt = dbConnect(MySQL(), user='root', password='',
#                  dbname='',
#                  host='localhost')

# or

# Get data from .csv file:
# dt <- read.csv("kp_coupon_fragments.csv")

# Function to get paragraph for processing:
getNextparagraph <- function(data, paragraphNumber){
  x <- paragraphNumber
  z <- as.character(unlist(data)) %>% gsub(" +", " ", .) %>%
    strsplit(split = "[\\.?!] ")
  nextParagraph <- z[[x]]
}

# Four Functions that search/extract phrases from a paragraph:
includeTerm <- function(paragraph, srchTerm){
  loc <- grep(srchTerm, paragraph, ignore.case=T)
  coupLine <- paragraph[loc]
  fllw <- unlist(strsplit(
    unlist(strsplit(coupLine, srchTerm))[2], " "))[2:6]
  phrse <- c(srchTerm, fllw)
}

followWords <- function(paragraph, srchTerm){
  loc <- grep(srchTerm, paragraph, ignore.case=T)
  coupLine <- paragraph[loc]
  fllw <- unlist(strsplit(
    unlist(strsplit(coupLine, srchTerm))[2], " "))[2:5]
}

allfollowWords <- function(paragraph, srchTerm){
  loc <- grep(srchTerm, paragraph, ignore.case=T)
  coupLine <- paragraph[loc]
  fllw <- unlist(strsplit(
    unlist(strsplit(coupLine, srchTerm))[2], " "))
}

includeWords <- function(paragraph, srchTerm){
  loc <- grep(srchTerm, paragraph, ignore.case=T)
  coupLine <- paragraph[loc]
  fllw <- unlist(strsplit(
    unlist(strsplit(coupLine, srchTerm))[2], " "))[1:5]
}

# Create datatable variable to hold all final data:
processedKoupons <- matrix(nrow=1, ncol=8)
colnames(processedKoupons) <- c("Description", "Discount", "MinimumSpendAmount",
                                "OfferApplicableOn", "BrandsUnderOffer",
                                "Validfor", "PaymentModeOffer",
                                "TermsAndConditions")

# for loop that processes entire imported data:
for(i in seq_len(nrow(dt))) {

#Get next paragraph for processing:
prgrph <- getNextparagraph(dt[3], i)

# Extraction-Base Summarization code:
if(sum(grepl("Flat", prgrph)) > 0){
  Discount <- includeTerm(prgrph, "Flat")
} else if(sum(grepl("Now", prgrph)) > 0){
  Discount <- includeTerm(prgrph, "Now")
} else if(sum(grepl("now", prgrph)) > 0){
  Discount <- includeTerm(prgrph, "now")
} else if(sum(grepl("discount", prgrph)) > 0){
  Discount <- followWords(prgrph, "discount")
} else if(sum(grepl("Up", prgrph)) > 0){
  Discount <- includeTerm(prgrph, "Up")
} else if(sum(grepl("to", prgrph)) > 0){
  Discount <- followWords(prgrph, "to")
} else if(sum(grepl("Get", prgrph)) > 0){
  Discount <- includeTerm(prgrph, "Get")
} else {Discount <- ""}
Discount <- Discount[!is.na(Discount)]

if(sum(grepl("Rs.", prgrph)) > 0){
  MinSpndAmt <- c("Rs.", includeWords(prgrph, "Rs."))
} else if(sum(grepl("Rs", prgrph)) > 0){
  MinSpndAmt <- c("Rs", includeWords(prgrph, "Rs"))
} else if(sum(grepl("Avail", prgrph)) > 0){
  MinSpndAmt <- followWords(prgrph, "Avail")
} else {MinSpndAmt <- ""}
MinSpndAmt <- MinSpndAmt[!is.na(MinSpndAmt)]

if(sum(grepl(" on ", prgrph)) > 0){
  OffrAplOn <- allfollowWords(prgrph, "on ")
} else if(sum(grepl("get", prgrph)) > 0){
  OffrAplOn <- includeTerm(prgrph, "get")
} else {OffrAplOn <- ""}
OffrAplOn <- OffrAplOn[!is.na(OffrAplOn)]

if(sum(grepl("like", prgrph)) > 0){
  Brands <- allfollowWords(prgrph, "like")
} else if(sum(grepl("to", prgrph)) > 0){
  Brands <- followWords(prgrph,"to")
} else {Brands <- ""}
Brands <- Brands[!is.na(Brands)]

if(sum(grepl("Customers", prgrph)) > 0){
  Validfor <- includeTerm(prgrph, "Customers")
} else if(sum(grepl("for all", prgrph)) > 0){
  Validfor <- followWords(prgrph, "for")
} else if(sum(grepl("old", prgrph)) > 0){
  Validfor <- includeTerm(prgrph, "old")
} else if(sum(grepl("Old", prgrph)) > 0){
  Validfor <- includeTerm(prgrph, "Old")
} else if(sum(grepl("valid", prgrph)) > 0){
  Validfor <- includeTerm(prgrph, "valid")
} else {Validfor <- ""}
Validfor <- Validfor[!is.na(Validfor)]

if(sum(grepl("by", prgrph)) > 0){
  PymntModeOff <- followWords(prgrph, "by")
} else if(sum(grepl("Use", prgrph)) > 0){
  PymntModeOff <- includeTerm(prgrph, "Use")
} else {PymntModeOff <- ""}
PymntModeOff <- PymntModeOff[!is.na(PymntModeOff)]

if(sum(grepl("Limited", prgrph)) > 0){
  TermsConditions <- includeTerm(prgrph, "Limited")
} else if(sum(grepl("Now", prgrph)) > 0){
  TermsConditions <- followWords(prgrph, "Now")
} else if(sum(grepl("No need", prgrph)) > 0){
  TermsConditions <- includeTerm(prgrph, "No need")
} else {TermsConditions <- ""}
TermsConditions <- TermsConditions[!is.na(TermsConditions)]

# Create next row of datatable to export:
m <- matrix(nrow=1, ncol=8)
m[1] <- toString(prgrph)
m[2] <- gsub(",", "", toString(Discount), fixed=T)
m[3] <- gsub(",", "", toString(MinSpndAmt), fixed=T)
m[4] <- gsub(",", "", toString(OffrAplOn), fixed=T)
m[5] <- gsub(",", "", toString(Brands), fixed=T)
m[6] <- gsub(",", "", toString(Validfor), fixed=T)
m[7] <- gsub(",", "", toString(PymntModeOff), fixed=T)
m[8] <- gsub(",", "", toString(TermsConditions), fixed=T)
processedKoupons <- rbind(processedKoupons, m[1:8])
}

# Assemble data for export:
processedKoupons <- processedKoupons[-1,]
processedKoupons <- cbind(dt[1:2], processedKoupons)

# Data export options:

# Export processed data to .csv file:
# write.csv(processedKoupons, file="ProcessedParagraphs.csv")

# or

# Export processed data to MySQL db:
# dbWriteTable(processedKoupons, name='table_name')