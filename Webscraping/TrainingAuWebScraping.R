# TrainingAUWebScraping.R

# authored in the R programming language by John Akwei, Data Scientist

# Software Terms and Conditions:
# 1) The Vendor, (John Akwei, Data Scientist, ContextBase), has
# established a record of successful R programming projects that
# have met, or exceeded, the expectations of the Clients. Verification
# of this is found at www.fiverr.com/johnakwei.
#
# 2) Sofware deliverables, (by the Vendor), are assured to operate
# without errors, and proof of error-free operation of Vendor-provided
# software is also a free inclusion with delivery of the internet project.
#
# 3) The Vendor provides help with the R programming language, and
# with RStudio, upon delivery of the Client's ordered R language software.
#
# 4) The Client, (or "Buyer"), agrees that lack of knowledge,
# (by the Client), of the R programming language, or RStuidio,
# is not grounds for cancellation, or non-payment, of the R language
# deliverable by the Vendor, (or "Seller").
#
# 5) The Client's agreement to the above Terms and Conditions is made by
# acceptance of the Custom Offer from the Vendor.

# Set Working Directory to the folder with "webAddress.csv"
setwd("C:/Users/.... ")

# Required R library packages
library(rvest)
library(RCurl)
library(magrittr)

# Data Import
webAddresses <- read.csv("webAddress.csv",header=F)

# Webscrape
scrapeInfo <- matrix(ncol=1, nrow=693)

for(i in 1:693) {
  TrainingOrganisations <- html(as.character(webAddresses[i,]))
  OrganisationsTable <- TrainingOrganisations %>%
    html_nodes(xpath='//*[@id="contactInformation"]') %>%
    html_text() %>%
    gsub("\r\n", "",.)
  if (length(OrganisationsTable)==0) {OrganisationsTable <- "NotAvailable"}
  scrapeInfo[i,] <- OrganisationsTable
}

# Data Formatting
scrapeInfo <- gsub("Name", "", scrapeInfo, fixed=T)
scrapeInfo <- gsub("-", " ", scrapeInfo, fixed=T)
scrapeInfo <- gsub("Phone", " ", scrapeInfo, fixed=T)
scrapeInfo <- gsub("E-Mail", " ", scrapeInfo, fixed=T)
scrapeInfo <- gsub("Address", " ", scrapeInfo, fixed=T)
scrapeInfo <- gsub("Training organsation website",
                   " ", scrapeInfo, fixed=T)
scrapeInfo <- na.omit(scrapeInfo)

# Data Export
write.csv(scrapeInfo, "TrainingAU.csv")
saveRDS(webAddresses, file = "webAddresses.rds")
saveRDS(scrapeInfo, file = "scrapeInfo.rds")

# Uncomment following statements to access saved files
# webAddresses <- readRDS("webAddresses.rds")
# scrapeInfo <- readRDS("scrapeInfo.rds")
