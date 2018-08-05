##################################################
##################################################
##
## Token Economics for Sponsy
##
## John Akwei, ECMp ERMp Data Scientist
## ContextBase, contextbase.github.io, johnakwei1@gmail.com
##
##################################################
##################################################

# Set Working Directory
setwd("C:/Users/Administrator/Dropbox/Programming/Sponsy")
getwd()

# Required Packages
if (!require("PoloniexR")) { install.packages("PoloniexR");
  require("PoloniexR") }

# The Poloniex R package is downloaded from this address:
# https://github.com/VermeirJellen/PoloniexR

# Create the Poloniex environment
poloniex.public <- PoloniexPublicAPI()

# Get ticker information
cryptoInfo <- ReturnTicker(poloniex.public)

# Examine ticker information, see below RStudio console for print out
cryptoInfo[1:10,1:5]

# Set Cryptocurrency pair
pair <- "USDT_ETH"

# Set date range
Sys.setenv(tz="UTC")
from <- as.POSIXct(Sys.Date()-1)
to <- as.POSIXct(Sys.Date())
period  <- "D"

# Get chart data
chart.data <- ReturnChartData(theObject=poloniex.public, pair=pair,
                              from=from, to=to, period=period)

# Find today's ETH price
ETH_Cap <- chart.data$close[[2]]

#####################################################
# Calculate Token Economics

# Reference for Token Economics calculations
# https://blog.goodaudience.com/a-token-metrics-lesson-for-the-ico-team-22c081a4c0e

# Formula for Token Economics, (unaffected by changes in Ethereum price)
# x = (PS + MS * (PB/100+1))/EC
# x = Token Price; PS = Pre-Sale Token Allocation;
# MS = Main Sale Token Allocation;
# PB = Pre-Sale Bonus Percentage; EC = ETH Cap.

# Set values for Token Economics parameters
Token_Name <- "SPONS"
Total_Token_Supply <- 474000000
Tokens_In_Circulation <- Total_Token_Supply * 0.5 # (50%)
ICO_Hard_Cap <- 14000000
Soft_Cap <- 2000000
ICO_Pre_Sale <- 0.15 * Tokens_In_Circulation
ICO_Main_Sale <- 0.85 * Tokens_In_Circulation

# Calculate Token Price
Bonus <- 0
Token_Price <- (ICO_Pre_Sale + ICO_Main_Sale * (Bonus/100+1))/ETH_Cap

#####################################################
# Calculate and Print Token Economics for ICO stages

# ICO Pre-Sale
Bonus <- 20
print(paste("Pre-Sale (20% Bonus) Token Price: 1 ETH =",
            round(Token_Price + (Token_Price * (Bonus/100)), 0),
            Token_Name))

print(paste("Pre-Sale (20% Bonus) Token Price in USD:",
            round(ETH_Cap /
                    round(Token_Price + (Token_Price * (Bonus/100)), 2),5)))

# ICO Public Sale #1
Bonus <- 10
print(paste("Public Sale #1 (10% Bonus) Token Price: 1 ETH =",
            round(Token_Price + (Token_Price * (Bonus/100)), 0),
            Token_Name))

print(paste("Public Sale #1 (10% Bonus) Token Price in USD:",
            round(ETH_Cap /
                    round(Token_Price + (Token_Price * (Bonus/100)), 2),5)))

# ICO Public Sale #2
Bonus <- 0
print(paste("Public Sale #2 (0% Bonus) Token Price: 1 ETH =",
            round(Token_Price + (Token_Price * (Bonus/100)), 0),
            Token_Name))

print(paste("Public Sale #2 (0% Bonus) Token Price in USD:",
            round(ETH_Cap /
                    round(Token_Price + (Token_Price * (Bonus/100)), 2),5)))
