library(httr)
library(XML)
library(plyr)
library(dplyr)
library(stringr)
library(devtools) #install_github("https://github.com/marchtaylor/sinkr")
library(sinkr)
library(png)
library(RColorBrewer)

circuitLengthKM <- 13.626


getImages <- function()
{
  sourceCode <- readLines("le-mans-website.txt")
  
  # Get lines with .png
  sourceCode <- sourceCode[ grepl( "png", sourceCode ) &
                grepl( "<img src", sourceCode ) &
                grepl( "entry_car", sourceCode ) ]
  
  
  pngURL <- gsub(".*<img src=\"|\".*","",sourceCode)
  #pngURL <- gsub(".*<img src=\"\\s*|\".*","",sourceCode)
  #pngURL <- str_extract( sourceCode, "https://\\s*(.*?)\\s*.png" )
  pngURL <- unique(pngURL)
  
  N <- length(pngURL)
  
  carNumber <- gsub(".*lm-|-.*","",pngURL)
  
  t <- carNumber=="toyota"
  carNumber[t] <- gsub(".*gr010-|-.*","",pngURL[t])
  
  g <- carNumber=="glickenhaus"
  carNumber[g] <- gsub(".*glickenhaus-|-.*","",pngURL[g])
  
  dir.create("images")
  
  for( i in 1:N )
  {
    download.file( url=pngURL[i],
                   destfile=paste0("images/",carNumber[i],".png") )
  }
}

wiki <- GET("https://en.wikipedia.org/wiki/2023_24_Hours_of_Le_Mans")
# Pull all tables from wikipedia article
wikiTables <- readHTMLTable( doc=content(wiki,"text"),
                          header=TRUE )

resultTable <- wikiTables[[5]]
colnames(resultTable) <- resultTable[1, ]

resultTable <- filter( resultTable,
                       Pos!="Pos", # Remove first row
                       !is.na(No.) ) %>%
               mutate( Distance=as.numeric(Laps)*circuitLengthKM,
                       Class=if_else(Class=="LMP2 (Pro-Am)","LMP2",Class) )

N <- nrow(resultTable)

# Adjust #8 Toyota that finished on the lead lap
timeDiff <- resultTable[2,"Time/Reason"]

mins <- str_match( timeDiff, "\\+\\s*(.*?)\\:" )[2] %>%
        as.numeric()

secDiff <- sub(".*:","",timeDiff) %>%
           as.numeric() + 60*mins

# Source: The B Pillar (https://mcusercontent.com/58ec14b624aac0d4f32a73311/files/1a937598-5795-63eb-6c7b-4a57844916e4/TBP_Report___2023_WEC_R04_Le_Mans.pdf)
# Brandon Hartley - 3:30.475
# Ryo Hirawkawa - 3:31.753
# Sebastian Buemi - 3:31.015

avgLapTimeSecs <- 31.753+3*60


pLapRemaining <- secDiff/avgLapTimeSecs
resultTable[2,"Distance"] <- resultTable[2,"Distance"] - pLapRemaining*circuitLengthKM

nFinishers <- sum(resultTable$Pos!="DNF")

class <- unique(resultTable$Class)
cols <- brewer.pal(length(class),"Set1")

maxX <- round_any(max(resultTable$Distance)+circuitLengthKM,1000,f=ceiling)

png( file="le-mans-2023-finishing-positions.png",
     res=300,
     height=12,
     width=7,
     units="in" )

plot( x=c(0,maxX),
      y=c(0,N+1),
      type="n",
      axes=FALSE,
      xaxs="i",
      yaxs="i",
      xlab="Distance covered (km)",
      ylab="Finishing position",
      main="91st 24 Hours of Le Mans (2023)" )

abline( v=seq(500,maxX-500,by=1000), col="grey80", lty=3 )
abline( v=seq(1000,maxX,by=1000), col="grey60", lty=3 )

abline( h=N-nFinishers+0.5, lty=2, lwd=2, col="orange" )

for( i in 1:N )
{
  carFilename <- paste0("images/",resultTable[i,"No."],".png")
  carObj      <- readPNG(carFilename)

  dist <- resultTable[i,"Distance"]
  classi <- which( class %in% resultTable[i,"Class"] )

  segments( x0=0, x1=dist-300, y0=N-i+1, lwd=11,
            col=cols[classi] )

  if( dist>500 )
  {
    label <- paste(resultTable[i,"No."],resultTable[i,"Team"],resultTable[i,"Chassis"],sep="-")
    text( x=0, y=N-i+0.95, labels=label, pos=4, col="white", cex=0.6 )
  }

  addImg( obj=carObj,
          x=dist-250,
          y=N-i+1.1,
          width=500 )
}

axis( side=1, at=seq(0,maxX,by=1000) )
axis( side=2, at=c(N,seq(N-4,1,by=-5),1), labels=c(1,seq(5,N,by=5),N), las=1 )

legend( x="bottomright", lwd=7, col=cols, legend=class, bty="n" )

par(font=2)
text( x=0.9*maxX, y=N-nFinishers-1.5, cex=1.2, labels="DNF", col="orange" )
par(font=1)

dev.off()


# Two cars completed more laps than the 40th placed Hertz Team Jota Porsche Hypercar but had to retire due to an accident (#57 Kessel Racing Ferrari) and accident damage (#911 Proton Competition Porsche - https://www.youtube.com/watch?v=NpSVuXsy2Do)

# How did qualification correlate with finish








