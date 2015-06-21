# Course  : Exploratory Data Analysis
# Project : PM2.5 Emission Trends in US between 1999 - 2008

# Download ZIP File from the Environmental Protection Agency (EPA)
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "exdata-data-NEI_data.zip")
unzip("exdata-data-NEI_data.zip")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# plot1.R
NEIperYear <- tapply(NEI$Emissions, NEI$year, sum)/1000000

# Open plot1.png file
png(filename="plot1.png", width=480, height=480, units="px", pointsize=12, bg="white")

# Create plot1.png
plot(NEIperYear, type = 'l', xlab = 'Year', ylab = expression("Total PM"[2.5] * " Emission (in million tons)"), 
     main = expression("Total PM"[2.5] * " Emission per Year"),xaxt="n" , col='blue', lwd = 3)
axis(1, at=1:4, labels = c("1999", "2002","2005","2008"))

# Closing plot1.png
dev.off ()

# Download ZIP File from the Environmental Protection Agency (EPA)
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "exdata-data-NEI_data.zip")
unzip("exdata-data-NEI_data.zip")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# plot2.R
Baltimore <- subset(NEI, fips == "24510")
BaltimorePerYear <- tapply(Baltimore$Emissions, Baltimore$year, sum)/1000

# Open plot2.png file
png(filename="plot2.png", width=480, height=480, units="px", pointsize=12, bg="white")

# Create plot2.png
plot(BaltimorePerYear, type = 'l', xlab = 'Year', ylab = expression("Total PM"[2.5] * " Emission (in thousand tons)"), 
     main = expression("Total PM"[2.5] * " Emission per Year in Baltimore City"),xaxt="n", col='blue', lwd = 3 )
axis(1, at=1:4, labels = c("1999", "2002","2005","2008"))

# Closing plot2.png
dev.off ()


# Download ZIP File from the Environmental Protection Agency (EPA)
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "exdata-data-NEI_data.zip")
unzip("exdata-data-NEI_data.zip")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# plot3.R
library(plyr)
library(ggplot2)
Baltimore <- subset(NEI, fips == "24510")
BaltimoreType <- ddply(Baltimore, .(type, year), summarize, Emissions = sum(Emissions))
names(BaltimoreType)[1] <- "Type_of_Source"
BaltimoreType
BaltimoreType[BaltimoreType[1] == "NON-ROAD", 3] <- (BaltimoreType[BaltimoreType[1] == "NON-ROAD", 3] - BaltimoreType[1,3]) * 100 / BaltimoreType[1,3]
BaltimoreType[BaltimoreType[1] == "NONPOINT", 3] <- (BaltimoreType[BaltimoreType[1] == "NONPOINT", 3] - BaltimoreType[5,3]) * 100 / BaltimoreType[5,3]
BaltimoreType[BaltimoreType[1] == "ON-ROAD", 3] <- (BaltimoreType[BaltimoreType[1] == "ON-ROAD", 3] - BaltimoreType[9,3]) * 100 / BaltimoreType[9,3]
BaltimoreType[BaltimoreType[1] == "POINT", 3] <- (BaltimoreType[BaltimoreType[1] == "POINT", 3] - BaltimoreType[13,3]) * 100 / BaltimoreType[13,3]

# Open plot3.png file
png(filename="plot3.png", width=480, height=480, units="px", pointsize=12, bg="white")

qplot(year, Emissions, data = BaltimoreType, group = Type_of_Source, color = Type_of_Source, 
      geom = c("point", "line"), ylab = expression("Change of Total Emissions, PM"[2.5] * " (% since 1999)"), 
      xlab = "Year", main = "Total Emissions in U.S. by Type of Source" )

# Closing plot3.png
dev.off ()


# Download ZIP File from the Environmental Protection Agency (EPA)
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "exdata-data-NEI_data.zip")
unzip("exdata-data-NEI_data.zip")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## combustion related sources
CoalCombustion <- SCC[SCC$EI.Sector == "Fuel Comb - Comm/Institutional - Coal", ]["SCC"]

## Subset emissions from coal combustion
EmissionFromCoal <- NEI[NEI$SCC %in% CoalCombustion$SCC, ]

## Emissions by year
EmissionsByYear <- tapply(EmissionFromCoal$Emissions, EmissionFromCoal$year, sum)

# Open plot4.png file
png(filename="plot4.png", width=480, height=480, units="px", pointsize=12, bg="white")

# create plot4.png
plot(EmissionsByYear, type='l', xlab = "Year", ylab = expression("Coal Combustion PM"[2.5] * " Emission (in tons)"), 
     xaxt="n", col = 'blue', lwd = 3, 
     main = expression("Coal Combustion PM"[2.5] * " Emissions per Year"))
axis(1, at=1:4, labels = c("1999", "2002","2005","2008"))

Closing plot4.png
dev.off ()




# Download ZIP File from the Environmental Protection Agency (EPA)
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "exdata-data-NEI_data.zip")
unzip("exdata-data-NEI_data.zip")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## List of Motor Vehicles codes from 'SCC'
MV <- unique(grep("Vehicles", SCC$EI.Sector, ignore.case = TRUE, value = TRUE))
MVCode <- SCC[SCC$EI.Sector %in% MV, ]["SCC"]

## Subset of motor vehicle from 'NEI' for Baltimore City
MVInBaltimore <- NEI[NEI$SCC %in% MVCode$SCC & NEI$fips == "24510", ]

## Emissions of motor vehicles in Baltimore City for every year
MVEmissionsByYear <- tapply(MVInBaltimore$Emissions, MVInBaltimore$year, sum)

# Open plot5.png file
png(filename="plot5.png", width=480, height=480, units="px", pointsize=12, bg="white")

# create plot5.png
plot(MVEmissionsByYear, type = 'l', xlab = "Year", xaxt = 'n', col = 'blue', lwd = 3,
     ylab = expression("Motor Vehicle Related PM"[2.5] * " Emission (in tons)"), 
     main = expression("Motor Vehicle Related PM"[2.5] * " Emissions per Year in Baltimore City"))
axis(1, at=1:4, labels = c("1999", "2002","2005","2008"))                                                                                                                  

# Closing plot5.png
dev.off ()



# Download ZIP File from the Environmental Protection Agency (EPA)
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "exdata-data-NEI_data.zip")
unzip("exdata-data-NEI_data.zip")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Comparing Balimore City with Los Angeles City
library("plyr")
library("ggplot2")
NEI<-transform(NEI,type=factor(type),year=factor(year))
cities<-NEI[NEI$fips=="24510"|NEI$fips=="06037",]
VM<-as.data.frame(SCC[grep("vehicles",SCC$SCC.Level.Two,ignore.case=T),1])
names(VM)<-"SCC"
VM <- merge(VM,cities,by="SCC")
VM$city[VM$fips=="24510"]<-"Baltimore"
VM$city[VM$fips=="06037"]<-"LA"
data<-ddply(VM,.(year,city),summarize,sum=sum(Emissions))

# Open plot6.png file
png(filename="plot6.png", width=480, height=480, units="px", pointsize=12, bg="white")

# create plot6.png
gplot<-ggplot(data,aes(year,sum,group=city,colour=city))
gplot+geom_point() +labs(title="PM2.5 Emissions from Motor Vehicle Sources per Year",
      y="Total PM2.5 Emissions (tons)")+geom_line()

# Closing plot6.png
dev.off ()

