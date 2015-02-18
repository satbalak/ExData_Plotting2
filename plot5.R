plot5 <- function() {
    library(dplyr)
    library(data.table)
    library(ggplot2)
    
    # Read the files into local variables
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    NEI <- data.table(NEI)
    
    #Problem: How have emissions from motor vehicle sources changed from 
    #1999-2008 in Baltimore City?
    #We first get the SCC codes relevant to Motor Vehicle sources
    #To do this, we see that SCC.Level.One should be "Mobile Sources"
    #and SCC.Level.Two should be any highway or off-highway vehicle and we
    #should exclude aircraft etc. Lets do this now
    msSCC <- SCC[SCC$SCC.Level.One == "Mobile Sources", ]
    
    mvSCC <- msSCC[msSCC$SCC.Level.Two == "Highway Vehicles - Gasoline"
                   |msSCC$SCC.Level.Two == "Highway Vehicles - Diesel"
                   |msSCC$SCC.Level.Two == "Off-highway Vehicle Gasoline, 2-Stroke"
                   |msSCC$SCC.Level.Two == "Off-highway Vehicle Gasoline, 4-Stroke"
                   |msSCC$SCC.Level.Two == "Off-highway Vehicle Diesel", ]
    
    #Now, we will filter NEI based the SCC in the motor vehicle sources
    mvNEI <- NEI[NEI$SCC %in% mvSCC$SCC, ]
    
    #Next we filter mvNEI to have data only for Baltimore
    mydf <- subset(mvNEI, fips=="24510", select=c(Emissions, year))
    
    #Now get the total of emissions across the years
    sumdt <- summarise(group_by(mydf, year), sum(Emissions))
    setnames(sumdt, old="sum(Emissions)", new="Total_Emissions")
    
    #draw a base plot showing the data
    png(filename="plot5.png", width=480, height=480)
    #plot with points and draw a smoother
    with(sumdt, plot(year, Total_Emissions, pch=20,
                     axes=FALSE, 
                     ylab="Total Emissions across Motor Vehicle Sources in Baltimore"))
    
    axis(side=1, at=c(1999, 2002, 2005, 2008))
    axis(side=2)
    box()
    model <- lm(Total_Emissions ~ year, sumdt)
    abline(model, lwd=2)
    dev.off()
}    