plot6 <- function() {
    library(dplyr)
    library(data.table)
    
    # Read the files into local variables
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    NEI <- data.table(NEI)
    
    #Problem: Compare emissions from motor vehicle sources in Baltimore City 
    #with emissions from motor vehicle sources in Los Angeles County, 
    #California (fips == "06037"). Which city has seen greater changes over 
    #time in motor vehicle emissions?
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
    mvNEI_b <- subset(mvNEI, fips=="24510", select=c(Emissions, year))
    
    #Next we filter mvNEI to have data only for Log Angeles
    mvNEI_la <- subset(mvNEI, fips=="06037", select=c(Emissions, year))
    
    #Now get the total of emissions across the years in baltimore
    sumdt_b <- summarise(group_by(mvNEI_b, year), sum(Emissions))
    setnames(sumdt_b, old="sum(Emissions)", new="Total_Emissions_in_Baltimore")
    
    #Now get the total of emissions across the years in los angeles
    sumdt_la <- summarise(group_by(mvNEI_la, year), sum(Emissions))
    setnames(sumdt_la, old="sum(Emissions)", new="Total_Emissions_in_LA")
    
    # To show the changes for the two counties, we will draw 2 plots
    # and draw a smoother. Visually you can make out which county has seen
    # greater changes in the emissions
    
    png(filename="plot6.png", width=700, height=480)
    #since we will be drawing 2 plots, set mfrow
    par(mfrow=c(1,2))
    
    #plot with points and draw a smoother for Baltimore
    with(sumdt_b, plot(year, Total_Emissions_in_Baltimore, pch=20,
                     axes=FALSE, 
                     ylab="Total Emissions across Motor Vehicle Sources in Baltimore"))
    
    axis(side=1, at=c(1999, 2002, 2005, 2008))
    axis(side=2)
    box()
    model <- lm(Total_Emissions_in_Baltimore ~ year, sumdt_b)
    abline(model, lwd=2)
    
    #plot with points and draw a smoother for LA
    with(sumdt_la, plot(year, Total_Emissions_in_LA, pch=20,
                       axes=FALSE, 
                       ylab="Total Emissions across Motor Vehicle Sources in LA"))
    
    axis(side=1, at=c(1999, 2002, 2005, 2008))
    axis(side=2)
    box()
    model <- lm(Total_Emissions_in_LA ~ year, sumdt_la)
    abline(model, lwd=2)
    
    dev.off()
    
}    