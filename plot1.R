plot1 <- function() {
    library(dplyr)
    library(data.table)
    
    # Read the files into local variables
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    NEI <- data.table(NEI)
    #make a new data frame with just year and Emissions from all sources 
    #for the years 1999, 2002, 2005 and 2008
    mydf <- subset(NEI, select=c(Emissions, year))
    
    #total PM2.5 emissions from all sources
    sumdt <- summarise(group_by(mydf, year), sum(Emissions))
    
    setnames(sumdt, old="sum(Emissions)", new="Total_Emissions")
    png(filename="plot1.png", width=480, height=480)
    #par(mar=c(4, 4, 2, 2))
    with(sumdt, plot(year, Total_Emissions, type="l", lwd=2, axes=FALSE))
    axis(side=1, at=c(1999, 2002, 2005, 2008))
    axis(side=2)
    box()
    dev.off()
}