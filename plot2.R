plot2 <- function() {
    library(dplyr)
    library(data.table)
    
    # Read the files into local variables
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    NEI <- data.table(NEI)
    
    #get data for Balitmore i.e. fips = 24510
    mydf <- subset(NEI, fips=="24510", select=c(Emissions, year))
    
    sumdt <- summarise(group_by(mydf, year), sum(Emissions))
    
    setnames(sumdt, old="sum(Emissions)", new="Total_Emissions_in_Baltimore")
    png(filename="plot2.png", width=480, height=480)
    #plot with points and draw a smoother
    with(sumdt, plot(year, Total_Emissions_in_Baltimore, pch=20,
                     axes=FALSE, ylab="Total Emissions in Baltimore"))
    
    axis(side=1, at=c(1999, 2002, 2005, 2008))
    axis(side=2)
    box()
    model <- lm(Total_Emissions_in_Baltimore ~ year, sumdt)
    abline(model, lwd=2)
    dev.off()
    
}    