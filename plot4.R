plot4 <- function() {
    library(dplyr)
    library(data.table)
    library(ggplot2)
    
    # Read the files into local variables
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    NEI <- data.table(NEI)
    
    
    #Problem : Across the United States, how have emissions from 
    #coal combustion-related sources changed from 1999-2008?
    #Here we need to define what "coal combustion-related sources" are
    #When I look at the SCC$EI.Sector column, I see that the most relevant
    #ones are the records "Fuel Comb - Electric Generation - Coal"
    #"Fuel Comb - Industrial Boilers, ICEs - Coal" and
    #"Fuel Comb - Comm/Institutional - Coal"
    
    #So first I will filter out these records from SCC, there are 99 such
    #records
    
    ccSCC <- SCC[SCC$EI.Sector == "Fuel Comb - Electric Generation - Coal"
                 |SCC$EI.Sector == "Fuel Comb - Industrial Boilers, ICEs - Coal"
                 |SCC$EI.Sector == "Fuel Comb - Comm/Institutional - Coal", ]
    
    
    #now filter NEI based on ccSCC$SCC to get data only for coal combustion
    #related sources
    ccNEI <- NEI[NEI$SCC %in% ccSCC$SCC, ]
    
    #Now get the total of emissions across the year
    sumdt <- summarise(group_by(ccNEI, year), sum(Emissions))
    setnames(sumdt, old="sum(Emissions)", new="Total_Emissions")
    
    #draw a base plot showing the data
    png(filename="plot4.png", width=480, height=480)
    #plot with points and draw a smoother
    with(sumdt, plot(year, Total_Emissions, pch=20,
                     axes=FALSE, ylab="Total Emissions across Coal Combustion Sources"))
    
    axis(side=1, at=c(1999, 2002, 2005, 2008))
    axis(side=2)
    box()
    model <- lm(Total_Emissions ~ year, sumdt)
    abline(model, lwd=2)
    dev.off()
}    