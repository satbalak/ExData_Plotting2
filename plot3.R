plot3 <- function() {
    library(dplyr)
    library(data.table)
    library(ggplot2)
    
    # Read the files into local variables
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    NEI <- data.table(NEI)
    
    #select Emissions, year and type for Baltimore
    mydf <- subset(NEI, fips=="24510", select=c(Emissions, year, type))
    
    #sum by year and type, so that we get the total emissions across
    #various sources (or types)
    sumdt <- summarise(group_by(mydf, year, type), sum(Emissions))
    setnames(sumdt, old="sum(Emissions)", new="Total_Emissions")
    
    #p <- qplot(year, Total_Emissions, data=sumdt, facets=.~type, 
    #           geom=c("point", "smooth"), method=lm, se=FALSE)
    
    #draw the plot, add the facets and smoother.
    p <- ggplot(sumdt, aes(x=year, y=Total_Emissions))
    p <- p + geom_point() + facet_grid(.~type) + 
        geom_smooth(method="lm", se=FALSE)
    # change the orientation of xlab to make it more readable
    p <- p + theme(text = element_text(size=10),
                   axis.text.x=element_text(angle=90, vjust=1)) 
    
    ggsave(filename="plot3.png", plot=p, width=5, height=2.2)
    
}    