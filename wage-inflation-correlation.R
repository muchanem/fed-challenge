library(readxl)
wage <- data.frame(read_excel("wage-growth-data.xlsx"))
inflation <- data.frame(read_excel("InflationSectors.xlsx"))
wage <- wage[wage$Date >= "2002-04-01",]


data <- data.frame(date=wage$Date,inflation=inflation$All_items,wage=wage$Weighted.Overall)
n <- 1
corrs <- numeric(21)
conf_min <- numeric(21)
conf_max <- numeric(21)
months <- numeric(21)
# i had a rolling window version (with standard window size) of this written,
    # but there were so many datapoints it became useless
    # so, I'm doing it by years. also the error bars were massive for 4 month windows
for (i in seq(from=2002,to=2022,by=1)) {
    # if i had more time, i think i'd shift the wage data back by a month
        # because i'm fairly certain we wouldn't see wage inflation causing real
        # inflation in real time, month to month
    max_date <- paste(i,"-12-31",sep="")
    corr <- cor.test(data[data$date<=max_date,"wage"],data[data$date<=max_date,"inflation"],method="pearson")
    corrs[n] <- corr$estimate
    conf_min[n] <- corr$conf.int[1]
    conf_max[n] <- corr$conf.int[2]
    months[n] <- i
    n <- n+1

}

correlations <- data.frame(labels=months,corr=corrs,min=conf_min,max=conf_max)

library(ggplot2)

p <- ggplot(correlations, aes(x=labels,y=corr)) +
    geom_line(color="#56B4E9") +
    geom_point(color="#56B4E9") + 
    ylim(-1,1) +
    geom_ribbon(aes(ymin=min,ymax=max),alpha=0.2) +
    labs(title="Correlation Between Wage Growth and Inflation by Year (2002-2022)", x="Year", y = "Pearson's R")

print(p)
ggsave("plot.png")