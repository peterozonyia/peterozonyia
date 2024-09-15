# Assignment_4 Data Visualization

# Import Premium and Claim data and merge both data sets into one data
data3 <- read.csv(file.choose(), header = TRUE)
data4 <- read.csv(file.choose(), header = TRUE)
str(data3)
str(data4)

# merge premiums and claim datas
Premium_Claim <- merge(x = data3, y = data4)
Premium_Claim
str(Premium_Claim)
summary(Premium_Claim)

# For each zone, obtain the mean Premium and plot a bar chart showing the 
# mean Premium over zone. (Use any color from a palette from R( Color Brewer)

# aggregating premium data over zones
data5 <- aggregate(Premium~ZONE_NAME,data = Premium_Claim, FUN = sum)
data5

# Simple Bar Chart of Mean Premiums for different Zones
data6 <- aggregate(Premium~ZONE_NAME,data = Premium_Claim, FUN = mean)
data6

barplot(data6$Premium, main= "SIMPLE BAR CHART (Mean Premium ~ ZONE_NAME)",
  names.arg = data6$ZONE_NAME, xlab = "ZONE_NAME",
  ylab="Mean Premium", col = "red")

# Obtain a stacked bar chart for all the Zones over Sub plans by the Premium amount
data7 <- table(Premium_Claim$Sub_Plan, Premium_Claim$ZONE_NAME)
data7

barplot(data7,main="STACKED BAR CHART",xlab ="ZONE_NAME",
        ylab ="Premium Amount", col=c("cadetblue","orange","pink","red"),
        legend=rownames(data7))

# Obtain a heat map of Plan and Zone with respective average Premium
install.packages("plotly")
library(plotly)
library(ggplot2)

avg_premiums <- aggregate(Premium ~ Plan + ZONE_NAME, data = Premium_Claim, FUN = mean)
avg_premiums

plot_ly(avg_premiums, x=avg_premiums$Plan, y=avg_premiums$ZONE_NAME,
        z=avg_premiums$Premium,
        type="heatmap",connectgaps=FALSE,showscale=T)

# Obtain a pie chart using ggplot2 for Premium amount across different 
# sub plans. (Use any palette from R (Color Brewer)  
data8<-aggregate(Premium~Sub_Plan,data = Premium_Claim, FUN = sum)
data8$pct <- round(data8$Premium/sum(data8$Premium)*100)
data8

pie(data8$Premium,labels = paste(data8$Sub_Plan,"(",data8$pct,"%)"),
    col=c("darkcyan","orange","yellowgreen","red"),main="PIE CHART WITH
PERCENTAGE")
