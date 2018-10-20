# read data from the txt file
data <- read.table("~/household_power_consumption.txt",header = T, sep = ";",as.is=T,na.strings = "?")

# create the subset of data range
subSetData <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]

# create histogram and save as png
png(filename = "plot1.png", width = 480, height = 480 )
hist(as.numeric(subSetData[,3]), main = "Global Active Power", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", col = "red")
    
# close the png file device
dev.off()