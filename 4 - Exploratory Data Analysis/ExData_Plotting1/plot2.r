# read data from the txt file
data <- read.table("~/household_power_consumption.txt",header = T, sep = ";",as.is=T,na.strings = "?")

# create the subset of data range
subSetData <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]

# convert to data type
dateTime <- paste(subSetData[,1], subSetData[,2], sep = ",")
dateTime <- strptime(dateTime, format="%d/%m/%Y,%H:%M:%S")

# create chart and save png
png(filename = "plot2.png", width = 480, height = 480 )
plot(dateTime, as.numeric(subSetData[,3]), type="l", xlab="", ylab="Global Active Power (kilowatts)")

# close the png file device
dev.off()