hpc_plot4 <- function(fromdate='2007-2-01', todate='2007-2-02'){
  
  
  ##***Reading the data & subsetting the data for the date range***
  
  hpc_data <- read.table("household_power_consumption.txt", sep=";", header = TRUE)
  hpc_data$Date <- as.Date(hpc_data$Date, format = "%d/%m/%Y")
  ##hpc_subset <-subset(hpc_data,as.Date(Date) >= '2007-2-01' & as.Date(Date) <= '2007-2-2')
  hpc_subset <-subset(hpc_data,as.Date(Date) >= fromdate & as.Date(Date) <= todate)
  
  ##***All CLASS conversions made ****
  hpc_subset$New <- do.call(paste, c(hpc_subset[c("Date","Time")], sep=" "))
  hpc_subset$Global_active_power <- as.numeric(as.character(hpc_subset$Global_active_power))
  hpc_subset$New <- strptime(hpc_subset$New, format="%Y-%m-%d %H:%M", tz="UTC")
  hpc_subset$Sub_metering_1<- as.numeric(as.character(hpc_subset$Sub_metering_1))
  hpc_subset$Sub_metering_2 <- as.numeric(as.character(hpc_subset$Sub_metering_2))
  hpc_subset$Sub_metering_3 <- as.numeric(as.character(hpc_subset$Sub_metering_3))
  hpc_subset$Voltage <-as.numeric(as.character(hpc_subset$Voltage))
  hpc_subset$Global_reactive_power <-as.numeric(as.character(hpc_subset$Global_reactive_power))
  
  ##**** Code for the graphing***
  
  ##*************Plot 4 Draw Base Plotting(Line) for to see all four plots**********************
  
   
  par(mfrow = c(2,2) , mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
  with (hpc_subset,
{plot(New,Global_active_power, pch=".", type="l", xlab = ' ', ylab='Global Active Power' )
 plot(New, Voltage, pch=".", type="l", xlab = 'datetime')
 plot(New, Sub_metering_1, pch=".", type="l", col="black",xlab= ' ', ylab = 'Energy sub metering')
 lines(New, Sub_metering_2, col="red")
 lines(New, Sub_metering_3, col="blue")
 legend("topright", pch = '___', col = c("black", "red", "blue"), bty='n',legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
 plot(New, Global_reactive_power, pch=".", type="l", col="black", xlab = 'datetime')
 mtext ("Plot4", outer = TRUE)
})
  ##**********Clear memory by calling the garbage collector*********************
  gc()
}