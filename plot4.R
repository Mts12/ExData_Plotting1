##########################################################################
# Function: read_data_sets
# Argument: variables (columns to read)
# Out:      data_sets 
# ------------------------
# Description: 
#   This function read data recorded between 2007/02/01 and 2007/02/02
#   because reading entire data in this assignment is time consuming.
#   The function is commonly used in the other assignmment (plot1 - plot4)
##########################################################################
read_data_sets <- function(variables){
    
    # Read only first row to know start of the date and time. 
    data_sets  <- read.table("household_power_consumption.txt", 
                             header = TRUE, 
                             sep = ";", 
                             nrows = 1,
                             stringsAsFactors = FALSE
    )
    
    # Determine the number of datas to skip not to read data sets entirely
    date1  <- paste(data_sets$Date, data_sets$Time, sep = " ")    
    date1  <- strptime(date1, format = "%d/%m/%Y %H:%M:%S", tz="")
    date2  <- strptime("01/02/2007 00:00:00", format = "%d/%m/%Y %H:%M:%S", tz="")
    
    # skip_rows = N(days) x 24(hours) x 60(min)
    skip_rows <- as.numeric(date2 - date1)*24*60
    
    # read_rows = 2(days) x 24(hours) x 60(min)
    read_rows <- 2*24*60          
    
    # Read data set between 2007/02/01 and 2007/02/02. 
    data_sets  <- read.table("household_power_consumption.txt", 
                             header = TRUE, 
                             sep = ";", 
                             skip  = skip_rows,
                             nrows = read_rows,
                             stringsAsFactors = FALSE
    )
    
    names(data_sets) <- c("Date", "Time", 
                          "Global_active_power", "Global_reactive_power",
                          "Voltage", "Global_intensity",
                          "Sub_metering_1", "Sub_metering_2","Sub_metering_3"
    )
    
    # Select columns designated in variables
    data_sets <- data_sets[variables]
}

##########################################################################
# Function: plot4
# Argument: - 
# Out:      -
# ------------------------
# Description: 
#   This function draw four different graphs and save it as plot4.png
#
##########################################################################
plot4 <- function(){
    
    # Variables needed to do the assignment
    variables <- c("Date", "Time", 
                          "Global_active_power", "Global_reactive_power",
                          "Voltage", "Global_intensity",
                          "Sub_metering_1", "Sub_metering_2","Sub_metering_3"
    )
    
    # Read data set between 2007/02/01 and 2007/02/02
    data_sets <- read_data_sets(variables)
    
    # Draw graph
    library(datasets)
    par(mfrow = c(2, 2), bg = "transparent")
    
    # conbine Date and Time column
    date_time <- paste(data_sets$Date, data_sets$Time, sep =" ")
    date_time <- strptime(date_time, format = "%d/%m/%Y %H:%M:%S", tz="")
    
    ## Graph 1
    plot(date_time, # Treat date_time as x axis value
         data_sets$Global_active_power, 
         type ="n",
         xlab = "",
         ylab = "Global Active Power")
    
    lines(date_time, # Treat date_time as x axis value
          data_sets$Global_active_power, 
          col="black")
    
    ## Graph 2
    plot(date_time, # Treat date_time as x axis value
         data_sets$Voltage, 
         type ="n",
         xlab = "datetime",
         ylab = "voltage")
    
    lines(date_time, # Treat date_time as x axis value
          data_sets$Voltage, 
          col="black")
    
    ## Graph 3
    plot(date_time,
         data_sets$Sub_metering_1,
         type ="n",
         xlab = "",
         ylab = "Energy sub metering")
    
    lines(date_time, # Treat date_time as x axis value
          data_sets$Sub_metering_1, 
          col="black")
    
    lines(date_time, # Treat date_time as x axis value
          data_sets$Sub_metering_2, 
          col="red")
    
    lines(date_time, # Treat date_time as x axis value
          data_sets$Sub_metering_3, 
          col="blue")
    
    legend("topright", 
           legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), 
           col    = c("black", "red", "blue"),
           cex    = 0.7,
           lty    = 1,
           box.lty = 0,
           box.col = "transparent")
    
    ## Graph 4
    plot(date_time, # Treat date_time as x axis value
         data_sets$Global_reactive_power, 
         type ="n",
         xlab = "datetime",
         ylab = "Global_reactive_power")
    
    lines(date_time, # Treat date_time as x axis value
          data_sets$Global_reactive_power, 
          col="black")    
    
    dev.copy(png, file = "plot4.png")
    dev.off()
    
}