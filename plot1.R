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
# Function: plot1
# Argument: - 
# Out:      -
# ------------------------
# Description: 
#   This function draw histgram and save it as plot1.png
#
##########################################################################
plot1 <- function(){
    
    # Variables needed to do the assignment
    variables <- c("Date", "Time", "Global_active_power")
    
    # Read data set between 2007/02/01 and 2007/02/02
    data_sets <- read_data_sets(variables)
    
    # Draw histgram
    library(datasets)
    par(mfrow = c(1, 1), bg = "transparent")
    
    hist(data_sets$Global_active_power, 
         col  = "red",
         main = "Global Active Power",
         xlab = "Global Active Power (kilowatts)")
    
    dev.copy(png, file = "plot1.png")
    dev.off()
    
}