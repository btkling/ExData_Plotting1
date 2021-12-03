library(dplyr)
library(data.table)
library(lubridate)

# load the data
hpc <- fread("./household_power_consumption.txt")

# format the data
hpc <- hpc %>%
    filter(dmy(Date) == "2007-02-01" | dmy(Date) == "2007-02-02") %>% #filter dates
    mutate(dt = dmy_hms(paste(Date, Time))) %>% #Parse date &time together
    mutate(across(Global_active_power:Sub_metering_3, na_if, y = "?")) %>% #Parse NAs %>%
    mutate(across(Global_active_power:Sub_metering_3, as.numeric)) # convert to numeric 

#Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels

# Plot 1: a histogram of Global Active Power (in kw)
png(file = "plot1.png", width = 480, height = 480)
hist(hpc$Global_active_power, 
     col = "red", 
     main = "Global Active Power", 
     xlab = "Global Active Power (kilowatts)"
     )
dev.off()


# Plot 2: a solid line chart of Global Active Power (in kw) vs. date&time
png(file = "plot2.png", width = 480, height = 480)
plot(hpc$Global_active_power ~ hpc$dt, 
     type = "n", 
     main = "", 
     xlab = "",
     ylab = "Global Active Power (kilowatts)"
     )
lines(hpc$Global_active_power ~ hpc$dt)
dev.off()


# Plot 3: a combination of 3 different line charts measuring energy sub metering over time
png(file = "plot3.png", width = 480, height = 480)
with(hpc, plot(Sub_metering_3 ~ dt, 
               ylim = c(0,max(Sub_metering_1, Sub_metering_2, Sub_metering_3)),
               main = "",
               xlab = "",
               ylab = "Energy sub metering",
               type = "n"
               )
     )
legend(x = "topright", 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       lty = 1,
       col = c("black", "red", "blue")
       )
with(hpc, lines(Sub_metering_1 ~ dt, col = "black"))
with(hpc, lines(Sub_metering_2 ~ dt, col = "red"))
with(hpc, lines(Sub_metering_3 ~ dt, col = "blue"))
dev.off()

# Plot 4: 4 charts:
    # 1. top-left - Global Active Power vs DateTime (same as plot 2)
    # 2. top-right - Voltage vs DateTime
    # 3. bottom-left - Energy Sub metering over time (same as plot 3)
    # 4. bottom-right - Global Reactive Power vs. DateTime

png(file = "plot4.png", width = 480, height = 480)

# set parameter to show 4 plots
par(mfrow = c(2,2))

# draw the first plot
plot(hpc$Global_active_power ~ hpc$dt, 
     type = "n", 
     main = "", 
     xlab = "",
     ylab = "Global Active Power (kilowatts)"
)
lines(hpc$Global_active_power ~ hpc$dt)

# draw the second plot
with(hpc, plot(Voltage ~ dt, 
               xlab = "datetime",
               ylab = "Voltage", 
               type = "n"
               )
     )
with(hpc, lines(Voltage ~ dt, lty = 1))

# draw the third plot
with(hpc, plot(Sub_metering_3 ~ dt, 
               ylim = c(0,max(Sub_metering_1, Sub_metering_2, Sub_metering_3)),
               main = "",
               xlab = "",
               ylab = "Energy sub metering",
               type = "n"
)
)
legend(x = "topright", 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       bty = "n",
       lty = 1,
       col = c("black", "red", "blue")
)
with(hpc, lines(Sub_metering_1 ~ dt, col = "black"))
with(hpc, lines(Sub_metering_2 ~ dt, col = "red"))
with(hpc, lines(Sub_metering_3 ~ dt, col = "blue"))


# draw the fourth plot
with(hpc, plot(Global_reactive_power ~ dt, 
               xlab = "datetime",
               ylab = "Global_reactive_power", 
               type = "n"
)
)
with(hpc, lines(Global_reactive_power ~ dt, lty = 1))
dev.off()