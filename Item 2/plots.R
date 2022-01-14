# Import required libraries

library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(scales)

# Import required data from dataset file

data <- read.table("specdata/household_power_consumption.txt", header = TRUE, sep=";", na.strings = "?") %>%
    mutate(DateTime = dmy_hms(paste(Date, Time))) %>%
    filter(DateTime >= ymd("2007-02-01"), DateTime < ymd("2007-02-03"))

# Construct plot 1

plot1 <- data %>%
    select(Global_active_power) %>%
    ggplot(aes(x = Global_active_power)) +
        theme(panel.grid.major = element_blank()) +
        theme(panel.grid.minor = element_blank()) +
        theme(panel.background = element_blank()) +
        theme(axis.line = element_line(colour = "black")) +

        geom_histogram(bins=18, fill="red", color="black") +

        ggtitle("Global Active Power") +
        theme(plot.title = element_text(face="bold")) +
        theme(plot.title = element_text(hjust = 0.5)) +

        xlab("Global Active Power (kilowatts)") + 
        theme(axis.title.y = element_blank())

# Construct plot 2

plot2 <- data %>%
    select(Global_active_power, DateTime) %>%
    ggplot(aes(x = DateTime, y = Global_active_power)) +
        theme(panel.grid.major = element_blank()) +
        theme(panel.grid.minor = element_blank()) +
        theme(panel.background = element_blank()) +
        theme(axis.line = element_line(colour = "black")) +

        geom_line() +
        scale_x_datetime(labels = date_format("%A")) +

        ylab("Global Active Power (kilowatts)") + 
        theme(axis.title.x = element_blank())

# Construct plot 3

plot3 <- data %>%
    select(Sub_metering_1, Sub_metering_2, Sub_metering_3, DateTime) %>%
    ggplot() + 
        theme(panel.grid.major = element_blank()) +
        theme(panel.grid.minor = element_blank()) +
        theme(panel.background = element_blank()) +
        theme(axis.line = element_line(colour = "black")) +

        geom_line(aes(x = DateTime, y = Sub_metering_1, colour = "Sub_metering_1")) +
        geom_line(aes(x = DateTime, y = Sub_metering_2, colour = "Sub_metering_2")) +
        geom_line(aes(x = DateTime, y = Sub_metering_3, colour = "Sub_metering_3")) +

        theme(legend.position = c(0.9, 0.95)) +
        theme(legend.title = element_blank()) +
        scale_color_manual(values = c("black", "red", "blue")) +
        scale_x_datetime(labels = date_format("%A")) +

        ylab("Energy sub metering") + 
        theme(axis.title.x = element_blank())

# Construct plot 4 subplot 2

plot4_2 <- data %>%
    select(Voltage, DateTime) %>%
    ggplot(aes(x = DateTime, y = Voltage)) +
        theme(panel.grid.major = element_blank()) +
        theme(panel.grid.minor = element_blank()) +
        theme(panel.background = element_blank()) +
        theme(axis.line = element_line(colour = "black")) +

        geom_line() +
        scale_x_datetime(labels = date_format("%A"))

# Construct plot 4 subplot 4

plot4_4 <- data %>%
    select(Global_reactive_power, DateTime) %>%
    ggplot(aes(x = DateTime, y = Global_reactive_power)) +
        theme(panel.grid.major = element_blank()) +
        theme(panel.grid.minor = element_blank()) +
        theme(panel.background = element_blank()) +
        theme(axis.line = element_line(colour = "black")) +

        geom_line() +
        scale_x_datetime(labels = date_format("%A"))

# Arrange subplots into a single plot

plot4 <- ggarrange(plot2 +
                       ylab("Global Active Power") +
                       theme(axis.title.x = element_text(color = "white")) +
                       theme(plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm")),
                   plot4_2 +
                       theme(plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm")),
                   plot3 + theme(legend.position = c(0.8, 0.9)) +
                       theme(axis.title.x = element_text(color = "white")) +
                       theme(plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm")),
                   plot4_4 +
                       theme(plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm")),
                   ncol = 2, nrow = 2)

# Export plots as PNG

png(file = "plot1.png")
plot1
dev.off()

png(file = "plot2.png")
plot2
dev.off()

png(file = "plot3.png")
plot3
dev.off()

png(file = "plot4.png")
plot4
dev.off()