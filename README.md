# 2nd Mini Project

## Item 1

* Download and import `dplyr` library to run functions such as `select`, `group_by`, and `summarize_at` used in this solution code.

``` r
library(dplyr)
```

* Download and extract required dataset from the provided link in the project guide.
* Import data contained in `train` and `test` dataset using `importDataset()` function. When executed, the function creates global variable containing data imported using the `read.table()` function. Consult the `README.txt` file included for more information regarding the dataset.

``` r
importDataset <- function() {
    activity_labels <<- read.table("specdata/activity_labels.txt", col.names = c("activity_id", "activity_name"))
    feature_labels <<- read.table("specdata/features.txt", col.names = c("feature_id", "feature_name"))

    train_set <<- read.table("specdata/train/X_train.txt", col.names = feature_labels$feature_name)
    train_labels <<- read.table("specdata/train/y_train.txt", col.names = c("activity_id"))
    train_subject <<- read.table("specdata/train/subject_train.txt", col.names = c("subject_id"))

    test_set <<- read.table("specdata/test/X_test.txt", col.names = feature_labels$feature_name)
    test_labels <<- read.table("specdata/test/y_test.txt", col.names = c("activity_id"))
    test_subject <<- read.table("specdata/test/subject_test.txt", col.names = c("subject_id"))

    print("Datasets imported successfully!")
}
```

* Merge the `train` and `test` set by calling `mergeDataset()` function. The function first checks if all the required variable are set. `importDataset()` is called otherwise. Merging of the dataset is done using a combination of `rbind()` and `cbind()` function. A global variable containing data from both sets is declared after linking `activity_names` to `activity_id`.

``` r
mergeDataset <- function() {
    if (!(exists("activity_labels") &&
        exists("feature_labels") &&
        exists("train_set") &&
        exists("train_labels") &&
        exists("train_subject") &&
        exists("test_set") &&
        exists("test_labels") &&
        exists("test_subject"))) {
        importDataset()
    }

    merged_subjects <- rbind(train_subject, test_subject)
    merged_sets <- rbind(train_set, test_set)
    merged_labels <- rbind(train_labels, test_labels)
    merged_labels$activity_name <- activity_labels$activity_name[match(merged_labels$activity_id, activity_labels$activity_id)]

    merged_dataset <<- cbind(merged_subjects, merged_labels, merged_sets)

    print("Train and test datasets merged successfully!")
}
```

* Extract the mean and standard deviation of the respective measurements with `extractMeansStd()`. Preliminary check is done on `merge_dataset` variable to avoid errors when running the function before importing and merging datasets. The function returns a data frame with columns: `subject_id`, `activity_name`, and means and standard deviations of various measurements.

``` r
extractMeansStd <- function() {
    if (!exists("merged_dataset")) {
        mergeDataset()
    }

    merged_dataset %>%
        select("subject_id", "activity_name", contains("mean") | contains("std"))
}
```

* Using the data from the previous function, generate a separate dataset summarizing the measurements of each subject at a given activity with the following function. `generateTidyMeans()` cleans the data by grouping the measurements according to `subject_id` and `activity_name`. Each group is further processed by compressing the data into their respective means. The resulting data frame is stored to a global variable and exported as a txt file.

``` r
generateTidyMeans <- function() {
    tidy_means <<- extractMeansStd() %>% 
        group_by(subject_id, activity_name) %>%
        summarize_at(-c(1:2), mean)

    write.table(tidy_means, file = "tidy_means.txt", row.names = FALSE)
}
```

- - -

## Item 2

* Download and import the following libraries in order to execute the algorithms written for this item. 

```r
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(scales)
```
* Download and extract required dataset from the provided link in the project guide.
* Import the dataset from the text file. Since only a subset of the dataset is needed for analysis, it is advised to import only the needed part for optimal memory usage. The code below does this by using `dplyr`'s pipe operator. A new column is added named `DateTime` to hold the transposed `DateTime` object from `Date` and `Time` column. Subsetting is done using `filter()` with the Date range as parameters.

```r
data <- read.table("specdata/household_power_consumption.txt", header = TRUE, sep=";", na.strings = "?") %>%
    mutate(DateTime = dmy_hms(paste(Date, Time))) %>%
    filter(DateTime >= ymd("2007-02-01"), DateTime < ymd("2007-02-03"))
```

* Create a histogram using `ggplot()` and `geom_histogram()` with the column `Global_active_power`. Customize the theme parameters to match the output shown on the project guide. The general shape of plot 1 was achieved by setting the number of bins of the histogram. This was determined by trial-and-error starting from 15 to 20.

```r
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
```

* Create a line plot using `ggplot`'s `geom_line()`. Parameters used for the plot are `Global_active_power` and `DateTime`. Similar theme parameters are applied to match the output on the project guide as close as possible. `scale_x_datetime()` from `scales` library is used to transform the numeric dates in the x-axis to their respective day names.

```r
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
```

* Create a layered line plot by calling `geom_line()` for each `Sub_metering_x` column. The same with the previous plots, theme was customized in addition to the name of the day and the legend of the plot.

```r
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
```

* Before proceeding with plot 4, subplots should be created first. 2 subplots were already created from the previous plots. The other subplots are created as follows. `plot4_2` is a line plot of `Voltage` and `DateTime`. Its constructor is fairly straightforward.

```r
plot4_2 <- data %>%
    select(Voltage, DateTime) %>%
    ggplot(aes(x = DateTime, y = Voltage)) +
        theme(panel.grid.major = element_blank()) +
        theme(panel.grid.minor = element_blank()) +
        theme(panel.background = element_blank()) +
        theme(axis.line = element_line(colour = "black")) +

        geom_line() +
        scale_x_datetime(labels = date_format("%A"))
```

* The same with the previous plot, `plot4_4` is also a line plot. The way it was created is also similar with the exception of the parameter of the x-axis.  

```r
plot4_4 <- data %>%
    select(Global_reactive_power, DateTime) %>%
    ggplot(aes(x = DateTime, y = Global_reactive_power)) +
        theme(panel.grid.major = element_blank()) +
        theme(panel.grid.minor = element_blank()) +
        theme(panel.background = element_blank()) +
        theme(axis.line = element_line(colour = "black")) +

        geom_line() +
        scale_x_datetime(labels = date_format("%A"))
```

* To assemble `plot4`, `ggarrange()` from `ggpubr` is used. Parameters to be supplied to the function are the previously made plots with a few adjustments on the theming. The number of columns and rows to fit the subplots are also declared.

```r
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
```

* Export the plots as PNG files by calling the `png()` function supplied with the filename to be saved as. Recall the variable names holding the `ggplot` data. Encode the variable name after the `png()` function call to store the plot data to the PNG file. Run `dev.off()` to release the handle on the file. Repeat the process until all plots are exported.

```r
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
```

* * *

###### Note: A Jupyter notebook for each item is also supplied in addition to the actual source codes for algorithm demonstration. 