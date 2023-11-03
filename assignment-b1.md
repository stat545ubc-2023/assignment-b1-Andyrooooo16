## Excercise 1 & 2: Make a Function & Document Function

    options(repos = "https://cloud.r-project.org/")

    install.packages("roxygen2")

    ## Installing package into 'C:/Users/andre/AppData/Local/R/win-library/4.3'
    ## (as 'lib' is unspecified)

    ## package 'roxygen2' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\andre\AppData\Local\Temp\RtmpIfPpXY\downloaded_packages

    library(roxygen2)

    ## Warning: package 'roxygen2' was built under R version 4.3.2

    #' @title Summarize Column Data
    #'
    #' This function generates a summary of a specific column within a dataset, providing relevant basic statistics such as minimum, 1st quartile, median, mean, 3rd quartile, and maximum values. The inspiration behind this function is from my MDA2 project where I had to use individual formulas to find basic statistics regarding the age column of my tree datset. 
    #'
    #' @param data Represents the dataset from which the column data is extracted. 'data' is a standard term widely used to reference a dataset.
    #' @param column_name The name of the column for which a summary is generated. The parameter 'column_name' succinctly represents the specific column to be summarized.
    #' 
    #' @return Returns a summary of the specified column in the dataset, including statistical measures like minimum, 1st quartile, median, mean, 3rd quartile, and maximum values.


    summarize_column <- function(data, column_name) {
      if (!(column_name %in% names(data))) {
        stop("Column not found in the dataset")
      }
      
      column_data <- data[[column_name]]
      
      # Apply the convert_to_na functionality to replace non-numeric elements with NA
      convert_to_na <- function(x) {
        is_numeric <- suppressWarnings(as.numeric(x))
        x[is.na(is_numeric)] <- NA
        return(x)
      }
      
      # Apply the conversion function to the specified column
      column_data <- convert_to_na(column_data)
      
      # Convert to numeric and summarize the column, excluding NA values
      column_data <- as.numeric(column_data)
      summary_data <- summary(column_data[!is.na(column_data)])
      
      return(summary_data)
    }

    dataset1 <- matrix(c(
      89, "test", 24, "56", 73, 10, 44, 62, 17, 90, 30, 5, 81, 13, 68, 49, 3, 70, 58, 27, 96, 
      41, 79, 2, 45, 36, 50, 86, 12, 94, 67, 25, 52, 76, 21, 85, 60, 8, 35, 91, 7, 
      29, 80, 18, 98, 64, 15, 63, 33, 69, 48, 16, 71, 40, 95, 22, 51, "anyString", 19, 
      84, 11, 47, 74, 6, 42, 55, 87, 32, 93, 38, 65, 1, 83, 20, 77, 37, 66, 31, 82, 
      9, 54, 88, 23, 57, 72, 4, 92, 39, 61), ncol = 5)

    ## Warning in matrix(c(89, "test", 24, "56", 73, 10, 44, 62, 17, 90, 30, 5, : data
    ## length [89] is not a sub-multiple or multiple of the number of rows [18]

    fake_dataset <- as.data.frame(dataset1)

    colnames(fake_dataset) <- c("Column_1", "Column_2", "Column_3", "Column_4", "Column_5")

    print(fake_dataset)

    ##    Column_1 Column_2 Column_3  Column_4 Column_5
    ## 1        89       58       60        95       83
    ## 2      test       27        8        22       20
    ## 3        24       96       35        51       77
    ## 4        56       41       91 anyString       37
    ## 5        73       79        7        19       66
    ## 6        10        2       29        84       31
    ## 7        44       45       80        11       82
    ## 8        62       36       18        47        9
    ## 9        17       50       98        74       54
    ## 10       90       86       64         6       88
    ## 11       30       12       15        42       23
    ## 12        5       94       63        55       57
    ## 13       81       67       33        87       72
    ## 14       13       25       69        32        4
    ## 15       68       52       48        93       92
    ## 16       49       76       16        38       39
    ## 17        3       21       71        65       61
    ## 18       70       85       40         1       89

    summarize_column(fake_dataset, "Column_2")

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    2.00   29.25   51.00   52.89   78.25   96.00

    #test
    test <- data.frame (Numeric_Column = c(10, 20, "na", 40, 50) )

    summarize_column(test, "Numeric_Column")

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    10.0    17.5    30.0    30.0    42.5    50.0

    print(summary(c(10, 20, 40, 50)))

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    10.0    17.5    30.0    30.0    42.5    50.0

    # Example dataset
    test_data <- data.frame(
      Numeric_Column = c(10, 20, NA, 40, 50),  # Includes non-numeric NA
      Another_Column = c(5, 15, 25, 35, 45),     # Numeric column with no NA
      na_Column = c(2, 1, "na", 55, 15)     # Numeric column with non numeric

    )

    # Test scenarios
    test_that("Testing summarize_column function", {
      # Test case 1: Vector with no NA's
      expect_equal(summarize_column(test_data, "Another_Column"), summary(c(5, 15, 25, 35, 45)))
      
      # Test case 2: Vector that has NA's
      expect_equal(summarize_column(test_data, "Numeric_Column"), summary(c(10, 20, 40, 50)))
      
      # Test case 3: Vector that has non numeric string
      expect_equal(summarize_column(test_data, "na_Column"), summary(c(2, 1, 55, 15)))
      
      # Test case 4: Vector of length 0, like numeric(0)
      expect_error(summarize_column(test_data, "Nonexistent_Column"), "Column not found in the dataset")
    })

    ## Test passed 🌈
