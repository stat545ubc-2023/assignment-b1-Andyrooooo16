## Excercise 1 & 2: Make a Function & Document Function

### This function generates a summary of a specific column within a dataset, providing relevant basic statistics such as minimum, 1st quartile, median, mean, 3rd quartile, and maximum values. The inspiration behind this function is from my MDA2 project where I had to use individual formulas to find basic statistics regarding the age column of my tree datset.

    #' @title Summarize Column Data
    #' @param data Represents the dataset from which the column data is extracted. 'data'parameter selected as it references a dataset.
    #' @param column_name The name of the column for which a summary is generated. The parameter 'column_name' succinctly represents the specific column to be summarized.
    #' @return Returns a summary of the specified column in the dataset, including statistical measures like minimum, 1st quartile, median, mean, 3rd quartile, and maximum values.Additionally, it may output the string "Column not found in the dataset" if an incorrect column name is entered, or "Error: No valid numeric values in the column" if the column has no numeric values. 


    summarize_column <- function(data, column_name, error = TRUE) {
      if (!(column_name %in% names(data))) {
        if (error) {
          stop("Column not found in the dataset")
        } else {
          return("Column not found in the dataset")
        }
      }
      
      column_data <- data[[column_name]]
      
      convert_to_na <- function(x) {
        is_numeric <- suppressWarnings(as.numeric(as.character(x)))
        x[!is.na(is_numeric)] <- is_numeric[!is.na(is_numeric)]
        x[is.na(is_numeric)] <- NA
        return(x)
      }
      
      column_data <- convert_to_na(column_data)
      
      column_data <- as.numeric(column_data)
      non_na_values <- column_data[!is.na(column_data)]
      
      if (length(non_na_values) == 0 && all(is.na(column_data))) {
        if (error) {
          stop("Error: No valid numeric values in the column")
        } else {
          return("No valid numeric values in the column")
        }
      } else {
        return(summary(non_na_values))
      }
    }

## Excercise 3: Include Examples

### Example 1: Here is a sample dataset with 200 mixed values including NA, string and numeric. The example demonstrates that the function is able to parse through the entire dataset and remove non-numeric values before summarizing the statistically relevant features.

    dataset1 <- matrix(c("string1", "string2", 123, "string3", 456, "string4", "string5", 789, "string6", "string7", "string8", 321, "string9", "string10", 654, 987, "string11", "string12", "string13", 135, "string14", "string15", 246, "string16", 579, "string17", "string18", 753, "string19", "864", "string20", 192, "string21", "string22", 435, "string23", "678", "string24", "string25", "921", "string26", "string27", "string28", "753", "246", "string29", "987", "string30", "string31", "135", "642", "string32", "string33", NA , "string34", "210", "string35", "string36", "9", "54", "88", "23", "57", "72", "4", "92", "39", "61", "543", "876", "string37", "string38", "102", "string39", "753", "string40", "936", "string41", "string42", "279", "string43", "408", "string44", "765", "string45", "string46", "942", "string47", "513", "string48", "654", "897", "string49", "string50", "321", "string51", "246", "string52", "789", "string53", "string54", "852", "963", "string55", "string56", "741", "string57", "258", "string58", "369", "486", "string59", "string60", "975", "string61", "630", "string62", "string63", "867", "294", "string64", "531", "string65", "string66", "978", "405", "string67", "string68", "762", "string69", "819", "string70", "89", "test", "24", "56", "73", "10", "44", "62", "17", "90", "30", "5", "81", "13", "68", "49", "3", "70", "58", "27", "96", "41", "79", "2", "45", "36", "50", "86", "12", "94", "67", "25", "52", "76", "21", "85", "60", "8", "35", "91", "7", "29", "80", "18", "98", "64", "15", "63", "33", "69", "48", "16", "71", "40", "95", "22", "51", "anyString", "19", "84", "11", "47", "74", "6", "42", "55", "87", "32", "93", "38", "65", "1", "83", "20", "77", "37", "66", "31", "82"), ncol = 5)

    ## Warning in matrix(c("string1", "string2", 123, "string3", 456, "string4", :
    ## data length [211] is not a sub-multiple or multiple of the number of rows [43]

    fake_dataset <- as.data.frame(dataset1)

    colnames(fake_dataset) <- c("Column_1", "Column_2", "Column_3", "Column_4", "Column_5")

    # Display first 10 rows of the dataset
    head(fake_dataset, 10)

    ##    Column_1 Column_2 Column_3 Column_4 Column_5
    ## 1   string1      753      942 string69        7
    ## 2   string2      246 string47      819       29
    ## 3       123 string29      513 string70       80
    ## 4   string3      987 string48       89       18
    ## 5       456 string30      654     test       98
    ## 6   string4 string31      897       24       64
    ## 7   string5      135 string49       56       15
    ## 8       789      642 string50       73       63
    ## 9   string6 string32      321       10       33
    ## 10  string7 string33 string51       44       69

    summarize_column(fake_dataset, "Column_1")

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   123.0   283.5   579.0   542.2   771.0   987.0

### Example 2: Here is a sample dataset with an empty column that contains no numeric values. The example demonstrates that the function is able to determine an empty column and trigger the built in error message.

    empty_dataset <- data.frame(
      test1_Column = c(10, 20, NA, 40, 50),    
      tes2_Column = c(5, 15, 25, 35, 45),    
      test3_Column = c(6, NA, "na", 35, 45),     
      test4_Column = c(2, 1, "na", 55, 15),          
      test5_Column = c(NA, NA, NA, NA, NA)      
    )

    summarize_column(empty_dataset, "test5_Column", error = FALSE)

    ## [1] "No valid numeric values in the column"

## Excercise 4: Test the Function

    test_data <- data.frame(
      Numeric_Column = c(10, 20, NA, 40, 50),    # Includes non-numeric NA
      Another_Column = c(5, 15, 25, 35, 45),     # Numeric column with no NA
      mix_Column = c(6, NA, "na", 35, 45),     # Numeric column with no NA
      na_Column = c(2, 1, "na", 55, 15),           # Numeric column with non-numeric values replaced by NA
      onlyna_Column = c(NA, NA, NA, NA, NA)      # Contains only NA values
    )

    test_that("Testing summarize_column function", {
      # Test case 1: Vector with no NA's
      expect_equal(summarize_column(test_data, "Another_Column"), summary(c(5, 15, 25, 35, 45)))
      
      # Test case 2: Vector that has NA's
      expect_equal(summarize_column(test_data, "Numeric_Column"), summary(c(10, 20, 40, 50)))
      
      # Test case 3: Vector that has non numeric string
      expect_equal(summarize_column(test_data, "na_Column"), summary(c(2, 1, 55, 15)))
      
      # Test case 4: Vector of length 0, like numeric(0)
      expect_error(summarize_column(test_data, "Nonexistent_Column"), "Column not found in the dataset")
      
      # Test case 5: Vector that only contains NA
      expect_error(summarize_column(test_data, "onlyna_Column"), "Error: No valid numeric values in the column")
      
      # Test case 6: Vector that contains mix of strings, NA and numeric 
      expect_equal(summarize_column(test_data, "mix_Column"), summary(c(6, 35, 45)))
    })

    ## Test passed 🥇

## Summary

The tests are successful. It’s important to note that our function
handles NA values within a column by excluding them when computing the
any statistical values. If a column consists entirely of NA values, the
resulting output would be an “Error: No valid numeric values in the
column” message. In a function with mixed data types and NA values, the
function continues to return the for the values in the numeric columns
without raising an error.
