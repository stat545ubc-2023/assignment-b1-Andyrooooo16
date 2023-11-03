## Excercise 1: Make a Function

    #' Summarize Data by Group
    #'
    #' This function groups a dataset by a specified variable and performs summarization on selected variables within each group.
    #'
    #' @param data The dataset to be summarized.
    #' @param group_var The variable used for grouping the data. 'group_var' is a concise and descriptive term that represents the grouping variable.
    #' @param summarise_func The summarization function to be applied (e.g., 'mean', 'sum', etc.). The name 'summarise_func' is used to convey the purpose of this argument.
    #' @param ... Ellipsis to specify variables to be summarized. The ellipsis allows flexibility in selecting multiple variables for summarization.
    #' 
    #' @return Returns a summarized dataset with calculations based on the specified summarization function and variables selected via the ellipsis. 
    #' 
    summary_function <- function(data, group_var, summarise_func, ...) {
      data %>%
        group_by({{group_var}}) %>%
        summarise(across(all_of({{...}}), .fns = list(~summarise_func(., na.rm = TRUE))))
    }

## Excercise 2: Document your Function
