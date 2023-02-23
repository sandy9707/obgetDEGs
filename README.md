# obgetDEGs
Gene one-bond difference analysis

A supporting R package for the whole process one click gene difference analysis.

# Description

## shelfEnvironment

This R code defines a function `shelfEnvironment` that checks or creates specified folders at a given path for storing or reading data files. The following is a detailed description of the code:

-   `file_dir_list` is a parameter used to specify a list of folders that need to be checked or created. By default, it is empty, and if no list is provided, it needs to be set manually in the function.
-   `path` is a parameter used to specify the root directory path. By default, it is the current working directory.
-   In the function, a `for` loop is used to iterate over each folder in `file_dir_list`, and for each folder, the following steps are executed:
    -   Use the `setwd` function to attempt to set the working directory to the path of the current folder. If the folder does not exist, an exception is thrown, and in this case, the folder needs to be created, and the working directory needs to be set to the path of the created folder. The `try` and `silent` parameters are used to catch the exception and ignore error messages.
    -   If the folder exists, the message "Environment check successfully!" is printed. If a folder needs to be created, the message "Environment fix successfully!" is printed.
-   After the loop ends, all folders have been checked and created (if necessary).

The application scenario of this function is when we need to specify a folder path for storing or reading data files in R. However, when executing R code, the current working directory may need to be changed to the folder path where data is stored. If the folder does not exist, it needs to be created. In this case, the `shelfEnvironment` function can help us to check and create the folder, allowing data to be read or written correctly.

# 介绍

## shelfEnvironment

这段R代码定义了一个函数`shelfEnvironment`，它的作用是检查或创建指定路径下的文件夹，用于存储或读取数据等文件。以下是该代码的详细介绍：

-   `file_dir_list`是一个参数，用于指定需要检查或创建的文件夹列表，默认为空。如果没有提供列表，则需要在函数中手动设置。
-   `path`是一个参数，用于指定根目录路径。默认为当前工作目录。
-   在函数中，使用`for`循环遍历`file_dir_list`中的每个文件夹，并对每个文件夹执行以下步骤：
    -   使用`setwd`函数尝试将工作目录设置为当前文件夹的路径。如果文件夹不存在，则会抛出异常，此时需要创建该文件夹并将工作目录设置为该文件夹路径。`try`和`silent`参数用于捕捉异常并忽略错误信息。
    -   如果文件夹存在，则打印“Environment check successfully!”的信息。如果需要创建文件夹，则打印“Environment fix successfully!”的信息。
    -   循环结束后，所有文件夹的检查和创建（如果需要）均已完成。

该函数的应用场景是：当需要在R中读取或写入数据时，需要指定存储数据的文件夹路径。但在执行R代码时，可能需要将当前工作目录更改为存储数据的文件夹路径。如果文件夹不存在，需要创建文件夹。这时，`shelfEnvironment`函数可以帮助我们检查并创建文件夹，使得数据可以正常读取或写入。