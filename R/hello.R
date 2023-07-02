# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# .libPaths( c( .libPaths(), "/home/r/R/x86_64-redhat-linux-gnu-library/4.1") )
library(librarian)
shelf(xml2, GEOquery, limma, dplyr)

options(stringsAsFactors = FALSE)
# proxy----
proxy <- function(ip = "", port = "7890") { # ip <- ""
  if ("" %in% ip) {
    Sys.setenv(http_proxy = "")
    Sys.setenv(https_proxy = "")
    Sys.setenv(all_proxy = "")
    print("Don't use proxy! ")
  } else {
    Sys.setenv(http_proxy = paste0("http://", ip, ":", port))
    Sys.setenv(https_proxy = paste0("http://", ip, ":", port))
    Sys.setenv(all_proxy = paste0("socks5://", ip, ":", port))
    print(paste("Use proxy ", ip, ":", port, " successful! ", sep = ""))
  }
}
# environment_check----
## Can create mutiple folder
# shelfEnvironment函数定义，参数为file_dir_list和path
shelfEnvironment <- function(file_dir_list = file_dir_list, path = ".") {
  # 对于file_dir_list中的每个元素
  for (i in seq_along(file_dir_list)) { # i <- 1
    # 取出file_dir_list[i]的值，作为文件夹名字
    file_dir <- file_dir_list[i]
    # 试图将工作路径设置为指定的路径
    environment_check <- try(setwd(paste(path, file_dir, sep = "/")),
      silent = TRUE
    )
    # 如果出现错误，即路径不存在，则创建路径
    if ("try-error" %in% class(environment_check)) {
      dir.create(paste(path, file_dir, sep = "/"), recursive = TRUE)
      print("Environment fix successfully!") # 输出创建路径的信息
      setwd(paste(path, file_dir, sep = "/")) # 设置工作路径为创建的路径
    } else {
      print("Environment check successfully!") # 输出路径存在的信息
    }
  }
}

# get GEOfile's downloading url----
# get file's downloading url
getFileList <- function(gseAcc, typeDown = "matrix") { # gseAcc <- "GSE166424"
  # get file directional content
  getDirListing <- function(url) {
    # Takes a URL and returns a character vector of filenames
    a <- xml2::read_html(url)
    fnames <- grep("^G", xml_text(xml_find_all(a, "//a/@href")), value = TRUE)
    return(fnames)
  }

  # get a type of file
  if ("matrix" %in% typeDown) {
    gdsurl <- "https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/"
  } else if ("suppl" %in% typeDown) {
    gdsurl <- "https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/suppl/"
  } else if ("annot" %in% typeDown) {
    gdsurl <- "https://ftp.ncbi.nlm.nih.gov/geo/platforms/%s/%s/annot/"
  }

  # merge links in list
  stub <- gsub("\\d{1,3}$", "nnn", gseAcc, perl = TRUE)
  b <- getDirListing(sprintf(gdsurl, stub, gseAcc))
  ret <- list()
  for (x in 1:length(b))
  {
    ret[[x]] <- sprintf(
      paste0(gdsurl, "%s"),
      stub, gseAcc, b[x]
    )
  }
  merge <- c(unlist(ret))
  print(merge)
  return(merge)
}
# download form file list and unzip
getFile <- function(FileList, unzip = FALSE, readin = F) {
  print(FileList)
  # select the number of list
  if (1 %in% length(FileList)) { # FileList <- supplUrlList[1]
    fileSelectNum <- 1
  } else {
    fileSelectNum <- as.numeric(readline("Select the file to download: "))
  }
  fileName <- str_remove_all(string = FileList[fileSelectNum], pattern = ".*\\/")
  if (fileName %in% list.files()) {
    print("The file is downloaded! ")
  } else {
    download.file(url = FileList[fileSelectNum], destfile = fileName)
  }
  output <- fileName
  # unzip
  insidefileName <- str_extract(string = fileName, pattern = ".*(?=\\.)")
  if (unzip == TRUE) {
    Suffix <- str_remove_all(string = fileName, pattern = ".*\\.")
    if (insidefileName %in% list.files()) {
      print("The file is unziped!")
    } else {
      if ("gz" %in% Suffix) {
        gunzip(fileName, remove = F)
        print("The file.gz is unziping!")
      } else {
        print("The file isn't a gz type file!")
        break
      }
    }
    # Display the filename
    if (insidefileName %in% list.files()) {
      fileNameRead <- insidefileName
      print(paste0("Read the file: ", fileNameRead))
    } else {
      fileNameRead <- setdiff(list.files(), list.files(pattern = "Rdata|gz"))
      print(fileNameRead)
      if (1 %in% length(fileNameRead)) {
        fileSelectNum <- 1
      } else {
        fileSelectNum <- as.numeric(readline("Select the file to load in: "))
      }
      fileNameRead <- fileNameRead[fileSelectNum]
      print(paste0("Read the file: ", fileNameRead))
    }
    output <- fileNameRead
  } else {

  }

  # Read in
  if (readin == T) {
    SuffixRead <- str_remove_all(string = fileNameRead, pattern = ".*\\.")
    if ("csv" %in% SuffixRead) {
      tem <- read.table(fileNameRead,
        sep = ",", quote = "",
        fill = TRUE, header = TRUE, check.names = F
      )
    } else if ("txt" %in% SuffixRead) {
      tem <- read.table(fileNameRead,
        sep = "\t", quote = "", fill = TRUE, header = TRUE, check.names = F
      )
    } else if ("annot" %in% SuffixRead) {
      tem <- read.delim(paste0(gplAcc, ".annot"), stringsAsFactors = FALSE, skip = 27)
    } else {
      print("I can't read the format, please matually imput")
    }
    output <- tem
    print("Read in completed!")
  }
  return(output)
}
# ID transform
idTrans <- function(dat = dat, type = "ENSG") {
  if (type == "ENSG") {
    # org.Hs.eg.db转换
    gene <- rownames(dat)
    shelf(DOSE, clusterProfiler, org.Hs.eg.db)
    geneSymbol <- bitr(gene,
      fromType = "ENSEMBL", # fromType为输入数据ID类型
      toType = "SYMBOL", # toType是指目标类型
      OrgDb = org.Hs.eg.db
    ) # Orgdb是指对应的注释包，这是人的注释包，相关注释包请百度
    head(geneSymbol)
    colnames(geneSymbol) <- c("id", "SYMBOL")
  } else {}
  dat$id <- rownames(dat)
  rt <- merge(geneSymbol, dat, by.x = "id", by.y = "id", all = T)
  dat <- rt %>%
    mutate(rowMean = rowMeans(.[grep("GSM", names(.))])) %>%
    arrange(desc(rowMean)) %>%
    distinct(SYMBOL, .keep_all = T) %>%
    dplyr::filter(SYMBOL != "") %>%
    dplyr::select(-rowMean) %>%
    dplyr::select(-id) %>%
    column_to_rownames(var = "SYMBOL")
  return(dat)
}
degWork <- function(dat = dat, degWorkWay = "limma") {
  if ("limma" %in% degWorkWay) {
    cat("Use the limma package to analyse! \nYou should \n1. judge if you need the log2 transformation. \n2. Start analysis. \n")
    # log2 transformation
    ex <- dat
    qx <- as.numeric(quantile(ex, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm = T))
    LogC <- (qx[5] > 100) ||
      (qx[6] - qx[1] > 50 && qx[2] > 0)
    if (LogC) {
      ex[which(ex <= 0)] <- NaN
      dat <- log2(ex)
      print("log2 transform finished")
    } else {
      print("log2 transform not needed")
    }

    cat("range(dat) is \n", paste(range(dat), collapse = " --> "))
    # boxplot(dat,outline=FALSE, notch=T,col=group_list,las=2)
    library(limma)
    dat <- normalizeBetweenArrays(dat)
    boxplot(dat, outline = FALSE, notch = TRUE, col = group_list, las = 2)

    # deg
    dat[1:4, 1:4]
    table(group_list) # table函数，查看group_list中的分组个数
    # 非匹配分析
    library(limma)
    design <- model.matrix(~group_list)
    fit <- lmFit(dat, design)
    fit <- eBayes(fit)
    deg <- topTable(fit, coef = 2, number = Inf)
  } else if ("DESeq2" %in% degWorkWay) {
    cat("Use the DESeq2 package to analyse! \nYou should \n1. Start analysis.")
    shelf(DESeq2)
    condi <- ifelse(rownames(pd) %in% rownames(pdcon), "control", "treat")
    coldata <- data.frame(condition = factor(condi, levels = c("control", "treat")))
    # 第一步，构建 DESeqDataSet 对象
    dds <- DESeqDataSetFromMatrix(countData = dat, colData = coldata, design = ~condition)
    # 第二步，计算差异倍数并获得 p 值
    # 备注：parallel = TRUE 可以多线程运行，在数据量较大时建议开启
    dds1 <- DESeq(dds, fitType = "mean", minReplicatesForReplace = 7, parallel = FALSE)
    # 注意，需将 treat 在前，control 在后，意为 treat 相较于 control 中哪些基因上调/下调
    res <- results(dds1, contrast = c("condition", "treat", "control"))
    res
    res1 <- data.frame(res, stringsAsFactors = FALSE, check.names = FALSE)
    ## 筛选差异表达基因
    # 首先对表格排个序，按 padj 值升序排序，相同 padj 值下继续按 log2FC 降序排序
    res1 <- res1[order(res1$padj, res1$log2FoldChange, decreasing = c(FALSE, TRUE)), ]
    deg <- res1
    colnames(deg)[match(c("log2FoldChange", "pvalue", "padj"), colnames(deg))] <- c("logFC", "P.Val", "adj.P.Val")
  }
  return(deg)
}
# function----
getDEGsList <- function(gseacc_list, logFC = 1, p = "a", is.matrix = FALSE, select = c("UP", "DOWN")) {
  DEGsList <- list()
  fileNameList <- c(paste(gseacc_list, 1, sep = "_"))
  DEGsList <- list()
  for (i in 1:length(fileNameList)) { # i <- 1
    fileName <- fileNameList[i]
    # logFCList <- rep(logFC,length(fileNameList))
    # logFC <- logFCList[i]
    dirName <- str_extract(fileName, pattern = ".*?(?=_)")
    tryCatch(
      {
        load(paste("~/Rstudio", dirName, paste0(fileNameList[i], "_deg.Rdata"), sep = "/"))
      },
      error = function(e) {
        print(paste(dirName, "is not existed! "))
        break
      }
    )
    ifelse("p" == p, pvalue <- "P.Val", pvalue <- "adj.P.Val")
    if (T %in% str_detect("P.Value", string = colnames(deg))) {
      colnames(deg)[grep("P.Value", x = colnames(deg))] <- "P.Val"
    }
    deg$g <- ifelse(deg[, pvalue] > 0.05, "stable",
      ifelse(deg$logFC > logFC, "UP",
        ifelse(deg$logFC < -logFC, "DOWN", "stable")
      )
    )
    ifelse(is.matrix == F,
      DEGsList[[i]] <- rownames(deg[deg$g %in% select, ]),
      DEGsList[[i]] <- deg
    )
  }

  names(DEGsList) <- gseacc_list
  return(DEGsList)
}
# Intersects----
Intersects <- function(...) {
  Reduce(intersect, ...)
}
# getDEGs
getGSEExprSet <- function(gseacc) {
  load(paste(rootDir, gseacc,
    paste(gseacc, "-output.Rdata", sep = ""),
    sep = "/"
  ))
}
#
sort_chr <- function(x) {
  x <- as.character(x)
  dat_x <- data.frame(
    x = x, x0 = x
  )
  dat_x$x0 <- str_extract(x, "[0-9]+") %>% as.numeric()
  dat_x <- dat_x[order(dat_x$x0, method = c("auto")), ]
  x1 <- factor(x, levels = dat_x$x) # levels定死了
  x1 <- sort(x1)
  x1 <- as.character(x1)
  return(x1)
}
get_all_choice_matrix <- function(gset, sam_data_matrix) {
  library(dplyr)
  library(stringr)
  library(purrr)
  # 获取所有选项
  source_name <- grep(
    x = names(sam_data_matrix),
    pattern = "source_name", fixed = T, value = T
  )
  title_name <- grep(names(sam_data_matrix),
    pattern = "title", value = T, fixed = T
  )
  sam_char <- grep(names(gset[[1]]@phenoData@data),
    pattern = ":", value = T, fixed = T
  )
  all_choice <- c(source_name, title_name, sam_char)
  # 获取所有选项数据
  all_choice_matrix_data <- map(
    all_choice, ~ {
      unique(sam_data_matrix[, .x])
    }
  ) %>%
    setNames(all_choice) %>%
    .[which(lengths(.) > 1)]
  all_choice <- names(all_choice_matrix_data)
  # 获取选项数据框
  all_choice_matrix <- do.call(cbind, lapply(
    lapply(all_choice_matrix_data, unlist),
    `length<-`, max(lengths(all_choice_matrix_data))
  )) %>% as.data.frame()
  return(all_choice_matrix)
}
