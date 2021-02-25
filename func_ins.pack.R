ins.pack <- function(...) {
        # options
        options(install.packages.compile.from.source = "always")
        # list of packages
        list.packages <- c(...)
        # check if the packages are installed
        new.packages <- list.packages[!(list.packages %in% installed.packages()[ , "Package"])]
        # install new packages
        if(length(new.packages)) install.packages(new.packages, type = "source")
        # load the packages
        lapply(list.packages, require, character.only = TRUE)
        # remove lists
        rm(list.packages, new.packages)    
        }