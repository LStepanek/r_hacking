###############################################################################
###############################################################################
###############################################################################

## instaluji a loaduji balíčky ------------------------------------------------

invisible(
    lapply(
        c(
            "openxlsx",
            "data.table"
        ),
        function(package){
            
            if(!(package %in% rownames(installed.packages()))){
                
                install.packages(
                    package,
                    dependencies = TRUE,
                    repos = "http://cran.us.r-project.org"
                )
                
            }
            
            library(package, character.only = TRUE)
            
        }
    )
)


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji handling se zipováním v R ----------------------------------------

Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip") 


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji pracovní složku --------------------------------------------------

while(!"__script__.R" %in% dir()){
    setwd(choose.dir())
}

mother_working_directory <- getwd()


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím posložky pracovní složky ------------------------------------------

setwd(mother_working_directory)

for(my_subdirectory in c("vstupy", "vystupy")){
    
    if(!file.exists(my_subdirectory)){
        
        dir.create(file.path(
            
            mother_working_directory, my_subdirectory
            
        ))
        
    }
    
}


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím velké množství dummy datasetů -------------------------------------

setwd(
    paste(mother_working_directory, "vstupy", sep = "/")
)

for(i in 1:100){
    
    set.seed(i)
    
    write.table(
        x = format(
            round(
                data.frame(
                    "a" = rnorm(n = 500, mean = 0, sd = 1),
                    "b" = rnorm(n = 500, mean = 100, sd = 1),
                    "c" = rnorm(n = 500, mean = 100, sd = 10)
                ),
                digits = 4
            ),
            nsmall = 4
        ),
        file = paste(
            "dataset",
            "_",
            paste(
                rep("0", 3 - nchar(as.character(i))),
                collapse = ""
            ),
            i,
            ".csv",
            sep = ""
        ),
        sep = ";",
        row.names = FALSE
    )
    
}

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## loaduji data ---------------------------------------------------------------

setwd(
    paste(mother_working_directory, "vstupy", sep = "/")
)

for(my_filename in dir()){
    
    assign(
        gsub(
            ".csv",
            "",
            my_filename
        ),
        read.table(
            file = my_filename,
            header = TRUE,
            sep = ";",
            stringsAsFactors = FALSE,
            encoding = "UTF-8"
        )
    )
    
}

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





