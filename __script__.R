###############################################################################
###############################################################################
###############################################################################

## instaluji a loaduji balíčky ------------------------------------------------

invisible(
    lapply(
        c(
            "openxlsx",
            "sqldf",
            "data.table",
            "parallel",
            "Rcpp"
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
            "\\.csv$",
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

## zkusme rychlý webscraping a úpravu dat -------------------------------------

#### stahuji statické HTML ----------------------------------------------------

my_html <- readLines(
    
    con = "http://dychanky.vse.cz/",
    encoding = "UTF-8"
    
)


#### nahrazuji pevnou mezeru svou entitou -------------------------------------

my_text <- gsub("&nbsp;", "M_E_Z_E_R_A", my_html)

#### extrahujuji pryč všechny HTML tagy a entity ------------------------------

my_text <- gsub("<.*?>", "", my_text)
my_text <- gsub("&.*?;", "", my_text)

my_text <- gsub("M_E_Z_E_R_A", " ", my_text)    # nahrazuji nezlomitelnou
                                                # zlomitelnou
                                                

#### odstraňuji white strapes -------------------------------------------------

for(i in 1:length(my_text)){
    
    while(grepl("  ", my_text[i])){
        
        my_text[i] <- gsub("  ", " ", my_text[i])
        
    }
    
}


#### ponechávám jen řádky obsahující text -------------------------------------

my_text <- my_text[!my_text %in% c("", " ")]


#### extrahuji emaily ---------------------------------------------------------

gsub(
    "(.*?)([a-zA-Z\\.]+@[a-zA-Z]+\\.[a-zA-Z]+)(.*)",
    "\\2",
    my_text[grepl("@", my_text)]
)


#### zbavuji text diakritiky --------------------------------------------------

my_text <- iconv(my_text, from = "UTF-8", to = "ASCII//TRANSLIT")


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## chceme zvýšit hodnoty i-tého datasetu i-krát a o i^2 -----------------------

for(i in 1:100){
    
    my_filename <- paste(
        "dataset",
        "_",
        paste(
            rep("0", 3 - nchar(as.character(i))),
            collapse = ""
        ),
        i,
        sep = ""
    )
    
    assign(
        my_filename,
        get(my_filename) * i + i ^ 2
    )
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## zkouším automatickou regresi nad datasetem "mtcars" ------------------------

sink("leave_one_out_regrese.txt")

for(my_variable in setdiff(colnames(mtcars), "mpg")){
    
    my_formula <- paste(
        
        "mpg",
        " ~ ",
        paste(
            setdiff(colnames(mtcars), c("mpg", my_variable)),
            collapse = " + "
        ),
        sep = ""
        
    )
    
    eval(
        parse(
            text = paste(
                "my_lm <- lm(",
                my_formula,
                ", ",
                "data = mtcars",
                ")",
                sep = ""            
            )
        )
    )
    
    print("################################################")
    print(summary(my_lm))
    
}

sink()


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## exportuji a importuji data do a z MS Excel ---------------------------------

## nastavuji handling se zipováním v R ----------------------------------------

#### !!! nutné předem nainstalovat Rtools z adresy
#### https://cran.r-project.org/bin/windows/Rtools/ ---------------------------

Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip")


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím sešit -------------------------------------------------------------

my_workbook <- createWorkbook()


## tohle jsou moje data, je to věstavěný dataset mtcars -----------------------

my_table <- mtcars


## vytvářím excelový list -----------------------------------------------------
    
addWorksheet(
    wb = my_workbook,
    sheetName = "můj první list"
)


## ukládám do listu data ------------------------------------------------------

writeData(
    wb = my_workbook,
    sheet = "můj první list",
    rowNames = FALSE,
    colNames = TRUE,
    x = my_table
)


## nastavuji automatickou šířku sloupce ---------------------------------------
 
setColWidths(
    wb = my_workbook,
    sheet = "můj první list",
    cols = 1:dim(my_table)[2],
    widths = "auto"
)


## vytvářím dva své styly - jednak tučné písmo, jednak písmo zarovnané
## doprava v rámci buňky ------------------------------------------------------

my_bold_style <- createStyle(textDecoration = "bold")
right_halign_cells <- createStyle(halign = "right")

addStyle(
    wb = my_workbook,
    sheet = "můj první list",
    style = my_bold_style,
    rows = 1,
    cols = 1:dim(my_table)[2],
    gridExpand = TRUE
)

addStyle(
    wb = my_workbook,
    sheet = "můj první list",
    style = right_halign_cells,
    rows = 1:(dim(my_table)[1]),
    cols = 2:(dim(my_table)[2] + 1),
    gridExpand = TRUE
)


## ukládám workbook -----------------------------------------------------------

setwd(
    paste(mother_working_directory, "vystupy", sep = "/")
)

saveWorkbook(
    wb = my_workbook,
    file = "moje_tabulka_je_ted_v_excelu.xlsx",
    overwrite = TRUE
)

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## nahrávám zpětně daný workbook ----------------------------------------------

setwd(
    paste(mother_working_directory, "vystupy", sep = "/")
)

my_data <- read.xlsx(
    xlsxFile = "moje_tabulka_je_ted_v_excelu.xlsx",
    sheet = 1,   # anebo jméno listu
    colNames = TRUE
)

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## dotazování SQL v R ---------------------------------------------------------

sqldf("
    SELECT *
    FROM mtcars
")

sqldf("
    SELECT cyl,
           round(avg(mpg), 2) AS 'mean_mpg'
    FROM mtcars
    GROUP BY cyl
    ORDER BY cyl
")


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## iterování nad funkcemi -----------------------------------------------------

set.seed(1)
x <- floor(runif(100) * 100)

for(my_function in c(
    "mean",
    "min",
    "max",
    "median",
    "sd",
    "var",
    function(i) i %% 10
)){
    print(do.call(my_function, list(x)))
}

    
## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## adresaci dolarem od teď již nikdy ------------------------------------------

for(my_variable in colnames(mtcars)){
    print(mean(mtcars$my_variable))
}

for(my_variable in colnames(mtcars)){
    print(mean(mtcars[, my_variable]))
}

for(my_variable in colnames(mtcars)){
    print(eval(parse(text = paste(
        "mean(mtcars$",
        my_variable,
        ")",
        sep = ""
    ))))
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## vytvořme vektor třetích mocnin čísel 1 až 1000 -----------------------------

#### I am a looper!

my_start <- Sys.time()

x <- NULL

for(i in 1:1000){
    x <- c(x, i ^ 3)
}

my_stop <- Sys.time()

my_stop - my_start

    
#### I am somewhere in the middle

my_start <- Sys.time()

x <- rep(0, 1000)

for(i in 1:1000){
    x[i] <- i ^ 3
}
    
my_stop <- Sys.time()

my_stop - my_start
    

#### I vectorise, anytime, anyhow!

my_start <- Sys.time()

x <- c(1:1000) ^ 3

my_stop <- Sys.time()

my_stop - my_start


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## najděme velikost integrálu \int\limits_{0}^{1} x^2 \textsf{d}x

#### I am a looper!

n_of_hits <- 0

for(i in 1:100000){
    if(runif(1) < runif(1) ^ 2){
        n_of_hits <- n_of_hits + 1
    }
}
n_of_hits / 100000



#### I vectorise, anytime, anyhow!

mean(runif(100000) <= runif(100000) ^ 2)


## ----------------------------------------------------------------------------

###############################################################################

## apply() --------------------------------------------------------------------

my_start <- Sys.time()

x <- apply(mtcars, 2, mean)

my_stop <- Sys.time()

my_stop - my_start


my_start <- Sys.time()

x <- NULL
for(i in 1:dim(mtcars)[2]){
    x <- c(x, mean(mtcars[, i]))
    names(x)[length(x)] <- colnames(mtcars)[i]
}

my_stop <- Sys.time()

my_stop - my_start


## ----------------------------------------------------------------------------

###############################################################################

## lapply() -------------------------------------------------------------------


## lapply() funkce, vhodná především pro vektory či listy ---------------------

getMyFifthPower <- function(x){

    # '''
    # vrací pátou mocninu čísla "x"
    # '''
    
    x ^ 5
    
}


## testuji lapply() vs. for cyklus nad stejnou operací ------------------------

my_n <- 10000000


## nejdřív lapply() -----------------------------------------------------------

beginning_lapply <- proc.time()

output_lapply <- lapply(list(1:my_n), getMyFifthPower)

end_lapply <- proc.time()


## nyní for() cyklus ----------------------------------------------------------

beginning_for <- proc.time()

output_for <- rep(0, my_n)

for(i in 1:my_n){
    output_for[i] <- getMyFifthPower(i)
}

end_for <- proc.time()


## porovnání obou časů --------------------------------------------------------

end_lapply - beginning_lapply
end_for - beginning_for
     ## lapply() je značně rychlejší než for()


## ----------------------------------------------------------------------------
## ----------------------------------------------------------------------------
## ----------------------------------------------------------------------------

my_start <- Sys.time()

for_x <- NULL
for(i in 1:100000){
    for_x <- c(for_x, i ^ 5)
}

my_stop <- Sys.time()

my_stop - my_start


my_start <- Sys.time()

lapply_x <- unlist(
    lapply(
        1:100000,
        function(i) i ^ 5
    )
)

my_stop <- Sys.time()

my_stop - my_start


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## paralelní výpočty ----------------------------------------------------------

fibonacci <- function(n){
    if(n == 1){return(1)}
    if(n == 2){return(1)}
    if(n >= 3){return(fibonacci(n - 1) + fibonacci(n - 2))}
}

detectCores()
cl <- makeCluster(1)
clusterCall(cl, fibonacci)

my_start <- Sys.time()
invisible(parLapply(cl, 1:30, fibonacci))
stopCluster(cl)

my_stop <- Sys.time(); my_stop - my_start

my_start <- Sys.time()

invisible(lapply(1:30, fibonacci))

my_stop <- Sys.time(); my_stop - my_start


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## C++ v R --------------------------------------------------------------------

mean_r <- function(x){
    my_mean <- 0
    for(i in 1:length(x)){
        my_mean= my_mean + x[i] / length(x)
    }
    return(my_mean)
}

set.seed(1)
mean_r(rnorm(1000000))


cppFunction("
  double mean_cpp(NumericVector x) {
    int i;
    int n = x.size();
    double mean = 0;

    for(i = 0; i < n; i++) {
      mean = mean + x[i] / n;
    }
    return mean;
  }
")

set.seed(1)
mean_cpp(rnorm(1000000))


## ----------------------------------------------------------------------------

my_start <- Sys.time()

set.seed(1)
mean(rnorm(1000000))

my_stop <- Sys.time(); my_stop - my_start


my_start <- Sys.time()

set.seed(1)
mean_cpp(rnorm(1000000))

my_stop <- Sys.time(); my_stop - my_start



my_start <- Sys.time()

set.seed(1)
mean_r(rnorm(1000000))

my_stop <- Sys.time(); my_stop - my_start


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





