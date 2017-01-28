# Codigos_Uteis_R

#Repositório com o objetivo de adicionar códigos úteis para implementação em R

# Fontes com funções básicas em R:

#R_importfiles:
https://www.datacamp.com/community/tutorials/r-data-import-tutorial#gs.BMsfZtE

#R_functions:
https://www.datacamp.com/community/tutorials/functions-in-r-a-tutorial#gs.5nPxaSM

#R_xlsx:
https://www.r-bloggers.com/importexport-data-to-and-from-xlsx-files/

#R_dates and time:
http://www.statmethods.net/input/dates.html
https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html

#R_conversions:
http://www.statmethods.net/management/typeconversion.html

#RODBC: 
https://blogs.oracle.com/R/entry/r_to_oracle_database_connectivity

#ftp
require(RCurl) 
myFile <- getURL("url", ssl.verifypeer = FALSE)
myText <- read.table(textConnection(myFile), header = FALSE)

#filter
subset(dataframe, column1 < criteria1 & column2 > criteria2, columnsToReturn)
