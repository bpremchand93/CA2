#Reading the NIpostcodes csv file from CA_2 Project workspace and header is set to false 
#since the csv file has no header, and stringasfactor is set to false to avoid the 
#default behaviour of R for converting char to factor 

#Section 1:

postcode_dataframe <- read.csv('NIPostcodes.csv', header = FALSE, stringsAsFactors = FALSE)

#Creating column name for each dataframe columns
column_names <- c("Organization Name", "Sub-building Name", "Building Name", "Number", "Primary Thorfar", "Alt Thorfare", "Secondary Thorfare",
                  "Locality", "Townland", "Town", "County", "Postcode", "x-coordinates", "y-coordinates", "Primary Key")


#Adding Column names in the postcode dataset dataframe
colnames(postcode_dataframe) <- column_names

#Primary key column has kept in the first column
postcode_dataframe <- subset(postcode_dataframe, select = c(15,1:14))


#Categorising the county variable from character to factor
postcode_dataframe$County <- as.factor(postcode_dataframe$County)


#Adding 'NA' in the spaces
attach(postcode_dataframe)
postcode_dataframe[postcode_dataframe ==""] <- NA

#Postcode_dataframe has been stored in the csv file
write.csv(postcode_dataframe, file = "CleanNIPostcodeData.csv", row.names = FALSE)

#Showing the structure of postcode dataframe
str(postcode_dataframe)

#Showing the total number of rows in postcode dataset
cat("Total Number of rows in the postcode dataframe is:", nrow(postcode_dataframe))

#Display the first 10 rows of dataframe
head(postcode_dataframe, n = 10)

#Showing the summary of postcode dataframe
summary(postcode_dataframe)

#Sum and mean of missing values in postcode datasets columns
cat("Sum of missing values for each column in the postcode dataframe :", colSums(is.na(postcode_dataframe)), sep = "\t")
cat("Mean of missing values for each column in the postcode dataframe :", colMeans(is.na(postcode_dataframe)), sep = "\t")

#Idententify the locality, townland and town containing the name Limavady and store in a csv file
library(sqldf)
Limavady_data <- sqldf('SELECT * FROM postcode_dataframe WHERE Town like "%LIMAVADY%"  OR Locality like "%LIMAVADY%" OR Townland like "%LIMAVADY%"')
write.csv(Limavady_data, file = "Limavady.csv", row.names = FALSE )

#Section 2:
#Adding Crime datasets from multiple subdirectories to a single dataframe
root <- 'NI Crime Data/NI Crime Data'
dir(root, pattern='csv$', recursive = TRUE, full.names = TRUE) %>%
  # read files into data frames
  lapply(FUN = read.csv) %>%
  # bind all data frames into a single data frame
  rbind_all %>%
  # write into a single csv file
  write.csv("all.csv",row.names = F)

#Read all the crime dataset and  stringasfactor is set to false to avoid the 
#default behaviour of R for converting char to factor 
AllNICrimeData <- read.csv("all.csv", stringsAsFactors = FALSE)
attach(AllNICrimeData)

#Showing the count of crime dataset dataframe
cat("Count of the Crime dataframe :",nrow(AllNICrimeData))

#Modified the structure of AllNICrimeData dataframe and factorised the crime type
AllNICrimeData <- AllNICrimeData[, !(names(AllNICrimeData) %in% c("Crime.ID", "Reported.by", "Falls.within", "LSOA.code", "LSOA.name", "Last.outcome.category", "Context"))] 
AllNICrimeData$Crime.type <- as.factor(AllNICrimeData$Crime.type)

#Shown the top 10 records and structure of the crime dataset
head(AllNICrimeData , 10)
str(AllNICrimeData)

#Removing 'On or near' column from a location column and once it is modified added 'NA' in a empty values
AllNICrimeData$Location <- gsub("On or near", "", AllNICrimeData$Location)
AllNICrimeData$Location[AllNICrimeData$Location==" "] <- NA
AllNICrimeData <- na.omit(AllNICrimeData)

#Random sample created from the crime dataset which consist of 1000 random records and location attribute should 
#contains the 'NA'.
random_crime_sample <- AllNICrimeData[sample(1:nrow(AllNICrimeData), 1000, replace = FALSE),]
random_crime_sample$Location <- toupper(random_crime_sample$Location)
random_crime_sample$Location <- trimws(random_crime_sample$Location, which = "both")


library(dplyr)
library(sqldf)
postcode_dataframe_unique <- tbl_df(postcode_dataframe)
postcode_dataframe_unique <- subset(postcode_dataframe_unique,!is.na(postcode_dataframe_unique$Postcode))
postcode_dataframe_unique <- subset(postcode_dataframe_unique,!is.na(postcode_dataframe_unique$`Primary Thorfar`))
postcode_dataframe_unique <- sqldf(" SELECT *, COUNT(DISTINCT`Primary Thorfar`) as count, COUNT(Postcode) AS count_1
                                   FROM postcode_dataframe_unique GROUP BY `Primary Thorfar`
                                   HAVING count !=0
                                   ORDER BY COUNT(Postcode) DESC")

postcode_dataframe_unique <- select(postcode_dataframe_unique, `Primary Thorfar`, Postcode)
postcode_dataframe_unique <- na.omit(postcode_dataframe_unique)
names(postcode_dataframe_unique)[1] <- "Location"

#Function created to get the postcode for the crime location from the postcode dataframe that is created earlier.
find_a_postcode <- function(loc){
  random_crime_sample <- as.data.frame(fn$sqldf('SELECT a.*, b.Postcode FROM random_crime_sample a LEFT JOIN postcode_dataframe_unique b where a.Location = b."$loc"'))  
  return(random_crime_sample)
}

random_crime_sample <- find_a_postcode("Location")

write.csv(random_crime_sample, "random_crime_sample.csv", row.names = F)


