################################################################
# R Investment Case Study done by SATYA, SURAJ, JEET and ASHISH
################################################################

# Calling the package tidyr and dplyr so we can use the functions available 
# in these packages

library(tidyr)
library(dplyr)

# Import the companies data set in Rstudio in a variable named companies
# companies.txt is the dataset file placed in the Rstudio working directory
companies <- read.delim("companies.txt",sep ="\t",quote = "\"")

# viewing the companies variable which has the dataset from the file companies.txt
View(companies)

#	When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of the data frame by using str()
str(companies)

# Converting the case of column "permalink" to lower case for data cleaning 
# and merging of companies dataframe with rounds2 dataframe
companies$permalink <- tolower(companies$permalink)

# Import the rounds2.csv data set in Rstudio in a variable named rounds2
# rounds2.csv is the dataset file placed in the Rstudio working directory
rounds2 <- read.csv("rounds2.csv")

# viewing the companies variable which has the dataset from the file companies.txt
View(rounds2)

#	When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of the data frame by using str()  
str(rounds2)

# Converting the case of column "company_permalink" to lower case for data cleaning 
# and merging of companies dataframe with rounds2 dataframe
rounds2$company_permalink <- tolower(rounds2$company_permalink )

################################################################################
#  Checkpoint 1: Data Cleaning 1
################################################################################
  
#1. How many unique companies are present in rounds2?
## This is done by taking length (count) of unique number of records of "company_permalink"
length(unique(rounds2$company_permalink)) 
# unique companies are :- 66368

#2. How many unique companies are present in the companies file? 
## This is done by taking length (count) of unique number of records of "permalink"
length(unique(companies$permalink)) 
# unique companies are :- 66368

#4. Are there any companies in the rounds2 file which are not present in companies ? 
#Answer Y/N.
## N we can find it by using setdiff on unique id column  and this shows 0 difference

setdiff(rounds2$company_permalink,companies$permalink)

#5. Merge the two data frames so that all variables (columns)in the companies 
#   frame are added to the rounds2 data frame.Name the merged frame master_frame.
#   How many observations are present in master_frame ?

## merging the two data frame comapnies and rounds2 into master dataframe below
master_frame <- merge(rounds2,companies, by.x="company_permalink", by.y = "permalink")

## to find the number of observation we can use length of unique column "company_permalink"
length(master_frame$company_permalink)
## 114949 is the number of observations.

setdiff(rounds2$company_permalink,companies$permalink)


##################################################################################
##Checkpoint 2: Funding Type Analysis
##This is the first of the three goals of data analysis - investment type analysis
##################################################################################
 
## To get the average funding amount of funding round type we have grouped the
## Master_fram by funding_round_type and thenpicked up answers for questions#1 
## to #4 and then take the average of raised_amount_usd for each group with help
## of summarize function as below.


fundingtype_groups <- group_by(master_frame, funding_round_type )

funding_avg <- summarise(fundingtype_groups, avg_funding = mean(raised_amount_usd, na.rm = TRUE))
funding_avg


#1.Average funding amount of venture type
## from the funding_avg data frame we can find answers below
funding_avg[((funding_avg$funding_round_type == "venture")),]

#2.Average funding amount of angel type
## from the funding_avg data frame we can find answers below
funding_avg[((funding_avg$funding_round_type == "angel")),]

#3.Average funding amount of seed type
## from the funding_avg data frame we can find answers below
funding_avg[((funding_avg$funding_round_type == "seed")),]

#4.Average funding amount of private equity type
## from the funding_avg data frame we can find answers below
funding_avg[((funding_avg$funding_round_type == "private_equity")),]

#5.Considering that Spark Funds wants to invest between 5 to 15 million USD per
#  investment round, which investment type is the most suitable for them?

## we will be using the condition of 5 to 15 million on avg_funding column to
## get the suitable invenstment type.
funding_avg[((funding_avg$avg_funding >= 5000000 & funding_avg$avg_funding <= 15000000)),]
## Venture which falls in 5 to 15 million range is the suitable investment type.

################################################################################
#Checkpoint 3: Country Analysis
################################################################################

## Approch is to first create one subset dataframe from master_frame for the investment
## type "venture" excluding country code blank or NA and then group_by the subset
## master_Frame_country df by country_code and take sum of "raised_amount_usd" for the groupby country df 
master_Frame_country <- filter(master_frame, funding_round_type=="venture",country_code !="",country_code != "NA")
mastercountry_groups <- group_by(master_Frame_country,country_code)
mastercountry_sum <- summarise(mastercountry_groups, mastercountry_sum= sum(raised_amount_usd, na.rm = TRUE))

## once we have the df having sum of investment by country we can sort it in descending 
## order to answer the questions. 

mastercountry_sum  <- arrange(mastercountry_sum, desc(mastercountry_sum))

top9 <- mastercountry_sum[1:9,]
## Printing the top 9 country
top9 

## once we have the top 9 country we can find the top3 english speaking country 
## from the  list of countries where English is an official language.
## the top 3 countries are USA, GBR and IND.


################################################################################
#Checkpoint 4: Sector Analysis 1
## This is the third goal of analysis - sector analysis.
################################################################################

## for some companies, the category list is a list of multiple sub-sectors 
## separated by a pipe (vertical bar |). For example, one of the companies' 
## category_list is Application Platforms|Real Time|Social Network Media.
## the first string before the vertical bar will be considered the primary sector
## In the example above, 'Application Platforms' will be considered the primary sector.
## we will be using separate function for this requirements.
category_list <-separate(master_frame,category_list, into = c("Primary sector"),sep = "\\|",extra = "drop",remove = FALSE)

# Import the mapping.csv data set in Rstudio in a variable named mapping
# mapping.csv is the dataset file placed in the Rstudio working directory
mapping <- read.csv(("mapping.csv"))

## converting mapping from wide to long for ease of processing
mapping_long <- gather(mapping,main_sector,my_val,2:10)

## removing the  "Blank" main_sector and "0" values since these are not required as 
## per business and assignment.

mapping_long <- mapping_long[(!(mapping_long$my_val == 0)& !(mapping_long$main_sector =="Blanks")),]

## merging the category_listdf with mapping long df to make master_main_sect df
## Merged data frame master_main_sect has each primary sector mapped to its main sector
## (the primary sector should be present in a separate column).
master_main_sect <- merge(category_list ,mapping_long,by.x="Primary sector", by.y = "category_list" )


################################################################################
# Checkpoint 5: Sector Analysis 2
################################################################################

## creating the three DF D1,D2,D3 as requested in checkpoint 5 with needed filter condition
## on funding_round_type, funding_round_type and raised_amount_usd
## Create three separate data frames D1, D2 and D3 for each of the three countries containing the 
## observations of funding type FT falling within the 5-15 million USD range. 
## The three data frames should contain:
## All the columns of the master_frame along with the primary sector and the main sector
## The total number (or count) of investments for each main sector in a separate column
## The total amount invested in each main sector in a separate column


D1 <- filter(master_main_sect,funding_round_type=="venture",funding_round_type =="USA", raised_amount_usd >= 5000000 , raised_amount_usd <= 15000000)
D2 <- filter(master_main_sect,funding_round_type=="venture",country_code =="GBR",raised_amount_usd >= 5000000 , raised_amount_usd <= 15000000)
D3 <- filter(master_main_sect,funding_round_type=="venture",country_code =="IND", raised_amount_usd >= 5000000 , raised_amount_usd <= 15000000)

## grouping by D1,D2 and D3 based on main_sector and then calculating the count of
## investment by main sector and sum of amount invested and merging it in main df D1,D2,D3
D1_groupby <- group_by(D1, main_sector)
D1_count_sum<- summarise(D1_groupby, count_invest= n(), total_amount_invest = sum(raised_amount_usd, na.rm = TRUE))
D1 <- merge(D1,D1_count_sum, by ="main_sector")


D2_groupby <- group_by(D2, main_sector)
D2_count_sum<- summarise(D2_groupby, count_invest= n(), total_amount_invest = sum(raised_amount_usd, na.rm = TRUE))
D2 <- merge(D2,D2_count_sum, by ="main_sector")

D3_groupby <- group_by(D3, main_sector)
D3_count_sum<- summarise(D3_groupby, count_invest= n(), total_amount_invest = sum(raised_amount_usd, na.rm = TRUE))
D3 <- merge(D3,D3_count_sum, by ="main_sector")

#1. Total number of Investments (count) 
## this can be found by summing of count_invest on D1,D2,D3 df as done below:-

sum(D1_count_sum$count_invest)

sum(D2_count_sum$count_invest)

sum(D3_count_sum$count_invest)

#2 Total amount of investment (USD)
## this can be done by summing total amount invested on D1_count_sum df for D1,D2,D3

sum(D1_count_sum$total_amount_invest)

sum(D2_count_sum$total_amount_invest)

sum(D3_count_sum$total_amount_invest)

# questions 3,4,5, First,second,third Sector name (no. of investment-wise)
# and question 6,7,8 Number of investments in first, second and third sector
## This can be done by sorting group by DF D1_count_sum/D2_count_sum/D3_count_sum
## in descending order and taking details for respective positions.

arrange(D1_count_sum, desc(count_invest))

arrange(D2_count_sum, desc(count_invest))

arrange(D3_count_sum, desc(count_invest))

#9 For point 3 (top sector count-wise), which company received the 
#  highest investment?
## for point 3 top sector count wise is "others" for this we will create a subset df
## having data for only main sector "Others" and the group by that based on company name
## and then take the sum of raised_amount_usd for the group by df.
top_company_D1 <-filter(D1,main_sector == "Others" )
top_company_D1_grpby <- group_by(top_company_D1, name)
top_company_D1 <- summarise(top_company_D1_grpby, top_company_invest = sum(raised_amount_usd))

## once we have the df having sum of raised_amount_usd per company we can sort it
## and take the first record
top_company_D1 <-arrange(top_company_D1, desc(top_company_invest))

# Printing the company received the highest investment for "Others" for D1
top_company_D1[1,1]

top_company_D2 <-filter(D2,main_sector == "Others" )
top_company_D2_grpby <- group_by(top_company_D2, name)
top_company_D2 <- summarise(top_company_D2_grpby, top_company_invest = sum(raised_amount_usd))
top_company_D2 <-arrange(top_company_D2, desc(top_company_invest))

# Printing the company received the highest investment for Others for D2
top_company_D2[1,1]

top_company_D3 <-filter(D3,main_sector == "Others" )
top_company_D3_grpby <- group_by(top_company_D3, name)
top_company_D3 <- summarise(top_company_D3_grpby, top_company_invest = sum(raised_amount_usd))
top_company_D3 <-arrange(top_company_D3, desc(top_company_invest))

# Printing the company received the highest investment for Others for D3
top_company_D3[1,1]


#10 For point 4 (second best sector count-wise), which company received the highest investment?
## for point 4 second best sector count wise is "Cleantech...Semiconductors" for this we will create a subset df
## having data for only main sector "Cleantech...Semiconductors" and the group by that based on company name
## and then take the sum of raised_amount_usd for the group by df.

top_2nd_company_D1 <-filter(D1,main_sector == "Cleantech...Semiconductors" )
top_2nd_company_D1_grpby <- group_by(top_2nd_company_D1, name)
top_2nd_company_D1 <- summarise(top_2nd_company_D1_grpby, top_company_invest = sum(raised_amount_usd))
top_2nd_company_D1 <-arrange(top_2nd_company_D1, desc(top_company_invest))

# Printing the company received the highest investment for Cleantech...Semiconductors ?
top_2nd_company_D1[1,1]

top_2nd_company_D2 <-filter(D2,main_sector == "Cleantech...Semiconductors" )
top_2nd_company_D2_grpby <- group_by(top_2nd_company_D2, name)
top_2nd_company_D2 <- summarise(top_2nd_company_D2_grpby, top_company_invest = sum(raised_amount_usd))
top_2nd_company_D2 <-arrange(top_2nd_company_D2, desc(top_company_invest))

# Printing the company received the highest investment for Cleantech...Semiconductors ?
top_2nd_company_D2[1,1]

top_2nd_company_D3 <-filter(D3,main_sector == "News..Search.and.Messaging" )
top_2nd_company_D3_grpby <- group_by(top_2nd_company_D3, name)
top_2nd_company_D3 <- summarise(top_2nd_company_D3_grpby, top_company_invest = sum(raised_amount_usd))
top_2nd_company_D3 <-arrange(top_2nd_company_D3, desc(top_company_invest))

# Printing the company received the highest investment for News..Search.and.Messaging ?
top_2nd_company_D3[1,1]

