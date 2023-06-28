rm(list = ls())

install.packages("dplyr")
library(dplyr)

df_pres2012 <- read.csv("/Users/justinsaunders/Downloads/georgia 2012 pres-by-cd.csv", header = TRUE, sep = ",")
df_pres2012$year <- recode_factor(df_pres2012$year, "2,012" = "2012")
df_pres2012$year <- as.character(df_pres2012$year)
df_pres2012$candidatevotes <- as.numeric(gsub(",", "", df_pres2012$candidatevotes))
df_pres2012$totalvotes <- as.numeric(gsub(",", "", df_pres2012$totalvotes))
df_pres2012$district <- as.character(df_pres2012$district)

df <- read.csv("/Users/justinsaunders/Downloads/1976-2020-house.csv", header = TRUE, sep = ",") %>%
  filter(party=="DEMOCRAT" | party=="REPUBLICAN" | party=="LIBERTARIAN" | party=="INDEPENDENT" | party=="GREEN") %>%
  filter(year=="2008" | year=="2010" | year=="2012" | year=="2014" | year=="2016" | year=="2018" | year=="2020") %>%
  filter(state=="GEORGIA")
df <- df[ , c("state","district","candidate","party","candidatevotes","year","totalvotes","office")]
df$candidatevotes <- as.numeric(df$candidatevotes)
df$totalvotes <- as.numeric(df$totalvotes)
df$district <- as.character(df$district)
df$year <- as.character(df$year)
library(dplyr)
electionGA <- bind_rows(df_pres2012, df)



df_pres2016 <- read.csv("/Users/justinsaunders/Downloads/Copy of georgia 2016 pres-by-cd.xlsx - Summary.csv", header = TRUE)
df_pres2016$year <- as.character(df_pres2016$year)
df_pres2016$candidatevotes <- as.numeric(gsub(",", "", df_pres2016$candidatevotes))
df_pres2016$totalvotes <- as.numeric(gsub(",", "", df_pres2016$totalvotes))
df_pres2016$district <- as.character(df_pres2016$district)
library(dplyr)
electionGA <- bind_rows(df_pres2016, electionGA)



df_gub2018 <-read.csv("/Users/justinsaunders/Downloads/Copy of Georgia 2018 gov-by-cd - Summary.csv", header = TRUE)
df_gub2018$year <- recode_factor(df_gub2018$year, "2,018" = "2018")
df_gub2018$year <- as.character(df_gub2018$year)
df_gub2018$candidatevotes <- as.numeric(gsub(",", "", df_gub2018$candidatevotes))
df_gub2018$totalvotes <- as.numeric(gsub(",", "", df_gub2018$totalvotes))
df_gub2018$district <- as.character(df_gub2018$district)
library(dplyr)
electionGA <- bind_rows(df_gub2018, electionGA)


df_pres2020 <-read.csv("/Users/justinsaunders/Downloads/Copy of georgia 2020 pres-by-cd - Summary.csv", header = TRUE)
df_pres2020$year <- as.character(df_pres2020$year)
df_pres2020$candidatevotes <- as.numeric(gsub(",", "", df_pres2020$candidatevotes))
df_pres2020$totalvotes <- as.numeric(gsub(",", "", df_pres2020$totalvotes))
df_pres2020$district <- as.character(df_pres2020$district)
library(dplyr)
electionGA <- bind_rows(df_pres2020, electionGA)

df_sen2020 <- read.csv("/Users/justinsaunders/Downloads/Copy of georgia 2020 Sen-A-by-cd - Summary.csv", header = TRUE)
df_sen2020$year <- as.character(df_sen2020$year)
df_sen2020$candidatevotes <- as.numeric(gsub(",", "", df_sen2020$candidatevotes))
df_sen2020$totalvotes <- as.numeric(gsub(",", "", df_sen2020$totalvotes))
df_sen2020$district <- as.character(df_sen2020$district)
library(dplyr)
electionGA <- bind_rows(df_sen2020, electionGA)


electionGA$party <- as.factor(electionGA$party)
electionGA$district <- as.factor(electionGA$district)
electionGA$year <- as.factor(electionGA$year)
electionGA$candidate <- as.factor(electionGA$candidate)
electionGA$office <- as.factor(electionGA$office)
electionGA$candidateid <- (as.factor(electionGA$district:electionGA$office:electionGA$party:electionGA$year))


colnames(electionGA)

electionGA$GEOID <- ifelse(electionGA$district == 1, 1301, 
                           ifelse(electionGA$district == 2, 1302, 
                                  ifelse(electionGA$district == 3, 1303,
                                         ifelse(electionGA$district == 4, 1304, 
                                                ifelse(electionGA$district == 5, 1305, 
                                                       ifelse(electionGA$district == 6, 1306,
                                                              ifelse(electionGA$district == 7, 1307, 
                                                                     ifelse(electionGA$district == 8, 1308, 
                                                                            ifelse(electionGA$district == 9, 1309,
                                                                                   ifelse(electionGA$district == 10, 1310,
                                                                                          ifelse(electionGA$district == 11, 1311,    
                                                                                                 ifelse(electionGA$district == 12, 1312, 
                                                                                                        ifelse(electionGA$district == 13, 1313,
                                                                                                               ifelse(electionGA$district == 14, 1314, NA))))))))))))))

electionGA$district <- recode_factor(electionGA$district, "1" = "Congressional District 01")
electionGA$district <- recode_factor(electionGA$district, "2" = "Congressional District 02")
electionGA$district <- recode_factor(electionGA$district, "3" = "Congressional District 03")
electionGA$district <- recode_factor(electionGA$district, "4" = "Congressional District 04")
electionGA$district <- recode_factor(electionGA$district, "5" = "Congressional District 05")
electionGA$district <- recode_factor(electionGA$district, "6" = "Congressional District 06")
electionGA$district <- recode_factor(electionGA$district, "7" = "Congressional District 07")
electionGA$district <- recode_factor(electionGA$district, "8" = "Congressional District 08")
electionGA$district <- recode_factor(electionGA$district, "9" = "Congressional District 09")
electionGA$district <- recode_factor(electionGA$district, "10" = "Congressional District 10")
electionGA$district <- recode_factor(electionGA$district, "11" = "Congressional District 11")
electionGA$district <- recode_factor(electionGA$district, "12" = "Congressional District 12")
electionGA$district <- recode_factor(electionGA$district, "13" = "Congressional District 13")
electionGA$district <- recode_factor(electionGA$district, "14" = "Congressional District 14")
# names(electionGA)[names(electionGA) == 'district'] <- 'NAMELSAD20'

electionGA$GEOID <- as.factor(electionGA$GEOID)


colnames(electionGA)

####  write.csv(electionGA, file = "/Users/justinsaunders/Downloads/electionGA.csv")


electionGA$candidateraces0 <- ifelse(electionGA$candidate == "DAVID PERDUE", "White",
                                   ifelse(electionGA$candidate == "JON OSSOFF", "White",
                                          ifelse(electionGA$candidate == "BARACK OBAMA", "Black",
                                                 ifelse(electionGA$candidate == "HILLARY CLINTON", "White",
                                                        ifelse(electionGA$candidate == "DONALD J TRUMP", "White",
                                                               ifelse(electionGA$candidate == "JOSEPH R BIDEN", "White",
                                                                      ifelse(electionGA$candidate == "A DREW FERGUSON IV", "White",
                                                                             ifelse(electionGA$candidate == "ANDREW CLYDE", "White",
                                                                                    ifelse(electionGA$candidate == "ANGELA PENDLEY", "White",
                                                                                         ifelse(electionGA$candidate == "ANGELA STANTON-KING", "Black",
                                                                                   ifelse(electionGA$candidate == "AUSTIN SCOTT", "White",
                                                                                ifelse(electionGA$candidate == "BARRY D LOUDERMILK", "White",
                                                                          ifelse(electionGA$candidate == "BARRY LOUDERMILK", "White",
                                                                                    ifelse(electionGA$candidate == "BECKY E HITES", "White",
                                                              ifelse(electionGA$candidate == "BILL GILLESPIE", "White",
                                                                  ifelse(electionGA$candidate == "BILL JONES", "White",
                                                                         ifelse(electionGA$candidate == "BOBBY SAXON", "White",
                                                                            ifelse(electionGA$candidate == "BRIAN CORWIN REESE", "Black",
                                                                             ifelse(electionGA$candidate == "BRIAN KEMP", "White",
                                                                                  ifelse(electionGA$candidate == "CAROLYN BOURDEAUX", "White",
                                                                                      ifelse(electionGA$candidate == "CHUCK ENDERLIN", "White",
                                                                                  ifelse(electionGA$candidate == "DANA BARRETT", "White",
                                                                             ifelse(electionGA$candidate == 'DANIEL "DANNY" GRANT', "White",
                                                                        ifelse(electionGA$candidate == "DAVID CALLAHAN", "White",
                                                                  ifelse(electionGA$candidate == "DAVID D VOGEL", "White",
                                                              ifelse(electionGA$candidate == "DAVID SCOTT", "Black",
                                                        ifelse(electionGA$candidate == "DEBORAH HONEYCUTT", "White",
                                                  ifelse(electionGA$candidate == "DEVIN PANDY", "Black",
                                              ifelse(electionGA$candidate == "DON COLE", "White",
                                        ifelse(electionGA$candidate == "DON WILSON", "Black",
                            ifelse(electionGA$candidate == "DOUG COLLINS", "White",
                                   ifelse(electionGA$candidate == "DOUG HECKMAN", "White",
                                          ifelse(electionGA$candidate == 'DOUG HECKMANN', "White",
                                          ifelse(electionGA$candidate == "DOUGLAS BELL", "White",
                                                 ifelse(electionGA$candidate == "DREW FERGUSON", "White",
                                                        ifelse(electionGA$candidate == 'E L "BUDDY" CARTER', "White",
                                                               ifelse(electionGA$candidate == 'EARL L "BUDDY" CARTER', "White",
                                                                      ifelse(electionGA$candidate == "FENN LITTLE", "White",
                                                                             ifelse(electionGA$candidate == "FLYNN D BROADY JR", "Black",
                                                                           ifelse(electionGA$candidate == "FRANCYS JOHNSON", "Black",
                                                                             ifelse(electionGA$candidate == "FRANK SAUNDERS", "White",
                                                                      ifelse(electionGA$candidate == "GREG DUKE", "White",
                                   ifelse(electionGA$candidate == 'GREGORY P "GREG" DUKE', "White",
                                               ifelse(electionGA$candidate == 'HENRY C "HANK" JOHNSON JR', "Black",
                                                          ifelse(electionGA$candidate == "HERMAN WEST JR", "Black",
                                                                         ifelse(electionGA$candidate == "HOWARD STOPECK", "White",
                                                                                 ifelse(electionGA$candidate == 'HUGH "BUD" GAMMON', "White",
                                                                        ifelse(electionGA$candidate == 'I K "KENNETH" DIOUS', "Black",
                                                         ifelse(electionGA$candidate == "J CHRIS VAUGHN", "White",
                                                     ifelse(electionGA$candidate == "JACK KINGSTON", "White",
                                          NA))))))))))))))))))))))))))))))))))))))))))))))))))


electionGA$candidateraces1 <- ifelse(electionGA$candidate == "JAMES NEAL HARRIS", "White",
                                            ifelse(electionGA$candidate == "JEFF KAZANOW", "White",
                                                   ifelse(electionGA$candidate == "JEFF SCOTT", "White",
                                                          ifelse(electionGA$candidate == "JIM MARSHALL", "White",
                                                                 ifelse(electionGA$candidate == "JODY B HICE", "White",
                                                                        ifelse(electionGA$candidate == "JODY HICE", "White",
                                                                         ifelse(electionGA$candidate == "JODY COOLEY", "White",
                                                                     ifelse(electionGA$candidate == "JOE PROFIT", "Black",
                                                                  ifelse(electionGA$candidate == "JOHN BARROW", "White",
                                                               ifelse(electionGA$candidate == "JOHN HOUSE", "White",
                                                             ifelse(electionGA$candidate == "JOHN LEWIS", "Black",
                                                          ifelse(electionGA$candidate == "JOHN R LEWIS", "Black",
                                                      ifelse(electionGA$candidate == "JOHN LINDER", "White",
                                                    ifelse(electionGA$candidate == "JOHN STONE", "White",
                                                 ifelse(electionGA$candidate == "JOHSIE CRUZ EZAMMUDEEN", "Latino",
                                              ifelse(electionGA$candidate == "JOSH MCCALL", "White",
                                           ifelse(electionGA$candidate == "JOYCE MARIE GRIGGS", "Black",
                                       ifelse(electionGA$candidate == "KAREN HANDEL", "White",
                                   ifelse(electionGA$candidate == "KEVIN VAN AUSDAL", "White",
                                ifelse(electionGA$candidate == "LEE ANDERSON", "White",
                                       ifelse(electionGA$candidate == "LEE FERRELL", "White",
                                              ifelse(electionGA$candidate == "LESLI RAE MESSINGER", "White",
                                                     ifelse(electionGA$candidate == "LINDSAY HOLLIDAY", "White",
                                                            ifelse(electionGA$candidate == "LISA M RING", "White",
                                                                   ifelse(electionGA$candidate == 'LISBETH "LIZ" CARTER', "White",
                                                                          ifelse(electionGA$candidate == "LIZ JOHNSON", "Black",
                                                                                 ifelse(electionGA$candidate == "LUCY MCBATH", "Black",
                                                                                        ifelse(electionGA$candidate == "LYNN A WESTMORELAND", "White",
       ifelse(electionGA$candidate == "MARJORIE TAYLOR GREENE", "White",
          ifelse(electionGA$candidate == "MIKE CRANE", "White",
                 ifelse(electionGA$candidate == "MIKE KEOWN", "White",
                        ifelse(electionGA$candidate == "MITT ROMNEY", "White",
                               ifelse(electionGA$candidate == "NATHAN DEAL", "White",
                                      ifelse(electionGA$candidate == "NIKEMA WILLIAMS", "Black",
                                             ifelse(electionGA$candidate == "OSCAR L HARRIS II", "White",
                                                    ifelse(electionGA$candidate == "PATRICIA C MCCRACKEN", "White",
                                                           ifelse(electionGA$candidate == "PATRICK THOMPSON", "White",
                                                                  ifelse(electionGA$candidate == "PAUL C BROUN", "White",
                                                                         ifelse(electionGA$candidate == "PHIL GINGREY", "White",
                                                                                ifelse(electionGA$candidate == 'R W "RICK" ALLEN', "White",
                                                                                       ifelse(electionGA$candidate == "RASHID MALIK", "AAPI",
                                                                                              NA)))))))))))))))))))))))))))))))))))))))))
                                                                                              



                                                                                              
                                                                                              
electionGA$candidateraces2 <- ifelse(electionGA$candidate == "RAYMOND MCKINNEY", "White",
               ifelse(electionGA$candidate == "RICH MCCORMICK", "White",
                      ifelse(electionGA$candidate == "RICK GODDARD", "White",
                             ifelse(electionGA$candidate == "RICK W ALLEN", "White",
                                    ifelse(electionGA$candidate == "ROB WOODALL", "White",
                                           ifelse(electionGA$candidate == "ROBERT G MONTIGEL", "White",
                                                  ifelse(electionGA$candidate == "RODNEY STOOKSBURY", "White",
                                                         ifelse(electionGA$candidate == "RUSSELL EDWARDS", "White",
                                                                ifelse(electionGA$candidate == "S MALIK", "AAPI",
                                                                       ifelse(electionGA$candidate == "SANFORD BISHOP", "Black",
                                 ifelse(electionGA$candidate == "SANFORD D BISHOP JR", "Black",
                              ifelse(electionGA$candidate == "STACEY ABRAMS", "Black",
                          ifelse(electionGA$candidate == "STEPHEN CAMP", "White",
                        ifelse(electionGA$candidate == "STEVE REILLY", "White",
                    ifelse(electionGA$candidate == "STEVEN LAMAR FOSTER", "White",
                   ifelse(electionGA$candidate == "TABITHA A JOHNSON-GREEN", "Black",
                 ifelse(electionGA$candidate == "TABITHA JOHNSON-GREEN", "Black",
                        ifelse(electionGA$candidate == "THOMAS D WRIGHT", "White",
                               ifelse(electionGA$candidate == "TOM GRAVES", "White",
                                      ifelse(electionGA$candidate == "TOM PRICE", "White",
                                             ifelse(electionGA$candidate == "VAL ALMONORD", "Black",
                                                    ifelse(electionGA$candidate == "VICTOR ARMENDARIZ", "Latino",
                                                               NA))))))))))))))))))))))

# Replace missing values in col1 with values in col0
electionGA$candidateraces1 <- ifelse(is.na(electionGA$candidateraces1), electionGA$candidateraces0, electionGA$candidateraces1)
# Replace missing values in col2 with values in col1
electionGA$candidateraces2 <- ifelse(is.na(electionGA$candidateraces2), electionGA$candidateraces1, electionGA$candidateraces2)
electionGA$candidaterace <- electionGA$candidateraces2

electionGA <- electionGA[, c("district","state","candidatevotes","candidate","totalvotes","party","office","year","candidateid","GEOID","candidaterace")]
colnames(electionGA)


electionGA$candidategender0 <- ifelse(electionGA$candidate == "DAVID PERDUE" | electionGA$candidate == "JON OSSOFF", "Male",
                                     ifelse(electionGA$candidate == "BARACK OBAMA", "Male",
                                            ifelse(electionGA$candidate == "HILLARY CLINTON", "Female",
                                                   ifelse(electionGA$candidate == "DONALD J TRUMP", "Male",
                                                          ifelse(electionGA$candidate == "JOSEPH R BIDEN", "Male",
                                                                 ifelse(electionGA$candidate == "A DREW FERGUSON IV", "Male",
                                                                        ifelse(electionGA$candidate == "ANDREW CLYDE", "Male",
                                                                               ifelse(electionGA$candidate == "ANGELA PENDLEY", "Female",
                                                                                      ifelse(electionGA$candidate == "ANGELA STANTON-KING", "Female",
                                                                                             ifelse(electionGA$candidate == "AUSTIN SCOTT", "Male",
                                                                                                    ifelse(electionGA$candidate == "BARRY D LOUDERMILK", "Male",
                                                                                                           ifelse(electionGA$candidate == "BARRY LOUDERMILK", "Male",
                                                                                                                  ifelse(electionGA$candidate == "BECKY E HITES", "Female",
                                                                                                                         ifelse(electionGA$candidate == "BILL GILLESPIE", "Male",
                                                                                                                                ifelse(electionGA$candidate == "BILL JONES", "Male",
                                                                                                                                       ifelse(electionGA$candidate == "BOBBY SAXON", "Male",
                                                                                                                                              ifelse(electionGA$candidate == "BRIAN CORWIN REESE", "Male",
                                                                                                                                                     ifelse(electionGA$candidate == "BRIAN KEMP", "Male",
                                                                                                                                                            ifelse(electionGA$candidate == "CAROLYN BOURDEAUX", "Female",
                                                                                                                                                                   ifelse(electionGA$candidate == "CHUCK ENDERLIN", "Male",
                                                                                                                                                                          ifelse(electionGA$candidate == "DANA BARRETT", "Female",
                                                                                                                                                                                 ifelse(electionGA$candidate == 'DANIEL "DANNY" GRANT', "Male",
                                                                                                                                                                                        ifelse(electionGA$candidate == "DAVID CALLAHAN", "Male",
                                                                                                                                                                                               ifelse(electionGA$candidate == "DAVID D VOGEL", "Male",
                                                                                                                                                                                                      ifelse(electionGA$candidate == "DAVID SCOTT", "Male",
                                                                                                                                                                                                             ifelse(electionGA$candidate == "DEBORAH HONEYCUTT", "Female",
                                                                                                                                                                                                                    ifelse(electionGA$candidate == "DEVIN PANDY", "Male",
                                                                                                                                                                                                                           ifelse(electionGA$candidate == "DON COLE", "Male",
                                                                                                                                                                                                                                  ifelse(electionGA$candidate == "DON WILSON", "Male",
                                                                                                                                                                                                                                                ifelse(electionGA$candidate == "DOUG COLLINS", "Male",
                                                                                                                                                                                                                                                       ifelse(electionGA$candidate == "DOUG HECKMAN", "Male",
                                                                                                                                                                                                                                                              ifelse(electionGA$candidate == 'DOUG HECKMANN', "Male",
                                                                                                                                                                                                                                                                     ifelse(electionGA$candidate == "DOUGLAS BELL", "Male",
                                                                                                                                                                                                                                                                            ifelse(electionGA$candidate == "DREW FERGUSON", "Male",
                                                                                                                                                                                                                                                                                   ifelse(electionGA$candidate == 'E L "BUDDY" CARTER', "Male",
                                                                                                                                                                                                                                                                                          ifelse(electionGA$candidate == 'EARL L "BUDDY" CARTER', "Male",
                                                                                                                                                                                                                                                                                                 ifelse(electionGA$candidate == "FENN LITTLE", "Male",
                                                                                                                                                                                                                                                                                                        ifelse(electionGA$candidate == "FLYNN D BROADY JR", "Male",
                                                                                                                                                                                                                                                                                                               ifelse(electionGA$candidate == "FRANCYS JOHNSON", "Male",
                                                                                                                                                                                                                                                                                                                      ifelse(electionGA$candidate == "FRANK SAUNDERS", "Male",
                                                                                                                                                                                                                                                                                                                             ifelse(electionGA$candidate == "GREG DUKE", "Male",
                                                                                                                                                                                                                                                                                                                                    ifelse(electionGA$candidate == 'GREGORY P "GREG" DUKE', "Male",
                                                                                                                                                                                                                                                                                                                                           ifelse(electionGA$candidate == 'HENRY C "HANK" JOHNSON JR', "Male",
                                                                                                                                                                                                                                                                                                                                                  ifelse(electionGA$candidate == "HERMAN WEST JR", "Male",
                                                                                                                                                                                                                                                                                                                                                         ifelse(electionGA$candidate == "HOWARD STOPECK", "Male",
                                                                                                                                                                                                                                                                                                                                                                ifelse(electionGA$candidate == 'HUGH "BUD" GAMMON', "Male",
                                                                                                                                                                                                                                                                                                                                                                       ifelse(electionGA$candidate == 'I K "KENNETH" DIOUS', "Male",
                                                                                                                                                                                                                                                                                                                                                                              ifelse(electionGA$candidate == "J CHRIS VAUGHN", "Male",
                                                                                                                                                                                                                                                                                                                                                                                     ifelse(electionGA$candidate == "JACK KINGSTON", "Male",
                                                                                                                                                                                                                                                                                                                                                                                            
NA)))))))))))))))))))))))))))))))))))))))))))))))))
                                                                                                                                                                                                                                                                                                                                                    

electionGA$candidategender1 <- ifelse(electionGA$candidate == "JAMES NEAL HARRIS", "Male",
                                     ifelse(electionGA$candidate == "JEFF KAZANOW", "Male",
                                            ifelse(electionGA$candidate == "JEFF SCOTT", "Male",
                                                   ifelse(electionGA$candidate == "JIM MARSHALL", "Male",
                                                          ifelse(electionGA$candidate == "JODY B HICE", "Male",
                                                                 ifelse(electionGA$candidate == "JODY HICE", "Male",
                                                                        ifelse(electionGA$candidate == "JODY COOLEY", "Male",
                                                                               ifelse(electionGA$candidate == "JOE PROFIT", "Male",
                                                                                      ifelse(electionGA$candidate == "JOHN BARROW", "Male",
                                                                                             ifelse(electionGA$candidate == "JOHN HOUSE", "Male",
                                                                                                    ifelse(electionGA$candidate == "JOHN LEWIS", "Male",
                                                                                                           ifelse(electionGA$candidate == "JOHN R LEWIS", "Male",
                                                                                                                  ifelse(electionGA$candidate == "JOHN LINDER", "Male",
                                                                                                                         ifelse(electionGA$candidate == "JOHN STONE", "Male",
                                                                                                                                ifelse(electionGA$candidate == "JOHSIE CRUZ EZAMMUDEEN", "Female",
                                                                                                                                       ifelse(electionGA$candidate == "JOSH MCCALL", "Male",
                                                                                                                                              ifelse(electionGA$candidate == "JOYCE MARIE GRIGGS", "Female",
                                                                                                                                                     ifelse(electionGA$candidate == "KAREN HANDEL", "Female",
                                                                                                                                                            ifelse(electionGA$candidate == "KEVIN VAN AUSDAL", "Male",
                                                                                                                                                                   ifelse(electionGA$candidate == "LEE ANDERSON", "Male",
                                                                                                                                                                          ifelse(electionGA$candidate == "LEE FERRELL", "Male",
                                                                                                                                                                                 ifelse(electionGA$candidate == "LESLI RAE MESSINGER", "Female",
                                                                                                                                                                                        ifelse(electionGA$candidate == "LINDSAY HOLLIDAY", "Male",
                                                                                                                                                                                               ifelse(electionGA$candidate == "LISA M RING", "Female",
                                                                                                                                                                                                      ifelse(electionGA$candidate == 'LISBETH "LIZ" CARTER', "Female",
                                                                                                                                                                                                             ifelse(electionGA$candidate == "LIZ JOHNSON", "Female",
                                                                                                                                                                                                                    ifelse(electionGA$candidate == "LUCY MCBATH", "Female",
                                                                                                                                                                                                                           ifelse(electionGA$candidate == "LYNN A WESTMORELAND", "Male",
                                                                                                                                                                                                                                  ifelse(electionGA$candidate == "MARJORIE TAYLOR GREENE", "Female",
                                                                                                                                                                                                                                         ifelse(electionGA$candidate == "MIKE CRANE", "Male",
                                                                                                                                                                                                                                                ifelse(electionGA$candidate == "MIKE KEOWN", "Male",
                                                                                                                                                                                                                                                       ifelse(electionGA$candidate == "MITT ROMNEY", "Male",
                                                                                                                                                                                                                                                              ifelse(electionGA$candidate == "NATHAN DEAL", "Male",
                                                                                                                                                                                                                                                                     ifelse(electionGA$candidate == "NIKEMA WILLIAMS", "Female",
                                                                                                                                                                                                                                                                            ifelse(electionGA$candidate == "OSCAR L HARRIS II", "Male",
                                                                                                                                                                                                                                                                                   ifelse(electionGA$candidate == "PATRICIA C MCCRACKEN", "Female",
                                                                                                                                                                                                                                                                                          ifelse(electionGA$candidate == "PATRICK THOMPSON", "Male",
                                                                                                                                                                                                                                                                                                 ifelse(electionGA$candidate == "PAUL C BROUN", "Male",
                                                                                                                                                                                                                                                                                                        ifelse(electionGA$candidate == "PHIL GINGREY", "Male",
                                                                                                                                                                                                                                                                                                               ifelse(electionGA$candidate == 'R W "RICK" ALLEN', "Male",
                                                                                                                                                                                                                                                                                                                      ifelse(electionGA$candidate == "RASHID MALIK", "Male",
NA)))))))))))))))))))))))))))))))))))))))))

electionGA$candidategender2 <- ifelse(electionGA$candidate == "RAYMOND MCKINNEY", "Male",
                                     ifelse(electionGA$candidate == "RICH MCCORMICK", "Male",
                                            ifelse(electionGA$candidate == "RICK GODDARD", "Male",
                                                   ifelse(electionGA$candidate == "RICK W ALLEN", "Male",
                                                          ifelse(electionGA$candidate == "ROB WOODALL", "Male",
                                                                 ifelse(electionGA$candidate == "ROBERT G MONTIGEL", "Male",
                                                                        ifelse(electionGA$candidate == "RODNEY STOOKSBURY", "Male",
                                                                               ifelse(electionGA$candidate == "RUSSELL EDWARDS", "Male",
                                                                                      ifelse(electionGA$candidate == "S MALIK", "Male",
                                                                                             ifelse(electionGA$candidate == "SANFORD BISHOP", "Male",
                                                                                                    ifelse(electionGA$candidate == "SANFORD D BISHOP JR", "Male",
                                                                                                           ifelse(electionGA$candidate == "STACEY ABRAMS", "Female",
                                                                                                                  ifelse(electionGA$candidate == "STEPHEN CAMP", "Male",
                                                                                                                         ifelse(electionGA$candidate == "STEVE REILLY", "Male",
                                                                                                                                ifelse(electionGA$candidate == "STEVEN LAMAR FOSTER", "Male",
                                                                                                                                       ifelse(electionGA$candidate == "TABITHA A JOHNSON-GREEN", "Female",
                                                                                                                                              ifelse(electionGA$candidate == "TABITHA JOHNSON-GREEN", "Female",
                                                                                                                                                     ifelse(electionGA$candidate == "THOMAS D WRIGHT", "Male",
                                                                                                                                                            ifelse(electionGA$candidate == "TOM GRAVES", "Male",
                                                                                                                                                                   ifelse(electionGA$candidate == "TOM PRICE", "Male",
                                                                                                                                                                          ifelse(electionGA$candidate == "VAL ALMONORD", "Male",
                                                                                                                                                                                 ifelse(electionGA$candidate == "VICTOR ARMENDARIZ", "Male",
NA))))))))))))))))))))))



# Replace missing values in col1 with values in col0
electionGA$candidategender1 <- ifelse(is.na(electionGA$candidategender1), electionGA$candidategender0, electionGA$candidategender1)
# Replace missing values in col2 with values in col1
electionGA$candidategender2 <- ifelse(is.na(electionGA$candidategender2), electionGA$candidategender1, electionGA$candidategender2)
electionGA$candidategender <- electionGA$candidategender2

electionGA <- electionGA[, c("district","state","candidatevotes","candidate","totalvotes","party","office","year","candidateid","GEOID","candidaterace","candidategender")]
electionGA$candidaterace <- as.factor(electionGA$candidaterace)
electionGA$candidategender <- as.factor(electionGA$candidategender)
colnames(electionGA)

write.csv(electionGA, file = "/Users/justinsaunders/Downloads/electionGA2.csv")


############################################
reg_initialGA <- lm(candidatevotes ~ party + totalvotes + candidategender + candidaterace + office,
                    data=electionGA)
print(summary(reg_initialGA))

residuals_initialGA <- resid(reg_initialGA)
plot(fitted(reg_initialGA), residuals_initialGA,
     ylab = "Residuals", xlab = "Expected Dependent Values",
     main = "Initial Vote Count Residuals GA")
abline(0,0)

