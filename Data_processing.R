#Dataset processing

library(tidyverse)

Death_in_NL <- read_csv2(file = "Data/7233ENG_TypedDataSet_04112022_143444.csv")

Death_in_NL <- Death_in_NL %>%
  drop_na(Deaths_1)%>%
  mutate(Sex = recode(Sex,
                      "3000" = "Male", "4000" = "Female")) %>%
  mutate(CausesOfDeath = recode(CausesOfDeath,
                                "A010668" = "Infections", "A010840" = "Neoplasms",
                                "A011013" = "Endocr+Metabol", "A011087" = "Psychological",
                                "A011166" = "NervousSystem", "A011234" = "Eye+Adnexa",
                                "A011307" = "CirculatorySystem", "A011384" = "RespiratorySystem",
                                "A011449" = "DigestiveSystem", "A011521" = "SkinDeseases",
                                "A011594" = "MusculSystem", "A011674" = "GenitourinarySystem",
                                "A011757" = "Pregnancy+Childbirth", "A011833" = "Perinatal",
                                "A011893" = "CongenitalMalform", "A011981" = "UnclassifiedAbnorm",
                                "A050205" = "COVID-19", "A012072" = "ExternalCauses")) %>%
  mutate(Age = recode(Age,
                      "10000" = "Total", "10010" = "0", "22000" = "95+", "51300" = "1-9",
                      "70200" = "1-9", "70300" = "10-14", "70400" = "15-19", "70500" = "20-24",
                      "70600" = "25-29", "70700" = "30-34", "70800" = "35-39", "70900" = "40-44",
                      "71000" = "45-49", "71100" = "50-54", "71200" = "55-59", "71300" = "60-64",
                      "71400" = "65-69", "71500" = "70-74", "71600" = "75-79", "71700" = "80-84",
                      "71800" = "85-89", "71900" = "90-94")) %>%
  mutate(Periods = recode(Periods,
                          "1996JJ00" = "1996", "1997JJ00" = "1997", "1998JJ00" = "1998", "1999JJ00" = "1999",
                          "2000JJ00" = "2000", "2001JJ00" = "2001", "2002JJ00" = "2002", "2003JJ00" = "2003",
                          "2004JJ00" = "2004", "2005JJ00" = "2005", "2006JJ00" = "2006", "2007JJ00" = "2007",
                          "2008JJ00" = "2008", "2009JJ00" = "2009", "2010JJ00" = "2010", "2011JJ00" = "2011",
                          "2012JJ00" = "2012", "2013JJ00" = "2013", "2014JJ00" = "2014", "2015JJ00" = "2015",
                          "2016JJ00" = "2016",  "2017JJ00" = "2017", "2018JJ00" = "2018",  "2019JJ00" = "2019",
                          "2020JJ00" = "2020",  "2021JJ00" = "2021")) %>%
  rename("Year" = "Periods", "Deaths" = "Deaths_1")%>%
  mutate(Year = as.numeric(Year))

write.csv(Death_in_NL, file = 'Data/Death_in_NL_formatted.csv', row.names = FALSE)


