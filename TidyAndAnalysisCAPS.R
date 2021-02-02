library(tidyverse)

caps <- read_csv("./Data/Mental Health and CAPS Responses.csv")

# Loading Distribution Class
# Cleaning the data
classDist <- read_csv("./UNLDistribution2019/ClassLevelDistribution.csv")
classDist$Percentage = as.numeric(str_replace_all(classDist$Percentage, '%', ''))

genderDist <- read_csv("./UNLDistribution2019/GenderDistribution.csv")
genderDist$Percentage = as.numeric(str_replace_all(genderDist$Percentage, '%', ''))

majorDist <- read_csv("./UNLDistribution2019/MajorDistribution.csv")
majorDist <- majorDist %>%
  select(College, Percentage)
majorDist$Percentage = as.numeric(str_replace_all(majorDist$Percentage, '%', ''))

raceDist <- read_csv("./UNLDistribution2019/RaceDistribution.csv")
raceDist$Percentage = as.numeric(str_replace_all(raceDist$Percentage, '%', ''))

# Summarizing Clean Data (No Errors in General)
caps$Class[caps$Class == "Graduate Student"] <- 'Graduate'

sampleClass <- caps %>% 
  select(Class) %>%
  mutate(count = 1) %>%
  group_by(Class) %>%
  summarize(freq = sum(count)) %>%
  mutate(percentage = 100*freq/sum(freq))

# Cleaning Data
unique(caps$Race)
unique(raceDist$`Race/Ethnicity`)

# Correcting data entry
caps$Race[caps$Race == "American Indian/Alaskan Native"] <- "Alaskan/Native American"
caps$Race[caps$Race == "White (Non-hispanic)"] <- "White (Non-Hispanic)"
caps$Race[caps$Race == "Black (Non-hispanic)"] <- "Black (Non-Hispanic)"

# Dealing with outlier data (I don't think the right word is outlier btw)
caps$Race[!(caps$Race %in% raceDist$`Race/Ethnicity`)] <- "Other"

sampleRace <- caps %>%
  select(Race) %>%
  mutate(count = 1) %>%
  group_by(Race) %>%
  summarize(freq = sum(count)) %>%
  mutate(percentage = 100*freq/sum(freq))

# Cleaning for Gender
unique(caps$Gender)

caps$Gender[!(caps$Gender %in% genderDist$Gender)] <- "Other"

sampleGender <- caps %>%
  select(Gender) %>%
  mutate(count = 1) %>%
  group_by(Gender) %>%
  summarize(freq = sum(count)) %>%
  mutate(percentage = 100*freq/sum(freq))

# Cleaning for College
unique(caps$College)
unique(majorDist$College)

# Hard coding for correction (you don't want to do this for large datasets)
caps$College[caps$College == "Business"] <- "College of Business"
caps$College[caps$College == "Ag economics"] <- "College of Agricultural Science and Natural Resources"
caps$College[caps$College == "Pre-Health"] <- "College of Arts and Sciences"
caps$College[caps$College == "Fashion Communications"] <- "College of Journalism and Mass Communication"
caps$College[caps$College == "College of Agricultural Science and Natural Resources"] <- "College of Agricultural Sciences and Natural Resources"

caps$College[!(caps$College %in% majorDist$College)] <- "Other"

sampleClass <- caps[caps$Online == 1,] %>%
  select(Class) %>%
  mutate(count = 1) %>%
  group_by(Class) %>%
  summarize(freq = sum(count)) %>%
  mutate(percentage = 100*freq/sum(freq))

# Comparing Distributions (Class)
classDist <- classDist %>%
  mutate(Type = "Actual")

sampleClass <- sampleClass %>%
  select(Class, percentage) %>%
  rename(`Class Level` = Class, Percentage = percentage) %>%
  mutate(Type = "Sample")

classComp <- rbind(classDist, sampleClass)

ggplot(data = classComp, aes(fill=Type, y=Percentage, x=`Class Level`)) +
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Class Level Distribution Comparison between Actual vs. Sample") +
  theme_minimal()


# Comparing Distributions (Gender)
genderDist <- genderDist %>%
  mutate(Type = "Actual")

sampleGender <- sampleGender %>%
  select(Gender, percentage) %>%
  rename(Gender = Gender, Percentage = percentage) %>%
  mutate(Type = "Sample")

genderComp <- rbind(genderDist, sampleGender)

ggplot(genderComp, aes(fill=Type, y=Percentage, x=Gender)) +
  geom_bar(position="dodge", stat="identity")  + 
  ggtitle("Gender Distribution Comparison between Actual vs. Sample") +
  theme_minimal()

# Comparing Distributions (Race)
raceDist <- raceDist %>%
  mutate(Type = "Actual")

sampleRace <- sampleRace %>%
  select(Race, percentage) %>%
  rename(`Race/Ethnicity` = Race, Percentage = percentage) %>%
  mutate(Type = "Sample")

raceComp <- rbind(raceDist, sampleRace)

ggplot(raceComp, aes(fill=Type, y=Percentage, x=`Race/Ethnicity`)) +
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Race Distribution Comparison between Actual vs. Sample") +
  theme_minimal()


# Comparing Distributions (College)
majorDist <- majorDist %>%
  mutate(Type = "Actual")

sampleCollege <- sampleCollege %>%
  select(College, percentage) %>%
  rename(College = College, Percentage = percentage) %>%
  mutate(Type = "Sample")

collegeComp <- rbind(majorDist, sampleCollege)

# Relabelling for better visuals
collegeComp$College[collegeComp$College == "College of Agricultural Sciences and Natural Resources"] <- 'CASNR'
collegeComp$College[collegeComp$College == "College of Architecture"] <- "CA"
collegeComp$College[collegeComp$College == "College of Arts and Sciences"] <- "CAS"
collegeComp$College[collegeComp$College == "College of Business"] <- "COB"
collegeComp$College[collegeComp$College == "College of Education and Human Sciences"] <- "CEHS"
collegeComp$College[collegeComp$College == "College of Engineering"] <- "CE"
collegeComp$College[collegeComp$College == "College of Fine and Performing Arts"] <- "CFPA"
collegeComp$College[collegeComp$College == "College of Journalism and Mass Communication"] <- "CJMC"
collegeComp$College[collegeComp$College == "College of Law"] <- "Law"
collegeComp$College[collegeComp$College == "Graduate Studies"] <- "Grad"

ggplot(collegeComp, aes(fill=Type, y=Percentage, x=College)) +
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("College Distribution Comparison between Actual vs. Sample") +
  theme_minimal()

# Looking at the Distribution of Results
# Serious Issue
serious <- caps %>%
  select(`Serious Issue`) %>%
  mutate(count = 1) %>%
  mutate(`Serious Issue` = as.character(`Serious Issue`)) %>%
  group_by(`Serious Issue`) %>%
  summarize(freq = sum(count))

serious <- serious %>% add_row(`Serious Issue` = '1', freq = 0)

summary(caps$`Serious Issue`)

serious <- serious %>% mutate(percentage = (freq*100)/sum(freq))

ggplot(serious, aes(x = `Serious Issue`, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="lightblue", fill="lightblue") +
  theme_minimal() +
  ggtitle("Distribution of Serious Issue")

# Personal Mental Health
pmh <- caps %>%
  select(`Personal Mental Health`) %>%
  mutate(count = 1) %>%
  mutate(`Personal Mental Health` = as.character(`Personal Mental Health`)) %>%
  group_by(`Personal Mental Health`) %>%
  summarize(freq = sum(count)) %>%
  mutate(percentage = freq * 100 / sum(freq))

summary(caps$`Personal Mental Health`)

ggplot(pmh, aes(x = `Personal Mental Health`, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="lightblue", fill="lightblue") +
  theme_minimal() +
  ggtitle("Distribution of Personal Mental Health")


# Stress Level
stress <- caps %>%
  select(`Stress Level`) %>%
  mutate(count = 1) %>%
  mutate(`Stress Level` = as.character(`Stress Level`)) %>%
  group_by(`Stress Level`) %>%
  summarize(freq = sum(count)) %>% 
  mutate(percentage = freq * 100 / sum(freq))

stress <- stress %>% add_row(`Stress Level` = '1', freq = 0)

summary(caps$`Stress Level`)

ggplot(stress, aes(x = `Stress Level`, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="lightblue", fill="lightblue") +
  theme_minimal() +
  ggtitle("Distribution of Stress Level")

# Academic Performance
academic <- caps %>%
  select(`Academic Performance`) %>%
  mutate(count = 1) %>%
  mutate(`Academic Performance` = as.character(`Academic Performance`)) %>%
  group_by(`Academic Performance`) %>%
  summarize(freq = sum(count)) %>% 
  mutate(percentage = freq * 100 / sum(freq))

summary(caps$`Academic Performance`)


ggplot(academic, aes(x = `Academic Performance`, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="lightblue", fill="lightblue") +
  theme_minimal() +
  ggtitle("Distribution of Academic Performance")

# Informed
informed <- caps %>%
  select(Informed) %>%
  mutate(count = 1) %>%
  mutate(Informed = as.character(Informed)) %>%
  group_by(Informed) %>%
  summarize(freq = sum(count)) %>% 
  mutate(percentage = freq * 100 / sum(freq))

summary(caps$Informed)

ggplot(informed, aes(x = Informed, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="lightblue", fill="lightblue") +
  theme_minimal() +
  ggtitle("Distribution of Informed")

# CAPS Adequacy 
adq <- caps %>%
  select(`CAPS Adequacy`) %>%
  mutate(count = 1) %>%
  mutate(`CAPS Adequacy` = as.character(`CAPS Adequacy`)) %>%
  group_by(`CAPS Adequacy`) %>%
  summarize(freq = sum(count)) %>% 
  mutate(percentage = freq * 100 / sum(freq))

summary(caps$`CAPS Adequacy`)

# Remove NA
adq <- adq[complete.cases(adq),]

ggplot(adq, aes(x = `CAPS Adequacy`, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="lightblue", fill="lightblue") +
  theme_minimal() +
  ggtitle("Distribution of CAPS Adequacy")

# CAPS USAGE
usage <- caps %>%
  select(`CAPS Usage`) %>%
  mutate(count = 1) %>%
  mutate(`CAPS Usage` = as.character(`CAPS Usage`)) %>%
  group_by(`CAPS Usage`) %>%
  summarize(freq = sum(count)) %>%
  mutate(prop = freq / sum(freq) * 100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(usage, aes(x = `CAPS Usage`, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="lightblue", fill="lightblue") +
  theme_minimal() +
  ggtitle("Distribution of CAPS Usage")

# CAPS Funding
funding <- caps %>%
  select(Funding) %>%
  mutate(count = 1) %>%
  mutate(Funding = as.character(Funding)) %>%
  group_by(Funding) %>%
  summarize(freq = sum(count)) %>% 
  mutate(percentage = freq * 100 / sum(freq))

summary(caps$Funding)

# Remove NA
funding <- funding[complete.cases(funding),]

ggplot(funding, aes(x = Funding, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="lightblue", fill="lightblue") +
  theme_minimal() +
  ggtitle("Distribution of Funding")

# CAPS No-Show Fee
noshow <- caps %>%
  select(`No-show Fee`) %>%
  mutate(count = 1) %>%
  mutate(`No-show Fee` = as.character(`No-show Fee`)) %>%
  group_by(`No-show Fee`) %>%
  summarize(freq = sum(count)) %>% 
  mutate(percentage = freq * 100 / sum(freq))

summary(caps$`No-show Fee`)

ggplot(noshow, aes(x = `No-show Fee`, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="lightblue", fill="lightblue") +
  theme_minimal() +
  ggtitle("Distribution of No-Show Fee")

# ASUN
asun <- caps %>%
  select(`ASUN`) %>%
  mutate(count = 1) %>%
  filter(ASUN > 0) %>%
  mutate(`ASUN` = as.character(`ASUN`)) %>%
  group_by(`ASUN`) %>%
  summarize(freq = sum(count)) %>% 
  mutate(percentage = freq * 100 / sum(freq))

summary(caps[caps$ASUN != 0,]$ASUN)

ggplot(asun, aes(x = ASUN, y=freq)) +
  geom_bar(position="dodge", stat="identity", color="lightblue", fill="lightblue") +
  theme_minimal() +
  ggtitle("Distribution of ASUN")

# Online
online <- caps %>%
  select(Online) %>%
  mutate(count = 1) %>%
  mutate(`Online` = as.character(`Online`)) %>%
  group_by(`Online`) %>%
  summarize(freq = sum(count))

ggplot(online, aes(x="", y=freq, fill=`Online`)) +
  geom_bar(stat="identity", width=1, color = 'white') +
  coord_polar("y", start=0) +
  ggtitle("Distribution of Online") +
  theme_void()
