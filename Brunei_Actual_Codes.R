install.packages("install.load")
install.load::install_load("haven",
                           "tidyverse",
                           "survey",
                           "openxlsx",
                           "MASS",
                           "labelled",
                           "readxl",
                           "expss",
                           "formattable")

# A. Communication and Collaboration

###   Digital Skill                                                         Database variable

## 1. Participating in social networks (including social media) 	            CC1
## 2. Making calls over the internet or messaging apps                        CC2
## 3. Sending messages (e.g. email, SMS) with attached files	                CC3
## 4. Taking part in consultation or voting via Internet	                    CC4

design_ind21$variables$ICC1<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$I1_C==1,1,0)
design_ind21$variables$ICC2<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$C7_C==1,1,0)
design_ind21$variables$ICC3<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$C7_D1==1,1,0)
design_ind21$variables$ICC4<-ifelse(design_ind21$variables$C3==1 & 
                                      (design_ind21$variables$G4_D==1 | design_ind21$variables$G4_E==1),1,0)

design_ind21$variables$ICC<-design_ind21$variables$ICC1+
  design_ind21$variables$ICC2+
  design_ind21$variables$ICC3+
  design_ind21$variables$ICC4


# B. Digital content creation

###   Digital Skill                                            Database variable

## 1. Creating electronic presentations 	                           DCC1
## 2. Writing a computer program 	                                   DCC2
## 3. Using basic arithmetic formula in a spreadsheet 	             DCC3
## 4. Using copy and paste tools 	                                   DCC4
## 5. Uploading self/user-created content	                           DCC5
## 6. Using editing software/application over the internet	         DCC6

design_ind21$variables$IDC1<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$I1_B==1,1,0)
design_ind21$variables$IDC2<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$I1_G==1,1,0)
design_ind21$variables$IDC3<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$I1_D==1,1,0)
design_ind21$variables$IDC4<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$I1_I==1,1,0)
design_ind21$variables$IDC5<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$C11_B==1,1,0)
design_ind21$variables$IDC6<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$C11_C==1,1,0)

design_ind21$variables$IDC<-design_ind21$variables$IDC1+
  design_ind21$variables$IDC2+
  design_ind21$variables$IDC3+
  design_ind21$variables$IDC4+
  design_ind21$variables$IDC5+
  design_ind21$variables$IDC6

# C. Information and Data Literacy

###   Digital Skill                                               Database variable

## 1. Reading or downloading newspapers, magazines or books	            IDC1    
## 2. Getting information about goods or services	                      IDC2
## 3. Seeking health information	                                      IDC3
## 4. Verifying the reliability of information found online	            IDC4

design_ind21$variables$IDL2 <- ifelse(design_ind21$variables$C8_A == 1, 1, 0)
design_ind21$variables$IDL3 <- ifelse(design_ind21$variables$C9_D == 1, 1, 0)
design_ind21$variables$IDL4 <- ifelse(design_ind21$variables$C8_B == 1, 1, 0)

design_ind21$variables$IDL <- design_ind21$variables$IDL2 +
  design_ind21$variables$IDL3 +
  design_ind21$variables$IDL4

# The above code basically means that :

"Give a score of 1 to someone if they used the internet (C3) 
AND used it to get information about goods or services (C8_A). 
Otherwise, give them a 0"

#ifelse(condition, value_if_true, value_if_false)

#C8_A == 1: This is the condition being checked.

#1: This is the value returned if the condition is true.

#0: This is the value returned if the condition is false.

#In C8_A, the Brazilians used the following as options for respondents: 

# value label
# 0 No
# 1 Yes
# 97 Undecided
# 98 No answer
# 99 Not applicable



# D. Problem Solving

###   Digital Skill                                                      Database variable

## 1. Finding, downloading, installing and configuring software & apps 	      PS1
## 2. Transferring files or applications between devices 	                    PS2
## 3. Electronic financial transactions (e.g. internet banking)	              PS3
## 4. Purchasing or ordering goods or services (online)	                      PS4
## 5. Doing an online course	                                                PS5
## 6. Connecting and installing new devices	                                  PS6

design_ind21$variables$IPL1<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$I1_F==1,1,0)
design_ind21$variables$IPL2<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$I1_E==1,1,0)
design_ind21$variables$IPL3<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$I1_H==1,1,0)
design_ind21$variables$IPL4<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$C8_H==1,1,0)
design_ind21$variables$IPL5<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$C10_B==1,1,0)
design_ind21$variables$IPL6<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$H2==1,1,0)

design_ind21$variables$IPL<-design_ind21$variables$IPL1+
  design_ind21$variables$IPL2+
  design_ind21$variables$IPL3+
  design_ind21$variables$IPL4+
  design_ind21$variables$IPL5+
  design_ind21$variables$IPL6

# E. Safety

###   Digital Skill                                                           Database variable

## 1. Changing privacy setting on device, account or app 	                          SFY1
## 2. Setting up effective security measures to protect devices & accounts 	        SFY2




#IDL2: person got info about goods/services online
#IDL3: person read news online
#IDL4: person searched health info online
#Each is 1 if yes, 0 if no.
#You also have C3, which likely means: "Did the person use the internet?" (1 = yes)

#===============================================================================
# Calculating the Skills by areas
#-------------------------------------------------------------------------------

# Areas are defined as classes, summing the amount of activities an individual executed
## 0 'None'                ===>       No activity 
## 1 'Basic'               ===>       At least one activity
## 2 'Above basic'         ===>       Two or more activities
## 9 'Not applicable'      ===>       Not an Internet user

# A. Information and data literacy
design_ind21$variables$AIndex_IDL<-ifelse((design_ind21$variables$IDL2+
                                             design_ind21$variables$IDL3+
                                             design_ind21$variables$IDL4)==0 & design_ind21$variables$C3==1,0,
                                          ifelse((design_ind21$variables$IDL2+
                                                    design_ind21$variables$IDL3+
                                                    design_ind21$variables$IDL4)==1 & design_ind21$variables$C3==1,1,
                                                 ifelse((design_ind21$variables$IDL2+
                                                           design_ind21$variables$IDL3+
                                                           design_ind21$variables$IDL4)>1 & design_ind21$variables$C3==1,2,
                                                        ifelse(design_ind21$variables$C3!=1,9,0))))

#“If a person used the internet (C3 == 1), score their digital skill level based on 
#how many online activities they did:

#0 skills = 0,
#1 skill = 1,
#2 or more = 2.
#If they didn’t use the internet at all, assign 9.”

#| #Internet Use (`C3`) | Digital Activities (`IDL2 + IDL3 + IDL4`) | `AIndex_IDL` Value | Meaning                                   |
#| #------------------- | ----------------------------------------- | ------------------ | ----------------------------------------- |
#  | Yes (1)             | 0                                         | 0                  | Used internet, but **no digital skills**  |
#  | Yes (1)             | 1                                         | 1                  | Used internet, with **1 digital skill**   |
#  | Yes (1)             | 2 or 3                                    | 2                  | Used internet, with **2+ digital skills** |
#  | No (not 1)          | (any)                                     | 9                  | **Did not use internet**                  |


# Factoring and labelling skills proeficiency 
design_ind21$variables$AIndex_IDL <- factor(design_ind21$variables$AIndex_IDL,
                                            levels = c(0,1,2,9),                      
                                            labels =  c('None',
                                                        'Basic',
                                                        'Above basic',
                                                        'No internet use (last 3 months)'),
                                            ordered = T)

#The codes above are converting AIndex_IDL into an ordered factor, with custom labels for each score.

#The table below is showing what is happening in the code above

#| #Original Score | Label Assigned                      | Skill Meaning                               |
#| #-------------- | ----------------------------------- | ------------------------------------------- |
#  | `0`            | `'None'`                            | Used internet, but did **no digital tasks** |
#  | `1`            | `'Basic'`                           | Did **1 digital task**                      |
#  | `2`            | `'Above basic'`                     | Did **2 or more digital tasks**             |
#  | `9`            | `'No internet use (last 3 months)'` | Didn't use internet                         |

# B. Communication and collaboration
design_ind21$variables$AIndex_ICC<-ifelse((design_ind21$variables$ICC2+
                                             design_ind21$variables$ICC3+
                                             design_ind21$variables$ICC4+
                                             design_ind21$variables$ICC1)==0 & design_ind21$variables$C3==1,0,
                                          ifelse((design_ind21$variables$ICC2+
                                                    design_ind21$variables$ICC3+
                                                    design_ind21$variables$ICC4+
                                                    design_ind21$variables$ICC1)==1 & design_ind21$variables$C3==1,1,
                                                 ifelse((design_ind21$variables$ICC2+
                                                           design_ind21$variables$ICC3+
                                                           design_ind21$variables$ICC4+
                                                           design_ind21$variables$ICC1)>1 & design_ind21$variables$C3==1,2,
                                                        ifelse(design_ind21$variables$C3!=1,9,0))))

# Factoring and labelling skills proeficiency 
design_ind21$variables$AIndex_ICC <- factor(design_ind21$variables$AIndex_ICC,
                                            levels = c(0,1,2,9),                      
                                            labels =  c('None',
                                                        'Basic',
                                                        'Above basic',
                                                        'No internet use (last 3 months)'),
                                            ordered = T)

# C. Digital content creation
design_ind21$variables$AIndex_IDC<-ifelse((design_ind21$variables$IDC2+
                                             design_ind21$variables$IDC3+
                                             design_ind21$variables$IDC4+
                                             design_ind21$variables$IDC1+
                                             design_ind21$variables$IDC5+
                                             design_ind21$variables$IDC6)==0 & design_ind21$variables$C3==1,0,
                                          ifelse((design_ind21$variables$IDC2+
                                                    design_ind21$variables$IDC3+
                                                    design_ind21$variables$IDC4+
                                                    design_ind21$variables$IDC1+
                                                    design_ind21$variables$IDC5+
                                                    design_ind21$variables$IDC6)==1 & design_ind21$variables$C3==1,1,
                                                 ifelse((design_ind21$variables$IDC2+
                                                           design_ind21$variables$IDC3+
                                                           design_ind21$variables$IDC4+
                                                           design_ind21$variables$IDC1+
                                                           design_ind21$variables$IDC5+
                                                           design_ind21$variables$IDC6)>1 & design_ind21$variables$C3==1,2,
                                                        ifelse(design_ind21$variables$C3!=1,9,0))))

# Factoring and labelling skills proeficiency 
design_ind21$variables$AIndex_IDC <- factor(design_ind21$variables$AIndex_IDC,
                                            levels = c(0,1,2,9),                      
                                            labels =  c('None',
                                                        'Basic',
                                                        'Above basic',
                                                        'No internet use (last 3 months)'),
                                            ordered = T)

# D. Problem solving
design_ind21$variables$AIndex_IPL<-ifelse((design_ind21$variables$IPL2+
                                             design_ind21$variables$IPL3+
                                             design_ind21$variables$IPL4+
                                             design_ind21$variables$IPL1+
                                             design_ind21$variables$IPL5+
                                             design_ind21$variables$IPL6)==0 & design_ind21$variables$C3==1,0,
                                          ifelse((design_ind21$variables$IPL2+
                                                    design_ind21$variables$IPL3+
                                                    design_ind21$variables$IPL4+
                                                    design_ind21$variables$IPL1+
                                                    design_ind21$variables$IPL5+
                                                    design_ind21$variables$IPL6)==1 & design_ind21$variables$C3==1,1,
                                                 ifelse((design_ind21$variables$IPL2+
                                                           design_ind21$variables$IPL3+
                                                           design_ind21$variables$IPL4+
                                                           design_ind21$variables$IPL1+
                                                           design_ind21$variables$IPL5+
                                                           design_ind21$variables$IPL6)>1 & design_ind21$variables$C3==1,2,
                                                        ifelse(design_ind21$variables$C3!=1,9,0))))

# Factoring and labelling skills proeficiency 
design_ind21$variables$AIndex_IPL <- factor(design_ind21$variables$AIndex_IPL,
                                            levels = c(0,1,2,9),                      
                                            labels =  c('None',
                                                        'Basic',
                                                        'Above basic',
                                                        'No internet use (last 3 months)'),
                                            ordered = T)

# Labelling classes
design_ind21$variables<- apply_labels(design_ind21$variables,
                                      AIndex_IDL='Information and data literacy',
                                      AIndex_ICC='Communication and collaboration',
                                      AIndex_IDC='Digital content creation',
                                      AIndex_IPL='Problem solving')

#"Give friendly names to these skill index variables, 
#so reports, tables, and summaries show clear labels 
#instead of cryptic variable codes."


# Frequency tables for each class
svymean(~AIndex_IDL,design_ind21)
svymean(~AIndex_ICC,design_ind21)
svymean(~AIndex_IDC,design_ind21)
svymean(~AIndex_IPL,design_ind21)

#Each line calculates the survey-weighted mean (average) of a specific digital skills index 
#using your survey design object (design_ind21).

#svymean() does the following: 

#Computes weighted averages, 
#taking into account sampling design 
#(weights, strata, clusters, etc.).

#In this case, it’s computing the average skill level 
#(0 = None, 1 = Basic, 2 = Above Basic, etc.) for each skill domain.

# Calculating an overall skill level
#-------------------------------------------------------------------------------

design_ind21$variables$auxskill<-apply(design_ind21$variables[269:272],1,function(x) length(which(x=="None")))
#table(design_ind21$variables$auxskill)

#design_ind21$variables[269:272]
# 👉 Selects columns 269 to 272 in your dataset — these are assumed to be digital skill levels like AIndex_IDL, AIndex_ICC, etc.
# 
# apply(..., 1, ...)
# 👉 Tells R to apply a function row by row (because 1 means rows).
# 
# function(x) length(which(x == "None"))
# 👉 For each row, it looks at the values in columns 269–272 and counts how many of them are equal to "None".
# 
# design_ind21$variables$auxskill <-
#   👉 Saves the count of "None" values as a new column called auxskill

# Classifying skills for Brazil pilot
# As there was one less area collected, we went for a three classification scale for the overall skill:
# At least basic level of skills  => the respondent executes at least one activity in all the areas
# Skills in 3 of 4 areas          => the respondent executes at least one activity in 3 of the 4 areas
# Skills in 0 to 2 out of 4 areas => the respondent executes activities in 1 or 2 of the 4 areas 
#                                    or do not execute any of the listed activities, but is a Internet user

design_ind21$variables$Skill<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$auxskill==0,1,
                                     ifelse(design_ind21$variables$C3==1 & design_ind21$variables$auxskill==1,2,
                                            ifelse((design_ind21$variables$C3==1 & (design_ind21$variables$auxskill>1)),3,
                                                   ifelse(design_ind21$variables$C3!=1,9,0))))

#The code above can be explained as below: 

# | Condition                                             | Code Output | Meaning (after labeling)            |
#   | ----------------------------------------------------- | ----------- | ----------------------------------- |
#   | Internet user (`C3==1`) with **0 "None"** skill areas | `1`         | **At least basic level of skills**  |
#   | Internet user with **1 "None"** skill area            | `2`         | **Skills in 3 of 4 areas**          |
#   | Internet user with **2+ "None"** skill areas          | `3`         | **Skills in 0-2 out of 4 areas**    |
#   | Not an internet user (`C3 != 1`)                      | `9`         | **No internet use (last 3 months)** |


design_ind21$variables$Skill <- factor(design_ind21$variables$Skill,
                                       levels = c(1,2,3,9),                      
                                       labels =  c('At least basic level of skills',
                                                   'Skills in 3 of 4 areas',
                                                   'Skills in 0-2 out of 4 areas',
                                                   'No internet use (last 3 months)'),
                                       ordered = T)
#table(design_ind21$variables$Skill)

# Sector charts (pie charts) for skill classes

#-------------------------------------------------------------------------------
# Generating survey means objects for each Skills class
b <- svymean(~AIndex_IDL,design_ind21)
c <- svymean(~AIndex_ICC,design_ind21)
d <- svymean(~AIndex_IDC,design_ind21)
e <- svymean(~AIndex_IPL,design_ind21)

# Converting survey means in tibbles, extracting and formatting labels and means
b <- as_tibble(b) %>% 
  mutate(labels_cat = substring(names(b), 11),
         mean = formattable::percent(mean, digits = 1))
c <- as_tibble(c) %>% 
  mutate(labels_cat = substring(names(c), 11),
         mean = formattable::percent(mean, digits = 1))
d <- as_tibble(d) %>% 
  mutate(labels_cat = substring(names(d), 11),
         mean = formattable::percent(mean, digits = 1))
e <- as_tibble(e) %>% 
  mutate(labels_cat = substring(names(e), 11),
         mean = formattable::percent(mean, digits = 1))

# as_tibble(b):
# Converts the object b (which is a matrix or data frame of survey means) into a tibble — a modern, cleaner format for tables from the tidyverse.
# 
# substring(names(b), 11):
# Extracts the label name from the column names by removing the first 10 characters.
# Example:
#   
#   If a column name is "AIndex_IDLAbove basic", this would extract just "Above basic".
# 
# This helps clean up the factor level labels from the svymean output.
# 
# formattable::percent(mean, digits = 1): 
# Converts the mean values (like 0.42) into percentage format with 1 decimal place (e.g., "42.0%").

# | mean  | SE    | labels\_cat                     |
#   | ----- | ----- | ------------------------------- |
#   | 15.0% | 0.01  | None                            |
#   | 40.0% | 0.02  | Basic                           |
#   | 30.0% | 0.015 | Above basic                     |
#   | 15.0% | 0.01  | No internet use (last 3 months) |


ggplot2::theme_set(theme_bw() +
                     theme(axis.text.x=element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           panel.border = element_blank(),
                           panel.grid=element_blank(),
                           axis.ticks = element_blank(),
                           plot.title = element_text(hjust = "0.5",face = "bold", size=14),
                           strip.background = element_rect(fill = 'white'),
                           legend.position  = 'right')
)

# Generating charts for each skill class
b %>% 
  ggplot(aes(x = '', y = mean, fill = labels_cat)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar('y', start = 0) +
  geom_label(aes(label = mean,
                 group = factor(labels_cat)),
             fill = "white", colour = "black", 
             position= position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  labs(title = "Digital literacy",
       fill  = 'Skill level') 

c %>% 
  ggplot(aes(x = '', y = mean, fill = labels_cat)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar('y', start = 0) +
  geom_label(aes(label = mean,
                 group = factor(labels_cat)),
             fill = "white", colour = "black", 
             position= position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  labs(title = "Communication and collaboration",
       fill  = 'Skill level')

d %>% 
  ggplot(aes(x = '', y = mean, fill = labels_cat)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar('y', start = 0) +
  geom_label(aes(label = mean,
                 group = factor(labels_cat)),
             fill = "white", colour = "black", 
             position= position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_viridis_d() +
  labs(title = "Digital content creation",
       fill  = 'Skill level')

e %>% 
  ggplot(aes(x = '', y = mean, fill = labels_cat)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar('y', start = 0) +
  geom_label(aes(label = mean,
                 group = factor(labels_cat)),
             fill = "white", colour = "black", 
             position= position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_viridis_d() +
  labs(title = "Problem solving",
       fill  = 'Skill level')

# Generating survey mean objects for overall skill indicator
a <- svymean(~Skill, design_ind21)


# Converting survey mean in tibble, extracting and formatting labels and mean
a <- as_tibble(a) %>% 
  mutate(labels_cat = substring(names(a), 6),
         mean = formattable::percent(mean, digits = 1))

# Generating chart for overall skill indicator
a %>% 
  ggplot(aes(x = '', y = mean, fill = labels_cat)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar('y', start = 0) +
  geom_label(aes(label = mean,
                 group = factor(labels_cat)),
             fill = "white", colour = "black", 
             position= position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_viridis_d() +
  labs(title = "Overall Skill",
       fill  = 'Skill level')


# Barplot for skill classes by Workforce status

#-------------------------------------------------------------------------------
# Labeling workforce status categories and variable
design_ind21$variables$PEA_2 <- factor(design_ind21$variables$PEA_2,
                                       levels = c(1,2),                      
                                       labels =  c('In the workforce',
                                                   'Out of workforce'),
                                       ordered = T)
design_ind21$variables<- apply_labels(design_ind21$variables,
                                      PEA_2='Workforce status')

# The code above can be explained as below:
#   
#  It Converts PEA_2 (likely a numeric variable like 1 or 2) into an ordered factor.
# 
# Assigns human-readable labels:
#   
#   1 becomes "In the workforce"
# 
#   2 becomes "Out of workforce"


# Getting mean of overall skill indicator by workforce condition
f <- svyby(~Skill,~PEA_2, design_ind21, svymean) %>% 
  pivot_longer(cols = -PEA_2,
               values_to = 'values',
               names_to = 'measure') %>% 
  filter(!grepl('se.Skill', measure)) %>% 
  mutate(measure = substring(measure, 6),
         measure = factor(measure, 
                          levels =  c('At least basic level of skills',
                                      'Skills in 3 of 4 areas',
                                      'Skills in 0-2 out of 4 areas',
                                      'No internet use (last 3 months)'),
                          ordered = T),
         values = formattable::percent(values, 1))

# The codes above can be seen as: 
#   
#   | Workforce Group      | Skill Level                     | % of People |
#   | -------------------- | ------------------------------- | ----------- |
#   | In the workforce     | At least basic skills           | 45.1%       |
#   | In the workforce     | Skills in 3 of 4 areas          | 33.2%       |
#   | Out of the workforce | Skills in 0–2 out of 4 areas    | 12.5%       |
#   | Out of the workforce | No internet use (last 3 months) | 9.2%        |


f %>% 
  mutate(PEA_2 = forcats::fct_reorder(PEA_2, values, .desc = F)) %>% 
  ggplot(aes(y = values, 
             fill = PEA_2,
             x = measure
  )) +
  geom_bar(position='dodge', stat='identity')+ 
  geom_label(aes(label = values,
                 group = PEA_2),
             fill = "white", colour = "black", 
             position= position_dodge(width = .9)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Overall Skill x Workforce status',
       x = 'Overall Skill',
       y = 'Distribution (%)',
       fill = 'Workforce status') +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(plot.title = element_text(hjust = "0.5",face = "bold", size=14),
        strip.background = element_rect(fill = 'white'),
        legend.position  = 'right')
