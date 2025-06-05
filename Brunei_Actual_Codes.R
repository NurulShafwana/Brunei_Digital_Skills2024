# Install packages and load the packages to be used
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

bruneidesign_ind24$variables$CC1 <- ifelse(bruneidesign_ind24$variables$CC1 == 1, 1, 0)
bruneidesign_ind24$variables$CC2 <- ifelse(bruneidesign_ind24$variables$CC2 == 1, 1, 0)
bruneidesign_ind24$variables$CC3 <- ifelse(bruneidesign_ind24$variables$CC3 == 1, 1, 0)
bruneidesign_ind24$variables$CC4 <- ifelse(bruneidesign_ind24$variables$CC4 == 1, 1, 0)

bruneidesign_ind24$variables$CC <- bruneidesign_ind24$variables$CC1 +
  bruneidesign_ind24$variables$CC2 +
  bruneidesign_ind24$variables$CC3 +
  bruneidesign_ind24$variables$CC4


# B. Digital content creation
###   Digital Skill                                            Database variable

## 1. Creating electronic presentations 	                           DCC1
## 2. Writing a computer program 	                                   DCC2
## 3. Using basic arithmetic formula in a spreadsheet 	             DCC3
## 4. Using copy and paste tools 	                                   DCC4
## 5. Uploading self/user-created content	                           DCC5
## 6. Using editing software/application over the internet	         DCC6

bruneidesign_ind24$variables$DCC1 <- ifelse(bruneidesign_ind24$variables$DCC1 == 1, 1, 0)
bruneidesign_ind24$variables$DCC2 <- ifelse(bruneidesign_ind24$variables$DCC2 == 1, 1, 0)
bruneidesign_ind24$variables$DCC3 <- ifelse(bruneidesign_ind24$variables$DCC3 == 1, 1, 0)
bruneidesign_ind24$variables$DCC4 <- ifelse(bruneidesign_ind24$variables$DCC4 == 1, 1, 0)
bruneidesign_ind24$variables$DCC5 <- ifelse(bruneidesign_ind24$variables$DCC5 == 1, 1, 0)
bruneidesign_ind24$variables$DCC6 <- ifelse(bruneidesign_ind24$variables$DCC6 == 1, 1, 0)

bruneidesign_ind24$variables$DCC <- bruneidesign_ind24$variables$DCC1 +
  bruneidesign_ind24$variables$DCC2 +
  bruneidesign_ind24$variables$DCC3 +
  bruneidesign_ind24$variables$DCC4 +
  bruneidesign_ind24$variables$DCC5 +
  bruneidesign_ind24$variables$DCC6
  

# C. Information and Data Literacy
###   Digital Skill                                               Database variable

## 1. Reading or downloading newspapers, magazines or books	            IDL1    
## 2. Getting information about goods or services	                      IDL2
## 3. Seeking health information	                                      IDL3
## 4. Verifying the reliability of information found online	            IDL4

bruneidesign_ind24$variables$IDL1 <- ifelse(bruneidesign_ind24$variables$IDL1 == 1, 1, 0)
bruneidesign_ind24$variables$IDL2 <- ifelse(bruneidesign_ind24$variables$IDL2 == 1, 1, 0)
bruneidesign_ind24$variables$IDL3 <- ifelse(bruneidesign_ind24$variables$IDL3 == 1, 1, 0)
bruneidesign_ind24$variables$IDL4 <- ifelse(bruneidesign_ind24$variables$IDL4 == 1, 1, 0)

bruneidesign_ind24$variables$IDL <- bruneidesign_ind24$variables$IDL1 +
  bruneidesign_ind24$variables$IDL2 +
  bruneidesign_ind24$variables$IDL3 +
  bruneidesign_ind24$variables$IDL4

#to check
#table(bruneidesign_ind24$variables$IDL)


# D. Problem Solving
###   Digital Skill                                                      Database variable

## 1. Finding, downloading, installing and configuring software & apps 	      PS1
## 2. Transferring files or applications between devices 	                    PS2
## 3. Electronic financial transactions (e.g. internet banking)	              PS3
## 4. Purchasing or ordering goods or services (online)	                      PS4
## 5. Doing an online course	                                                PS5
## 6. Connecting and installing new devices	                                  PS6

bruneidesign_ind24$variables$PS1 <- ifelse(bruneidesign_ind24$variables$PS1 == 1, 1, 0)
bruneidesign_ind24$variables$PS2 <- ifelse(bruneidesign_ind24$variables$PS2 == 1, 1, 0)
bruneidesign_ind24$variables$PS3 <- ifelse(bruneidesign_ind24$variables$PS3 == 1, 1, 0)
bruneidesign_ind24$variables$PS4 <- ifelse(bruneidesign_ind24$variables$PS4 == 1, 1, 0)
bruneidesign_ind24$variables$PS5 <- ifelse(bruneidesign_ind24$variables$PS5 == 1, 1, 0)
bruneidesign_ind24$variables$PS6 <- ifelse(bruneidesign_ind24$variables$PS6 == 1, 1, 0)

bruneidesign_ind24$variables$PS <- bruneidesign_ind24$variables$PS1 +
  bruneidesign_ind24$variables$PS2 +
  bruneidesign_ind24$variables$PS3 +
  bruneidesign_ind24$variables$PS4 +
  bruneidesign_ind24$variables$PS5 +
  bruneidesign_ind24$variables$PS6

# E. Safety
###   Digital Skill                                                           Database variable

## 1. Changing privacy setting on device, account or app 	                          SFY1
## 2. Setting up effective security measures to protect devices & accounts 	        SFY2


bruneidesign_ind24$variables$SFY1 <- ifelse(bruneidesign_ind24$variables$SFY1 == 1, 1, 0)
bruneidesign_ind24$variables$SFY2 <- ifelse(bruneidesign_ind24$variables$SFY2 == 1, 1, 0)

bruneidesign_ind24$variables$SFY <- bruneidesign_ind24$variables$SFY1 +
  bruneidesign_ind24$variables$SFY2 
  


#IDL2: person got info about goods/services online
#IDL3: person read news online
#IDL4: person searched health info online
#Each is 1 if yes, 0 if no.
#You also have C3, which likely means: "Did the person use the internet?" (1 = yes)




#===============================================================================
# Calculating the Skills by areas by levels
#-------------------------------------------------------------------------------

### A. CC - Communication and collaboration
bruneidesign_ind24$variables$AIndex_CC<-ifelse((bruneidesign_ind24$variables$CC2+
                                             bruneidesign_ind24$variables$CC3+
                                             bruneidesign_ind24$variables$CC4+
                                             bruneidesign_ind24$variables$CC1)==0,0,
                                          ifelse((bruneidesign_ind24$variables$CC2+
                                                    bruneidesign_ind24$variables$CC3+
                                                    bruneidesign_ind24$variables$CC4+
                                                    bruneidesign_ind24$variables$CC1)==1,1,
                                                 ifelse((bruneidesign_ind24$variables$CC2+
                                                           bruneidesign_ind24$variables$CC3+
                                                           bruneidesign_ind24$variables$CC4+
                                                           bruneidesign_ind24$variables$CC1)>=2,2,0)))

# Factoring and labelling skills proeficiency 
bruneidesign_ind24$variables$AIndex_CC <- factor(bruneidesign_ind24$variables$AIndex_CC,
                                            levels = c(0,1,2),                      
                                            labels =  c('None',
                                                        'Basic',
                                                        'Above basic'),
                                            ordered = T)

### B. DCC - Digital content creation
bruneidesign_ind24$variables$AIndex_DCC<-ifelse((bruneidesign_ind24$variables$DCC2+
                                             bruneidesign_ind24$variables$DCC3+
                                             bruneidesign_ind24$variables$DCC4+
                                             bruneidesign_ind24$variables$DCC1+
                                             bruneidesign_ind24$variables$DCC5+
                                             bruneidesign_ind24$variables$DCC6)==0,0,
                                          ifelse((bruneidesign_ind24$variables$DCC2+
                                                    bruneidesign_ind24$variables$DCC3+
                                                    bruneidesign_ind24$variables$DCC4+
                                                    bruneidesign_ind24$variables$DCC1+
                                                    bruneidesign_ind24$variables$DCC5+
                                                    bruneidesign_ind24$variables$DCC6)==1,1,
                                                 ifelse((bruneidesign_ind24$variables$DCC2+
                                                           bruneidesign_ind24$variables$DCC3+
                                                           bruneidesign_ind24$variables$DCC4+
                                                           bruneidesign_ind24$variables$DCC1+
                                                           bruneidesign_ind24$variables$DCC5+
                                                           bruneidesign_ind24$variables$DCC6)>1,2,0)))

# Factoring and labelling skills proeficiency 
bruneidesign_ind24$variables$AIndex_DCC <- factor(bruneidesign_ind24$variables$AIndex_DCC,
                                            levels = c(0,1,2),                      
                                            labels =  c('None',
                                                        'Basic',
                                                        'Above basic'),
                                            ordered = T)

# C. IDL - Information and data literacy
bruneidesign_ind24$variables$AIndex_IDL<-ifelse((bruneidesign_ind24$variables$IDL2+
                                             bruneidesign_ind24$variables$IDL3+
                                             bruneidesign_ind24$variables$IDL4+
                                               bruneidesign_ind24$variables$IDL1)==0,0,
                                          ifelse((bruneidesign_ind24$variables$IDL2+
                                                    bruneidesign_ind24$variables$IDL3+
                                                    bruneidesign_ind24$variables$IDL4+
                                                  bruneidesign_ind24$variables$IDL1)==1,1,
                                                 ifelse((bruneidesign_ind24$variables$IDL2+
                                                           bruneidesign_ind24$variables$IDL3+
                                                           bruneidesign_ind24$variables$IDL4+
                                                           bruneidesign_ind24$variables$IDL1)>=2,2,0
                                                        )))

# Factoring and labelling skills proeficiency 
bruneidesign_ind24$variables$AIndex_IDL <- factor(bruneidesign_ind24$variables$AIndex_IDL,
                                            levels = c(0,1,2),                      
                                            labels =  c('None',
                                                        'Basic',
                                                        'Above basic'),
                                            ordered = T)

### D. PS - Problem solving
bruneidesign_ind24$variables$AIndex_PS<-ifelse((bruneidesign_ind24$variables$PS2+
                                             bruneidesign_ind24$variables$PS3+
                                             bruneidesign_ind24$variables$PS4+
                                             bruneidesign_ind24$variables$PS1+
                                             bruneidesign_ind24$variables$PS5+
                                             bruneidesign_ind24$variables$PS6)==0,0,
                                          ifelse((bruneidesign_ind24$variables$PS2+
                                                    bruneidesign_ind24$variables$PS3+
                                                    bruneidesign_ind24$variables$PS4+
                                                    bruneidesign_ind24$variables$PS1+
                                                    bruneidesign_ind24$variables$PS5+
                                                    bruneidesign_ind24$variables$PS6)==1,1,
                                                 ifelse((bruneidesign_ind24$variables$PS2+
                                                           bruneidesign_ind24$variables$PS3+
                                                           bruneidesign_ind24$variables$PS4+
                                                           bruneidesign_ind24$variables$PS1+
                                                           bruneidesign_ind24$variables$PS5+
                                                           bruneidesign_ind24$variables$PS6)>=2,2,0
                                                        )))

# Factoring and labelling skills proeficiency 
bruneidesign_ind24$variables$AIndex_PS <- factor(bruneidesign_ind24$variables$AIndex_PS,
                                            levels = c(0,1,2),                      
                                            labels =  c('None',
                                                        'Basic',
                                                        'Above basic'
                                                        ),
                                            ordered = T)

### E. SFY - Safety
bruneidesign_ind24$variables$AIndex_SFY<-ifelse((bruneidesign_ind24$variables$SFY2+
                                             bruneidesign_ind24$variables$SFY1)==0,0,
                                          ifelse((bruneidesign_ind24$variables$SFY2+
                                                    bruneidesign_ind24$variables$SFY1)==1,1,
                                                 ifelse((bruneidesign_ind24$variables$SFY2+
                                                           bruneidesign_ind24$variables$SFY1)>=2,2,0
                                                      )))

# Factoring and labelling skills proeficiency 
bruneidesign_ind24$variables$AIndex_SFY <- factor(bruneidesign_ind24$variables$AIndex_SFY,
                                            levels = c(0,1,2),                      
                                            labels =  c('None',
                                                        'Basic',
                                                        'Above basic'
                                                      ),
                                            ordered = T)

# Labelling classes FOR THE WHOLE DIGITAL SKILLS
bruneidesign_ind24$variables<- apply_labels(bruneidesign_ind24$variables,
                                            AIndex_CC='Communication and collaboration',
                                            AIndex_DCC='Digital content creation',
                                            AIndex_IDL='Information & data literacy',
                                            AIndex_PS='Problem solving',
                                            AIndex_SFY="Safety")


# Frequency tables for each class
svymean(~AIndex_CC,bruneidesign_ind24)
svymean(~AIndex_DCC,bruneidesign_ind24)
svymean(~AIndex_IDL,bruneidesign_ind24)
svymean(~AIndex_PS,bruneidesign_ind24)
svymean(~AIndex_SFY,bruneidesign_ind24)


# ### Calculating an overall skill level ### #
#-------------------------------------------------------------------------------

bruneidesign_ind24$variables$auxskill<-apply(bruneidesign_ind24$variables[, c("AIndex_CC", "AIndex_DCC", "AIndex_IDL", "AIndex_PS", "AIndex_SFY")],
                                             1,
                                             function(x) length(which(x=="None")))
#table(bruneidesign_ind24$variables$auxskill)

bruneidesign_ind24$variables$Skill<-ifelse(bruneidesign_ind24$variables$auxskill==0,1,
                                           ifelse(bruneidesign_ind24$variables$auxskill==1,2,
                                                  ifelse(((bruneidesign_ind24$variables$auxskill>1)),3,0
                                                  )))


bruneidesign_ind24$variables$Skill <- factor(bruneidesign_ind24$variables$Skill,
                                       levels = c(1,2,3),                      
                                       labels =  c('At least basic level of skills',
                                                   'Skills in 3 of 4 areas',
                                                   'Skills in 0-2 out of 4 areas'
                                                   ),
                                       ordered = T)
#table(bruneidesign_ind24$variables$Skill)

# Sector charts (pie charts) for skill classes

#-------------------------------------------------------------------------------
# Generating survey means objects for each Skills class
b <- svymean(~AIndex_CC,bruneidesign_ind24)
c <- svymean(~AIndex_DCC,bruneidesign_ind24)
d <- svymean(~AIndex_IDL,bruneidesign_ind24)
e <- svymean(~AIndex_PS,bruneidesign_ind24)
f <- svymean(~AIndex_SFY,bruneidesign_ind24)

# Converting survey means in tibbles, extracting and formatting labels and means
# mutate(labels_cat = substring(names(b), 10)) means extracting the 10th letter from AIndex_CC
# mutate(labels_cat = substring(names(b), 10)) means extracting the 11th letter from AIndex_DCC
b <- as_tibble(b) %>% 
  mutate(labels_cat = substring(names(b), 10),
         mean = formattable::percent(mean, digits = 1))
c <- as_tibble(c) %>% 
  mutate(labels_cat = substring(names(c), 11),
         mean = formattable::percent(mean, digits = 1))
d <- as_tibble(d) %>% 
  mutate(labels_cat = substring(names(d), 11),
         mean = formattable::percent(mean, digits = 1))
e <- as_tibble(e) %>% 
  mutate(labels_cat = substring(names(e), 10),
         mean = formattable::percent(mean, digits = 1))
f <- as_tibble(f) %>% 
  mutate(labels_cat = substring(names(f), 11),
         mean = formattable::percent(mean, digits = 1))

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
## b for CC
b %>% 
  ggplot(aes(x = '', y = mean, fill = labels_cat)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar('y', start = 0) +
  geom_label(aes(label = mean,
                 group = factor(labels_cat)),
             fill = "white", colour = "black", 
             position= position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    name = "Skill level",
    values = c(
      "None" = "#FFB6C1",         
      "Basic" = "#C7DBFF",       
      "Above basic" = "#B1E5D3"   
    )
  )+
  labs(title = "Communication & Collaboration",
       fill  = 'Skill level') 

## c is for DCC
c %>% 
  ggplot(aes(x = '', y = mean, fill = labels_cat)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar('y', start = 0) +
  geom_label(aes(label = mean,
                 group = factor(labels_cat)),
             fill = "white", colour = "black", 
             position= position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    name = "Skill level",
    values = c(
      "None" = "#FFB6C1",         
      "Basic" = "#C7DBFF",       
      "Above basic" = "#B1E5D3"   
    )
  )+
  labs(title = "Digital Content Creation",
       fill  = 'Skill level')

## d is for IDL
d %>% 
  ggplot(aes(x = '', y = mean, fill = labels_cat)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar('y', start = 0) +
  geom_label(aes(label = mean,
                 group = factor(labels_cat)),
             fill = "white", colour = "black", 
             position= position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(
    name = "Skill level",
    values = c(
      "None" = "#FFB6C1",         
      "Basic" = "#C7DBFF",       
      "Above basic" = "#B1E5D3"   
    )
  )+
  labs(title = "Information and Data Literacy",
       fill  = 'Skill level')

## e is for PS
e %>% 
  ggplot(aes(x = '', y = mean, fill = labels_cat)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar('y', start = 0) +
  geom_label(aes(label = mean,
                 group = factor(labels_cat)),
             fill = "white", colour = "black", 
             position= position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(
    name = "Skill level",
    values = c(
      "None" = "#FFB6C1",         
      "Basic" = "#C7DBFF",       
      "Above basic" = "#B1E5D3"   
    )
  )+
  labs(title = "Problem solving",
       fill  = 'Skill level')

## f is for SFY
f %>% 
  ggplot(aes(x = '', y = mean, fill = labels_cat)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar('y', start = 0) +
  geom_label(aes(label = mean,
                 group = factor(labels_cat)),
             fill = "white", colour = "black", 
             position= position_fill(vjust = .5)) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(
    name = "Skill level",
    values = c(
      "None" = "#FFB6C1",         
      "Basic" = "#C7DBFF",       
      "Above basic" = "#B1E5D3"   
    )
  )+
  labs(title = "Safety",
       fill  = 'Skill level')

# Generating survey mean objects for overall skill indicator
a <- svymean(~Skill, bruneidesign_ind24)


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

#Include gender here, dont be sexist

bruneidesign_ind24$variables$PEA_2 <- factor(bruneidesign_ind24$variables$PEA_2,
                                       levels = c(1,2),                      
                                       labels =  c('In the workforce',
                                                   'Out of workforce'),
                                       ordered = T)
bruneidesign_ind24$variables<- apply_labels(bruneidesign_ind24$variables,
                                      PEA_2='Workforce status')


# Getting mean of overall skill indicator by workforce condition
f <- svyby(~Skill,~PEA_2, bruneidesign_ind24, svymean) %>% 
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
