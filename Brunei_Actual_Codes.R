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
table(bruneidesign_ind24$variables$IDL)


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
# Calculating the Skills by areas
#-------------------------------------------------------------------------------

### A. CC - Communication and collaboration
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

### B. DCC - Digital content creation
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

# C. IDL - Information and data literacy
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

# Factoring and labelling skills proeficiency 
design_ind21$variables$AIndex_IDL <- factor(design_ind21$variables$AIndex_IDL,
                                            levels = c(0,1,2,9),                      
                                            labels =  c('None',
                                                        'Basic',
                                                        'Above basic',
                                                        'No internet use (last 3 months)'),
                                            ordered = T)

### D. PS - Problem solving
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

### E. SFY - Safety
design_ind21$variables$AIndex_IPL<-ifelse((design_ind21$variables$SFY2+
                                             design_ind21$variables$SFY3+
                                             design_ind21$variables$SFY4+
                                             design_ind21$variables$SFY1+
                                             design_ind21$variables$SFY5+
                                             design_ind21$variables$SFY6)==0 & design_ind21$variables$C3==1,0,
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

# Labelling classes FOR THE WHOLE DIGITAL SKILLS
design_ind21$variables<- apply_labels(design_ind21$variables,
                                      AIndex_IDL='Information and data literacy',
                                      AIndex_ICC='Communication and collaboration',
                                      AIndex_IDC='Digital content creation',
                                      AIndex_IPL='Problem solving',
                                      AIndex_SFY="Safety")


# Frequency tables for each class
svymean(~AIndex_IDL,design_ind21)
svymean(~AIndex_ICC,design_ind21)
svymean(~AIndex_IDC,design_ind21)
svymean(~AIndex_IPL,design_ind21)
svymean(~AIndex_SFY, bruneidesign_ind24)


# ### Calculating an overall skill level ### #
#-------------------------------------------------------------------------------

design_ind21$variables$auxskill<-apply(design_ind21$variables[269:272],1,function(x) length(which(x=="None")))
#table(design_ind21$variables$auxskill)


design_ind21$variables$Skill<-ifelse(design_ind21$variables$C3==1 & design_ind21$variables$auxskill==0,1,
                                     ifelse(design_ind21$variables$C3==1 & design_ind21$variables$auxskill==1,2,
                                            ifelse((design_ind21$variables$C3==1 & (design_ind21$variables$auxskill>1)),3,
                                                   ifelse(design_ind21$variables$C3!=1,9,0))))



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

#Include gender here, dont be sexist

design_ind21$variables$PEA_2 <- factor(design_ind21$variables$PEA_2,
                                       levels = c(1,2),                      
                                       labels =  c('In the workforce',
                                                   'Out of workforce'),
                                       ordered = T)
design_ind21$variables<- apply_labels(design_ind21$variables,
                                      PEA_2='Workforce status')


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
