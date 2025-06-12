# Install packages and load the packages to be used
if (!require("install.load")) install.packages("install.load")
install.load::install_load("haven",
                           "tidyverse",
                           "survey",
                           "openxlsx",
                           "MASS",
                           "labelled",
                           "readxl",
                           "expss",
                           "formattable")

### BEFORE running these codes, make sure the Rdata of bruneidesign_ind24 is already in Environment

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

#edit label from 'None' to 'Below Basic'

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
svytotal(~AIndex_CC,bruneidesign_ind24)
svytotal(~AIndex_DCC,bruneidesign_ind24)
svytotal(~AIndex_IDL,bruneidesign_ind24)
svytotal(~AIndex_PS,bruneidesign_ind24)
svytotal(~AIndex_SFY,bruneidesign_ind24)

# ============================================================================ #
# ### Calculating an overall skill level ### #
#-------------------------------------------------------------------------------

# Recode skill domain levels to numeric for logic processing:
# "None" = 0, "Basic" = 1, "Above basic" = 2

bruneidesign_ind24$variables <- bruneidesign_ind24$variables %>%
  mutate(across(starts_with("AIndex_"),
                ~ case_when(
                  . == "None" ~ 0,
                  . == "Basic" ~ 1,
                  . == "Above basic" ~ 2,
                  TRUE ~ NA_real_
                )))

# Now classify overall digital skill level per your logic
bruneidesign_ind24$variables$Skill <- apply(bruneidesign_ind24$variables[, c("AIndex_CC", "AIndex_DCC", "AIndex_IDL", "AIndex_PS", "AIndex_SFY")],
                                            1,
                                            function(row) {
                                              if (any(row == 0)) {
                                                return("None")
                                              } else if (all(row >= 2)) {
                                                return("Above basic")
                                              } else {
                                                return("Basic")
                                              }
                                            })

# Convert to factor with ordering
bruneidesign_ind24$variables$Skill <- factor(bruneidesign_ind24$variables$Skill,
                                             levels = c("None", "Basic", "Above basic"),
                                             ordered = TRUE)

# ============================================================================ #
# Sector charts (pie charts) for skill classes
#-------------------------------------------------------------------------------
bruneidesign_ind24$variables <- bruneidesign_ind24$variables %>%
  mutate(
    AIndex_CC_cat = factor(AIndex_CC,
                           levels = c(0, 1, 2),
                           labels = c("None", "Basic", "Above basic")),
    AIndex_DCC_cat = factor(AIndex_DCC,
                            levels = c(0, 1, 2),
                            labels = c("None", "Basic", "Above basic")),
    AIndex_IDL_cat = factor(AIndex_IDL,
                            levels = c(0, 1, 2),
                            labels = c("None", "Basic", "Above basic")),
    AIndex_PS_cat = factor(AIndex_PS,
                           levels = c(0, 1, 2),
                           labels = c("None", "Basic", "Above basic")),
    AIndex_SFY_cat = factor(AIndex_SFY,
                            levels = c(0, 1, 2),
                            labels = c("None", "Basic", "Above basic"))
  )

# # Generating survey means objects for each Skills class
# b <- svytotal(~AIndex_CC,bruneidesign_ind24)
# c <- svytotal(~AIndex_DCC,bruneidesign_ind24)
# d <- svytotal(~AIndex_IDL,bruneidesign_ind24)
# e <- svytotal(~AIndex_PS,bruneidesign_ind24)
# f <- svytotal(~AIndex_SFY,bruneidesign_ind24)
# svytotal cannot be used - due to non-numerics so need to change first

# Update with numeric indicators (0/1) for all categories
bruneidesign_ind24 <- update(bruneidesign_ind24,
                             # AIndex_CC
                             None_num_CC         = as.numeric(AIndex_CC_cat == "None"),
                             Basic_num_CC        = as.numeric(AIndex_CC_cat == "Basic"),
                             Above_basic_num_CC  = as.numeric(AIndex_CC_cat == "Above basic"),
                             
                             # AIndex_DCC
                             None_num_DCC         = as.numeric(AIndex_DCC_cat == "None"),
                             Basic_num_DCC        = as.numeric(AIndex_DCC_cat == "Basic"),
                             Above_basic_num_DCC  = as.numeric(AIndex_DCC_cat == "Above basic"),
                             
                             # AIndex_IDL
                             None_num_IDL         = as.numeric(AIndex_IDL_cat == "None"),
                             Basic_num_IDL        = as.numeric(AIndex_IDL_cat == "Basic"),
                             Above_basic_num_IDL  = as.numeric(AIndex_IDL_cat == "Above basic"),
                             
                             # AIndex_PS
                             None_num_PS         = as.numeric(AIndex_PS_cat == "None"),
                             Basic_num_PS        = as.numeric(AIndex_PS_cat == "Basic"),
                             Above_basic_num_PS  = as.numeric(AIndex_PS_cat == "Above basic"),
                             
                             # AIndex_SFY
                             None_num_SFY         = as.numeric(AIndex_SFY_cat == "None"),
                             Basic_num_SFY        = as.numeric(AIndex_SFY_cat == "Basic"),
                             Above_basic_num_SFY  = as.numeric(AIndex_SFY_cat == "Above basic")
)



# Calculate proportions (means) for each
b <- svytotal(~None_num_CC + Basic_num_CC + Above_basic_num_CC, bruneidesign_ind24)

c <- svytotal(~None_num_DCC + Basic_num_DCC + Above_basic_num_DCC, bruneidesign_ind24)

d <- svytotal(~None_num_IDL + Basic_num_IDL + Above_basic_num_IDL, bruneidesign_ind24)

e <- svytotal(~None_num_PS + Basic_num_PS + Above_basic_num_PS, bruneidesign_ind24)

f <- svytotal(~None_num_SFY + Basic_num_SFY + Above_basic_num_SFY, bruneidesign_ind24)


# Prepare tidy table for each
b_df <- data.frame(
  labels_cat = names(coef(b)),
  prop = coef(b),
  SE = sqrt(diag(vcov(b))))
b <- as_tibble(b_df) %>%
  mutate(
    labels_cat = case_when(
      grepl("None_num_CC", labels_cat) ~ "None",
      grepl("Basic_num_CC", labels_cat) ~ "Basic",
      grepl("Above_basic_num_CC", labels_cat) ~ "Above basic",
      TRUE ~ "Other"
    ),
    labels_cat = factor(labels_cat, levels = c("None", "Basic", "Above basic"))
  )

c_df <- data.frame(
  labels_cat = names(coef(c)),
  prop = coef(c),
  SE = sqrt(diag(vcov(c))))
c <- as_tibble(c_df) %>%
  mutate(
    labels_cat = case_when(
      grepl("None_num_DCC", labels_cat) ~ "None",
      grepl("Basic_num_DCC", labels_cat) ~ "Basic",
      grepl("Above_basic_num_DCC", labels_cat) ~ "Above basic",
      TRUE ~ "Other"
    ),
    labels_cat = factor(labels_cat, levels = c("None", "Basic", "Above basic"))
  )

d_df <- data.frame(
  labels_cat = names(coef(d)),
  prop = coef(d),
  SE = sqrt(diag(vcov(d))))
d <- as_tibble(d_df) %>%
  mutate(
    labels_cat = case_when(
      grepl("None_num_IDL", labels_cat) ~ "None",
      grepl("Basic_num_IDL", labels_cat) ~ "Basic",
      grepl("Above_basic_num_IDL", labels_cat) ~ "Above basic",
      TRUE ~ "Other"
    ),
    labels_cat = factor(labels_cat, levels = c("None", "Basic", "Above basic"))
  )

e_df <- data.frame(
  labels_cat = names(coef(e)),
  prop = coef(e),
  SE = sqrt(diag(vcov(e))))
e <- as_tibble(e_df) %>%
  mutate(
    labels_cat = case_when(
      grepl("None_num_PS", labels_cat) ~ "None",
      grepl("Basic_num_PS", labels_cat) ~ "Basic",
      grepl("Above_basic_num_PS", labels_cat) ~ "Above basic",
      TRUE ~ "Other"
    ),
    labels_cat = factor(labels_cat, levels = c("None", "Basic", "Above basic"))
  )

f_df <- data.frame(
  labels_cat = names(coef(f)),
  prop = coef(f),
  SE = sqrt(diag(vcov(f))))
f <- as_tibble(f_df) %>%
  mutate(
    labels_cat = case_when(
      grepl("None_num_SFY", labels_cat) ~ "None",
      grepl("Basic_num_SFY", labels_cat) ~ "Basic",
      grepl("Above_basic_num_SFY", labels_cat) ~ "Above basic",
      TRUE ~ "Other"
    ),
    labels_cat = factor(labels_cat, levels = c("None", "Basic", "Above basic"))
  )

# setting ggplot for the pie charts theme
ggplot2::theme_set(theme_bw() +
                     theme(axis.text.x=element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           panel.border = element_blank(),
                           panel.grid=element_blank(),
                           axis.ticks = element_blank(),
                           plot.title = element_text(hjust = "0.5",face = "bold", size=14),
                           strip.background = element_rect(fill = 'white'),
                           legend.position  = 'right'))

# Generating charts for each skill class
## b for CC
# b %>%
#   ggplot(aes(x = "", y = prop, fill = labels_cat)) +
#   geom_bar(stat = "identity", width = 1, color = "white") +
#   geom_label(aes(label = round(values), group = labels_cat),
#              fill = "white", colour = "black",
#              position = position_fill(vjust = 0.5)) +
#   scale_fill_manual(
#     name = "Skill level",
#     values = c("None" = "#FFB6C1", "Basic" = "#C7DBFF", "Above basic" = "#B1E5D3")
#   ) +
#   labs(title = "Communication & Collaboration")

b %>%
  ggplot(aes(x = "", y = prop, fill = labels_cat)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_label(aes(label = round(prop), group = labels_cat),
             fill = "white", colour = "black",
             position = position_fill(vjust = 0.5)) +  # This keeps label centered
  scale_fill_manual(
    name = "Skill level",
    values = c("None" = "#FFB6C1", "Basic" = "#C7DBFF", "Above basic" = "#B1E5D3")
  ) +
  labs(title = "Communication & Collaboration")




## c is for DCC
c %>%
  ggplot(aes(x = "", y = prop, fill = labels_cat)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_label(aes(label = round(prop), group = labels_cat),
             fill = "white", colour = "black",
             position = position_fill(vjust = 0.5)) +  # This keeps label centered
  scale_fill_manual(
    name = "Skill level",
    values = c("None" = "#FFB6C1", "Basic" = "#C7DBFF", "Above basic" = "#B1E5D3")
  ) +
  labs(title = "Digital Content Creation")


## d is for IDL
d %>%
  ggplot(aes(x = "", y = prop, fill = labels_cat)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_label(aes(label = round(prop), group = labels_cat),
             fill = "white", colour = "black",
             position = position_fill(vjust = 0.5)) +  # This keeps label centered
  scale_fill_manual(
    name = "Skill level",
    values = c("None" = "#FFB6C1", "Basic" = "#C7DBFF", "Above basic" = "#B1E5D3")
  ) +
  labs(title = "Information and Data Literacy")

## e is for PS
e %>%
  ggplot(aes(x = "", y = prop, fill = labels_cat)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_label(aes(label = round(prop), group = labels_cat),
             fill = "white", colour = "black",
             position = position_fill(vjust = 0.5)) +  # This keeps label centered
  scale_fill_manual(
    name = "Skill level",
    values = c("None" = "#FFB6C1", "Basic" = "#C7DBFF", "Above basic" = "#B1E5D3")
  ) +
  labs(title = "Problem Solving")

## f is for SFY
f %>%
  ggplot(aes(x = "", y = prop, fill = labels_cat)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_label(aes(label = round(prop), group = labels_cat),
             fill = "white", colour = "black",
             position = position_fill(vjust = 0.5)) +  # This keeps label centered
  scale_fill_manual(
    name = "Skill level",
    values = c("None" = "#FFB6C1", "Basic" = "#C7DBFF", "Above basic" = "#B1E5D3")
  ) +
  labs(title = "Safety")



# Combining all the bars into 1 graph
# Step 1: Add Skill Type column
b$SkillType <- "Communication & Collaboration"
c$SkillType <- "Digital Content Creation"
d$SkillType <- "Information & Data Literacy"
e$SkillType <- "Problem Solving"
f$SkillType <- "Safety"

# Step 2: Combine into one data frame
all_skills_df <- bind_rows(b, c, d, e, f)

# Step 3: Plot grouped bar chart
ggplot(all_skills_df, aes(x = SkillType, y = prop, fill = labels_cat)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(aes(label = round(prop)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(
    name = "Skill level",
    values = c("None" = "#FFB6C1", "Basic" = "#C7DBFF", "Above basic" = "#B1E5D3")
  ) +
  labs(
    title = "Overall Digital Skills 2024",
    x = "Digital Skills",
    y = "No. of Individuals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
           


# ============================================================================ #
# Barplot for skill classes by Genders
#-------------------------------------------------------------------------------
# Labeling workforce status categories and variable
# Include gender here, dont be sexist
bruneidesign_ind24$variables$GEN <- factor(bruneidesign_ind24$variables$GEN,
                                           levels = c(1, 2),
                                           labels = c("MALE", "FEMALE"),
                                           ordered = TRUE)
# Filter out NA in Skill if any
design_filtered <- subset(bruneidesign_ind24, !is.na(Skill))

# Create skill dummy indicators
# Reason is because in skills, they're non-numerics
design_filtered$variables <- design_filtered$variables %>%
  mutate(
    skill_below  = if_else(Skill == "None", 1, 0),
    skill_basic = if_else(Skill == "Basic", 1, 0),
    skill_above = if_else(Skill == "Above basic", 1, 0)
  )

# Compute skill means by gender
#-------------------------------------------------------------------------------

# Function to compute skill means by GEN
get_skill_means <- function(gen_value) {
  sub <- subset(design_filtered, GEN == gen_value)
  means <- svytotal(~skill_below + skill_basic + skill_above, sub)
  data.frame(
    GEN = gen_value,
    measure = names(means),
    values = coef(means)
  )
}

# Apply to both genders
df_male <- get_skill_means("MALE")
df_female <- get_skill_means("FEMALE")

# Combine and clean
g <- bind_rows(df_male, df_female) %>%
  mutate(
    measure = case_when(
      measure == "skill_below" ~ "None",
      measure == "skill_basic" ~ "Basic",
      measure == "skill_above" ~ "Above basic"
    ),
    measure = factor(measure,
                     levels = c("None",
                                "Basic",
                                "Above basic"),
                     ordered = TRUE)
  )

#-------------------------------------------------------------------------------
# Generating barplot
g %>% 
  mutate(GEN = forcats::fct_reorder(GEN, values, .desc = F)) %>% 
  ggplot(aes(y = values, 
             fill = GEN,
             x = measure
  )) +
  geom_bar(position='dodge', stat='identity')+ 
  geom_label(aes(label = round(values),
                 group = GEN),
             fill = "white", colour = "black", 
             position= position_dodge(width = .9)) +
  labs(title = 'Overall Skill x Gender',
       x = 'Overall Skill',
       y = 'Distribution (%)',
       fill = 'Gender') +
  scale_fill_manual(values = c("MALE" = "blue", 
                               "FEMALE" = "red")) +  # <-- Your custom colours
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        strip.background = element_rect(fill = 'white'),
        legend.position  = 'right')


# Barplot for skill classes by AREA
#-------------------------------------------------------------------------------
# Labeling workforce status categories and variable
# Include gender here, dont be sexist
bruneidesign_ind24$variables$AREA <- haven::as_factor(bruneidesign_ind24$variables$AREA)
bruneidesign_ind24$variables$AREA <- factor(bruneidesign_ind24$variables$AREA,
                                            levels = c(1, 2),
                                            labels = c("URBAN", "RURAL"),
                                            ordered = TRUE)
# Filter out NA in Skill if any
design_filtered <- subset(bruneidesign_ind24, !is.na(Skill))

# Create skill dummy indicators
# Reason is because in skills, they're non-numerics
design_filtered$variables <- design_filtered$variables %>%
  mutate(
    skill_below  = if_else(Skill == "None", 1, 0),
    skill_basic = if_else(Skill == "Basic", 1, 0),
    skill_above = if_else(Skill == "Above basic", 1, 0)
  )

# Compute skill means by AREA
#-------------------------------------------------------------------------------

# Function to compute skill means by AREA
get_skill_means <- function(urban_value) {
  sub <- subset(design_filtered, AREA == urban_value)
  means <- svytotal(~skill_below + skill_basic + skill_above, sub)
  data.frame(
    AREA = urban_value,
    measure = names(means),
    values = coef(means)
  )
}

# Apply to both AREAS
df_urban <- get_skill_means("URBAN")
df_rural <- get_skill_means("RURAL")

# Combine and clean
g <- bind_rows(df_urban, df_rural) %>%
  mutate(
    measure = case_when(
      measure == "skill_below" ~ "None",
      measure == "skill_basic" ~ "Basic",
      measure == "skill_above" ~ "Above basic"
    ),
    measure = factor(measure,
                     levels = c("None",
                                "Basic",
                                "Above basic"),
                     ordered = TRUE)
  )

#-------------------------------------------------------------------------------
# Generating barplot
g %>% 
  mutate(AREA = forcats::fct_reorder(AREA, values, .desc = F)) %>% 
  ggplot(aes(y = values, 
             fill = AREA,
             x = measure
  )) +
  geom_bar(position='dodge', stat='identity')+ 
  geom_label(aes(label = round(values),
                 group = AREA),
             fill = "white", colour = "black", 
             position= position_dodge(width = .9)) +
  labs(title = 'Overall Skill x Area',
       x = 'Overall Skill',
       y = 'Distribution (%)',
       fill = 'Area') +
  scale_fill_manual(values = c("URBAN" = "#C7DBFF", 
                               "RURAL" = "#FFB6C1")) +  # <-- Your custom colours
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        strip.background = element_rect(fill = 'white'),
        legend.position  = 'right')



#Combining both graphs (Overall Skill x Gender x Area)

design_filtered$variables <- design_filtered$variables %>%
  mutate(
    combo = paste(GEN, AREA, sep = " - "),
    skill_below  = if_else(Skill == "None", 1, 0),
    skill_basic = if_else(Skill == "Basic", 1, 0),
    skill_above = if_else(Skill == "Above basic", 1, 0)
  )

get_combo_means <- function(group_label) {
  sub <- subset(design_filtered, combo == group_label)
  means <- svytotal(~skill_below + skill_basic + skill_above, sub)
  data.frame(
    combo = group_label,
    measure = names(means),
    values = coef(means)
  )
}

combo_levels <- unique(design_filtered$variables$combo)
combo_data <- purrr::map_dfr(combo_levels, get_combo_means) %>%
  mutate(
    measure = case_when(
      measure == "skill_below" ~ "None",
      measure == "skill_basic" ~ "Basic",
      measure == "skill_above" ~ "Above basic"
    ),
    percent_label = scales::percent(values, accuracy = 1),
    measure = factor(measure,
                     levels = c("None",
                                "Basic",
                                "Above basic"),
                     ordered = TRUE)
  )

ggplot(combo_data, aes(x = measure, y = values, fill = combo)) +
  geom_col(position = "dodge") +
  geom_label(aes(label = round(values)), position = position_dodge(width = 0.9)) +
  scale_fill_manual(
    values = c(
      "MALE - URBAN"   = "#C6E2FF",
      "FEMALE - URBAN" = "#FFE4E1",
      "MALE - RURAL"   = "#E0FFFF",
      "FEMALE - RURAL" = "#FFB6C1"
    )
  ) +
  labs(title = "Overall Skill by Gender and Area",
       x = "Skill Level",
       y = "Percentage",
       fill = "Group") +
  theme_minimal()

# Barplot for skill classes by OCCUPATION
#-------------------------------------------------------------------------------
# Labeling workforce status categories and variable

bruneidesign_ind24$variables$OCC <- haven::as_factor(bruneidesign_ind24$variables$OCC)

bruneidesign_ind24$variables$OCC <- factor(as.numeric(bruneidesign_ind24$variables$OCC),
                                           levels = c(1:10, 0),
                                           labels = c("Manager",
                                                      "Professional",
                                                      "Technician and associate professional",
                                                      "Clerical support worker",
                                                      "Services and sales worker",
                                                      "Skilled agricultural, forestry and fishery worker",
                                                      "Craft and related trades worker",
                                                      "Plant/machine operators and assemblers",
                                                      "Elementary occupations",
                                                      "Armed forces",
                                                      "Blank/Not stated"),
                                           ordered = FALSE)


design_filtered_occ <- subset(design_filtered, !is.na(Skill) & OCC != "Blank/Not stated")


install.packages("srvyr")

library(srvyr)
library(ggplot2)

# Convert to srvyr object for tidy syntax
survey_tbl <- as_survey_design(design_filtered_occ)

# Summarize skill levels by Gender and Occupation
skill_occ_gender <- survey_tbl %>%
  group_by(GEN, OCC, Skill) %>%
  summarize(p = survey_mean(vartype = "ci", na.rm = TRUE)) %>%
  ungroup()


ggplot(skill_occ_gender, aes(x = Skill, y = p, fill = GEN)) +
  geom_col(position = "dodge") +
  facet_wrap(~ OCC, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("MALE" = "#C7DBFF", "FEMALE" = "#FFB6C1")) +
  labs(
    title = "Digital Skill Levels by Occupation and Gender",
    x = "Overall Digital Skill",
    y = "Percentage",
    fill = "Gender"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save plot to a variable called skill_plot
skill_plot <- ggplot(skill_occ_gender, aes(x = Skill, y = p, fill = GEN)) +
  geom_col(position = "dodge") +
  facet_wrap(~ OCC, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("MALE" = "#C7DBFF", "FEMALE" = "#FFB6C1")) +
  labs(
    title = "Digital Skill Levels by Occupation and Gender",
    x = "Overall Digital Skill",
    y = "Percentage",
    fill = "Gender"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================================ #
# Barplot for skill levels by age group
#-------------------------------------------------------------------------------
# Part 1 - skill x age groups
bruneidesign_ind24$variables$AGE <- haven::as_factor(bruneidesign_ind24$variables$AGE)
bruneidesign_ind24$variables$AGE <- factor(bruneidesign_ind24$variables$AGE,
                                           levels = c(1, 2, 3, 4),
                                           labels = c("15 below",
                                                      "15-24 years old",
                                                      "25-74 years old",
                                                      "75 above"),
                                           ordered = TRUE)
# Filter out NA in Skill if any
design_filtered <- subset(bruneidesign_ind24, !is.na(Skill))

# Create skill dummy indicators
# Reason is because in skills, they're non-numerics
design_filtered$variables <- design_filtered$variables %>%
  mutate(
    skill_below  = if_else(Skill == "None", 1, 0),
    skill_basic = if_else(Skill == "Basic", 1, 0),
    skill_above = if_else(Skill == "Above basic", 1, 0)
  )

# Compute skill means by age group
#-------------------------------------------------------------------------------
# Function to compute skill means by age
get_skill_means <- function(age_value) {
  sub <- subset(design_filtered, AGE == age_value)
  means <- svymean(~skill_below + skill_basic + skill_above, sub)
  data.frame(
    AGE = age_value,
    measure = names(means),
    values = coef(means)
  )
}

# Apply to both genders
df_1 <- get_skill_means("15 below")
df_2 <- get_skill_means("15-24 years old")
df_3 <- get_skill_means("25-74 years old")
df_4 <- get_skill_means("75 above")

# Combine and clean
g <- bind_rows(df_1, df_2, df_3, df_4) %>%
  mutate(
    measure = case_when(
      measure == "skill_below" ~ "None",
      measure == "skill_basic" ~ "Basic",
      measure == "skill_above" ~ "Above basic"
    ),
    values = formattable::percent(values, 1),
    measure = factor(measure,
                     levels = c("None",
                                "Basic",
                                "Above basic"),
                     ordered = TRUE)
  )

#-------------------------------------------------------------------------------
# Generating barplot
g %>% 
  mutate(AGE = forcats::fct_reorder(AGE, values, .desc = F)) %>% 
  ggplot(aes(y = values, 
             fill = AGE,
             x = measure
  )) +
  geom_bar(position='dodge', stat='identity')+ 
  geom_label(aes(label = values,
                 group = AGE),
             fill = "white", colour = "black", 
             position= position_dodge(width = .9)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Overall Skill x Age Group',
       x = 'Overall Skill',
       y = 'Distribution (%)',
       fill = 'Gender') +
  scale_fill_manual(values = c("15 below" = "blue", 
                               "15-24 years old" = "red",
                               "25-74 years old" = "yellow",
                               "75 above" = "green")) +  # <-- Your custom colours
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        strip.background = element_rect(fill = 'white'),
        legend.position  = 'right')


# ---------------------------------------------------------------------------- #
# Part 2 - combining skill x age groups & skill x genders
# since skill x genders have been done previously, so no need to make a new one
# Compute skill means by age group by genders
# Compute skill means by age group by gender
get_skill_means_age_gender <- function(age_value, gen_value) {
  sub <- subset(design_filtered, AGE == age_value & GEN == gen_value)
  means <- svymean(~skill_below + skill_basic + skill_above, sub)
  data.frame(
    AGE = age_value,
    GEN = gen_value,
    measure = names(means),
    values = coef(means)
  )
}

age_levels <- levels(design_filtered$variables$AGE)
gen_levels <- levels(design_filtered$variables$GEN)

df_age_gen <- purrr::cross_df(list(AGE = age_levels, GEN = gen_levels)) %>%
  purrr::pmap_dfr(~ get_skill_means_age_gender(..1, ..2))

g <- df_age_gen %>%
  mutate(
    measure = case_when(
      measure == "skill_below" ~ "None",
      measure == "skill_basic" ~ "Basic",
      measure == "skill_above" ~ "Above basic"
    ),
    values = formattable::percent(values, 1),
    measure = factor(measure, levels = c("None", "Basic", "Above basic"), ordered = TRUE),
    GEN = factor(GEN, levels = gen_levels, ordered = TRUE),
    AGE = factor(AGE, levels = age_levels, ordered = TRUE),
    AGE_GEN = interaction(AGE, GEN, sep = " - ")
  )
# plot
g %>%
  mutate(
    AGE = factor(AGE, levels = c("15 below", "15-24 years old", "25-74 years old", "75 above"), ordered = TRUE),
    measure = factor(measure, levels = c("None", "Basic", "Above basic"), ordered = TRUE)
  ) %>%
  ggplot(aes(x = measure, y = values, fill = AGE)) +
  geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
  geom_label(aes(label = values, group = AGE),
             position = position_dodge(width = 0.8),
             fill = "white", colour = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Overall Skill by Age Group and Gender",
       x = "Overall Skill",
       y = "Distribution (%)",
       fill = "Age Group") +
  scale_fill_manual(values = c(
    "15 below" = "blue",
    "15-24 years old" = "red",
    "25-74 years old" = "green",
    "75 above" = "yellow"
  )) +
  facet_wrap(~ GEN) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.background = element_rect(fill = "white"),
    legend.position = "right"
  )

# ============================================================================ #
# Barplot for overall skill by Education level and Gender
# Part 1 - skill x education level
bruneidesign_ind24$variables$EDU <- haven::as_factor(bruneidesign_ind24$variables$EDU)
bruneidesign_ind24$variables$EDU <- factor(bruneidesign_ind24$variables$EDU,
                                           levels = c(1, 2, 3, 4),
                                           labels = c("Primary education or lower",
                                                      "Lower secondary education",
                                                      "Upper secondary, technical or vocational",
                                                      "Tertiary or post tertiary"),
                                           ordered = TRUE)
# Filter out NA in Skill if any
design_filtered <- subset(bruneidesign_ind24, !is.na(Skill))

# Create skill dummy indicators
# Reason is because in skills, they're non-numerics
design_filtered$variables <- design_filtered$variables %>%
  mutate(
    skill_below  = if_else(Skill == "None", 1, 0),
    skill_basic = if_else(Skill == "Basic", 1, 0),
    skill_above = if_else(Skill == "Above basic", 1, 0)
  )
# Compute skill means by age education level
# Function to compute skill means by education level
get_skill_means <- function(edu_value) {
  sub <- subset(design_filtered, EDU == edu_value)
  means <- svymean(~skill_below + skill_basic + skill_above, sub)
  data.frame(
    EDU = edu_value,
    measure = names(means),
    values = coef(means)
  )
}

# Apply to all education levels
df_1 <- get_skill_means("Primary education or lower")
df_2 <- get_skill_means("Lower secondary education")
df_3 <- get_skill_means("Upper secondary, technical or vocational")
df_4 <- get_skill_means("Tertiary or post tertiary")

# Combine and clean
g <- bind_rows(df_1, df_2, df_3, df_4) %>%
  mutate(
    measure = case_when(
      measure == "skill_below" ~ "None",
      measure == "skill_basic" ~ "Basic",
      measure == "skill_above" ~ "Above basic"
    ),
    values = formattable::percent(values, 1),
    measure = factor(measure,
                     levels = c("None",
                                "Basic",
                                "Above basic"),
                     ordered = TRUE)
  )

#-------------------------------------------------------------------------------
# Generating barplot
g %>% 
  mutate(EDU = forcats::fct_reorder(EDU, values, .desc = F)) %>% 
  ggplot(aes(y = values, 
             fill = EDU,
             x = measure
  )) +
  geom_bar(position='dodge', stat='identity')+ 
  geom_label(aes(label = values,
                 group = EDU),
             fill = "white", colour = "black", 
             position= position_dodge(width = .9)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Overall Skill x Education Levels',
       x = 'Overall Skill',
       y = 'Distribution (%)',
       fill = 'Edu') +
  scale_fill_manual(values = c("Primary education or lower" = "blue", 
                               "Lower secondary education" = "red",
                               "Upper secondary, technical or vocational" = "yellow",
                               "Tertiary or post tertiary" = "green")) +  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        strip.background = element_rect(fill = 'white'),
        legend.position  = 'right')


# Part 2 - combining overall skill x genders and overall skill x education level
get_skill_means_edu_gender <- function(edu_value, gen_value) {
  sub <- subset(design_filtered, EDU == edu_value & GEN == gen_value)
  means <- svymean(~skill_below + skill_basic + skill_above, sub)
  data.frame(
    EDU = edu_value,
    GEN = gen_value,
    measure = names(means),
    values = coef(means)
  )
}

edu_levels <- levels(design_filtered$variables$EDU)
gen_levels <- levels(design_filtered$variables$GEN)

df_edu_gen <- purrr::cross_df(list(EDU = edu_levels, GEN = gen_levels)) %>%
  purrr::pmap_dfr(~ get_skill_means_edu_gender(..1, ..2))

g <- df_edu_gen %>%
  mutate(
    measure = case_when(
      measure == "skill_below" ~ "None",
      measure == "skill_basic" ~ "Basic",
      measure == "skill_above" ~ "Above basic"
    ),
    values = formattable::percent(values, 1),
    measure = factor(measure, levels = c("None", "Basic", "Above basic"), ordered = TRUE),
    GEN = factor(GEN, levels = gen_levels, ordered = TRUE),
    EDU = factor(EDU, levels = edu_levels, ordered = TRUE),
    EDU_GEN = interaction(EDU, GEN, sep = " - ")
  )
# plot
g %>%
  mutate(
    EDU = factor(EDU, levels = c("Primary education or lower",
                                 "Lower secondary education",
                                 "Upper secondary, technical or vocational",
                                 "Tertiary or post tertiary"), ordered = TRUE),
    measure = factor(measure, levels = c("None", "Basic", "Above basic"), ordered = TRUE)
  ) %>%
  ggplot(aes(x = measure, y = values, fill = EDU)) +
  geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
  geom_label(aes(label = values, group = EDU),
             position = position_dodge(width = 0.8),
             fill = "white", colour = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Overall Skill by Education levels and Gender",
       x = "Overall Skill",
       y = "Distribution (%)",
       fill = "Education levels") +
  scale_fill_manual(values = c(
    "Primary education or lower" = "blue",
    "Lower secondary education" = "red",
    "Upper secondary, technical or vocational" = "green",
    "Tertiary or post tertiary" = "yellow"
  )) +
  facet_wrap(~ GEN) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.background = element_rect(fill = "white"),
    legend.position = "right"
  )

# ============================================================================ #
# Barplot for overall skill by employment status and Gender
# Part 1 - skill x employment status
bruneidesign_ind24$variables$EMP <- haven::as_factor(bruneidesign_ind24$variables$EMP)
bruneidesign_ind24$variables$EMP <- factor(bruneidesign_ind24$variables$EMP,
                                           levels = c(1, 2, 3, 4, 5, 0),
                                           labels = c("Employee or paid apprentice/intern",
                                                      "Employer or self-employed or helping without pay in household/family business",
                                                      "Not employed but seeking jobs",
                                                      "Not employed and not looking for work",
                                                      "Studying",
                                                      "Studying (below 13 years old)"),
                                           ordered = TRUE)
# Filter out NA in Skill if any
design_filtered <- subset(bruneidesign_ind24, !is.na(Skill))

# Create skill dummy indicators
# Reason is because in skills, they're non-numerics
design_filtered$variables <- design_filtered$variables %>%
  mutate(
    skill_below  = if_else(Skill == "None", 1, 0),
    skill_basic = if_else(Skill == "Basic", 1, 0),
    skill_above = if_else(Skill == "Above basic", 1, 0)
  )
# Compute skill means by age education level
# Function to compute skill means by education level
get_skill_means <- function(emp_value) {
  sub <- subset(design_filtered, EMP == emp_value)
  means <- svymean(~skill_below + skill_basic + skill_above, sub)
  data.frame(
    EMP = emp_value,
    measure = names(means),
    values = coef(means)
  )
}

# Apply to all education levels
df_1 <- get_skill_means("Employee or paid apprentice/intern")
df_2 <- get_skill_means("Employer or self-employed or helping without pay in household/family business")
df_3 <- get_skill_means("Not employed but seeking jobs")
df_4 <- get_skill_means("Not employed and not looking for work")
df_5 <- get_skill_means("Studying")
df_0 <- get_skill_means("Studying (below 13 years old)")

# Combine and clean
g <- bind_rows(df_1, df_2, df_3, df_4, df_5, df_0) %>%
  mutate(
    measure = case_when(
      measure == "skill_below" ~ "None",
      measure == "skill_basic" ~ "Basic",
      measure == "skill_above" ~ "Above basic"
    ),
    values = formattable::percent(values, 1),
    measure = factor(measure,
                     levels = c("None",
                                "Basic",
                                "Above basic"),
                     ordered = TRUE)
  )

#-------------------------------------------------------------------------------
# Generating barplot
g %>% 
  mutate(EMP = forcats::fct_reorder(EMP, values, .desc = F)) %>% 
  ggplot(aes(y = values, 
             fill = EMP,
             x = measure
  )) +
  geom_bar(position='dodge', stat='identity')+ 
  geom_label(aes(label = values,
                 group = EMP),
             fill = "white", colour = "black", 
             position= position_dodge(width = .9)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Overall Skill x Employment status',
       x = 'Overall Skill',
       y = 'Distribution (%)',
       fill = 'Employment status') +
  scale_fill_manual(values = c("Employee or paid apprentice/intern" = "blue", 
                               "Employer or self-employed or helping without pay in household/family business" = "red",
                               "Not employed but seeking jobs" = "yellow",
                               "Not employed and not looking for work" = "green",
                               "Studying" = "pink",
                               "Studying (below 13 years old)" = "black")) +  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        strip.background = element_rect(fill = 'white'),
        legend.position  = 'right')


# Part 2 - combining overall skill x genders and overall skill x education level
get_skill_means_emp_gender <- function(emp_value, gen_value) {
  sub <- subset(design_filtered, EMP == emp_value & GEN == gen_value)
  means <- svymean(~skill_below + skill_basic + skill_above, sub)
  data.frame(
    EMP = emp_value,
    GEN = gen_value,
    measure = names(means),
    values = coef(means)
  )
}

emp_levels <- levels(design_filtered$variables$EMP)
gen_levels <- levels(design_filtered$variables$GEN)

df_emp_gen <- purrr::cross_df(list(EMP = emp_levels, GEN = gen_levels)) %>%
  purrr::pmap_dfr(~ get_skill_means_emp_gender(..1, ..2))

g <- df_emp_gen %>%
  mutate(
    measure = case_when(
      measure == "skill_below" ~ "None",
      measure == "skill_basic" ~ "Basic",
      measure == "skill_above" ~ "Above basic"
    ),
    values = formattable::percent(values, 1),
    measure = factor(measure, levels = c("None", "Basic", "Above basic"), ordered = TRUE),
    GEN = factor(GEN, levels = gen_levels, ordered = TRUE),
    EMP = factor(EMP, levels = emp_levels, ordered = TRUE),
    EMP_GEN = interaction(EMP, GEN, sep = " - ")
  )
# plot
g %>%
  mutate(
    EMP = factor(EMP, levels = c("Employee or paid apprentice/intern", 
                                 "Employer or self-employed or helping without pay in household/family business",
                                 "Not employed but seeking jobs",
                                 "Not employed and not looking for work",
                                 "Studying",
                                 "Studying (below 13 years old)"), ordered = TRUE),
    measure = factor(measure, levels = c("None", "Basic", "Above basic"), ordered = TRUE)
  ) %>%
  ggplot(aes(x = measure, y = values, fill = EMP)) +
  geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
  geom_label(aes(label = values, group = EMP),
             position = position_dodge(width = 0.8),
             fill = "white", colour = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Overall Skill by Employment status and Gender",
       x = "Overall Skill",
       y = "Distribution (%)",
       fill = "Employment status") +
  scale_fill_manual(values = c(
    "Employee or paid apprentice/intern" = "blue", 
    "Employer or self-employed or helping without pay in household/family business" = "red",
    "Not employed but seeking jobs" = "yellow",
    "Not employed and not looking for work" = "green",
    "Studying" = "pink",
    "Studying (below 13 years old)" = "black"
  )) +
  facet_wrap(~ GEN) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.background = element_rect(fill = "white"),
    legend.position = "right"
  )





