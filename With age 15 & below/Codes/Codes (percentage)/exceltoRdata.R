# === These codes will turn Brunei Digital Skills 2024 excel file to Rdata === #
# ================= To easily access this data in Rstudio ==================== #
# ================= This only need to be run ONE TIME ======================== #

# Install packages
install.packages("readxl")    # readxl for extracting excel file
install.packages("survey")    # survey for turning data into survey form

# Load libraries to use the packages
library(readxl)
library(survey)

# Read excel file
mydata <- read_excel("With age 15 & below/Codes/Codes (%)/Brunei Digital Skills 2024.xlsx")

# Check if data is correct - not necessary to do
head(mydata)

# Removing one row from the excel data frame
mydata <- mydata[-1, -4]


# Creating survey design using the data
## I named the survey as bruneidesign_ind24
### | Term      | Formal Definition                          | Kids Version                                     |
### | --------- | ------------------------------------------ | ------------------------------------------------ |
### | `ids`     | Primary sampling unit                      | Which group you visited (e.g., classroom)        |
### | `strata`  | Population subgroups used before sampling  | Types of groups you made (e.g., city or village) |
### | `weights` | How many people each respondent represents | How much each kid’s answer counts                |

### In this case, we use ids (or cluster) as HH,
###                       strata as Gender,
###                       and no weights used.
bruneidesign_ind24 <- svydesign(ids = ~HH, strata = ~AREA, data = mydata,
                                nest = TRUE)

# Save the file as Rdata file
save(bruneidesign_ind24, file = "bruneidesign_ind24.Rdata")