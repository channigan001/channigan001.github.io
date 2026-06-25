##############################

# Author: Your Name
# Class: POLS Capstone
# Research Question: ?

##############################


# --------------------------------------------
# RENAME THIS FILE
# --------------------------------------------

  # Start by renaming this file using "Save As..." or "Save a copy"
  # following this naming convention: LastnameFirstname_CapstoneScript.R

# --------------------------------------------
# INSTALL PACKAGES / LOAD LIBRARIES
# --------------------------------------------

  # Uncomment (remove the #) if you need to install a package.
  # You only need to install a package once per computer.

# install.packages("RCPA3")
# install.packages("readxl")
# install.packages("haven")
# install.packages("dplyr")
# install.packages("stargazer")

  # Load the libraries each time you start a new R session.

library(RCPA3)
library(readxl)
library(haven)
library(dplyr)
library(stargazer)


# --------------------------------------------
# MAKE A FILEPATH OBJECT
# --------------------------------------------

  # Create or navigate to a OneDrive folder where you'll store
  # the raw data and this script.

  # Find the file path to that folder and replace
  # /Path/To/File/Here/ with your actual file path.

filepath <- "/Path/To/File/Here/"  # be sure to keep the quotes

  # PC users example:
  # "C:\\Users\\CHANNIGAN001\\OneDrive - CSBSJU\\CapstoneData\\"
  
  # Mac users example:
  # "/Users/CHANNIGAN001/OneDrive - CSBSJU/CapstoneData/"


# --------------------------------------------
# IMPORT / LOAD YOUR DATA
# --------------------------------------------

  # The names of the dataset files and the data objects you create
  # are customizable. The examples below are just starting points.

csvDataset <- read.csv(file = paste0(filepath, "datasetName.csv"))
excelDataset <- read_excel(path = paste0(filepath, "datasetName.xlsx"))
spssDataset <- read_sav(file = paste0(filepath, "datasetName.sav"))
stataDataset <- read_dta(file = paste0(filepath, "datasetName.dta"))

# RData files load objects directly into your workspace.

load(file = paste0(filepath, "datasetName.Rdata"))


# --------------------------------------------
# SAVE YOUR DATA AS AN RDATA FILE
# --------------------------------------------

# Replace csvDataset with the name of the data object you created.
# Then replace datasetName with the name you want to give
# the RData file.

save(csvDataset, file = paste0(filepath, "datasetName.Rdata"))


# --------------------------------------------
# CHANGING VARIABLE NAMES
# --------------------------------------------

  # 1. Using names()

names(datasetName)[names(datasetName) == "origVariableName"] <- "newVariableName"

  # 2. Create a duplicate variable with a new name

datasetName$newVariableName <- datasetName$origVariableName

  # 3. Using dplyr::rename()

datasetName <- datasetName %>%
  rename(
    newVariableName1 = origVariableName1,
    newVariableName2 = origVariableName2
  )


# --------------------------------------------
# REPLACING VARIABLE VALUES
# --------------------------------------------

  # Replace missing value codes with NA where appropriate.

datasetName$variableName[datasetName$variableName == -99] <- NA
datasetName$variableName[datasetName$variableName < 0] <- NA
datasetName$variableName[datasetName$variableName %in% c(-99, -88, -77, -66)] <- NA

  # Make a dummy variable.

datasetName$newDummyVariableName <- ifelse(  # if...
  is.na(datasetName$variableName),           # the original value is missing...
  NA,                                        # call it missing, 
  ifelse(                                    # otherwise, if...
    datasetName$variableName == 4,           # the value is exactly 4 (or whatever you put there)...
    1,                                       # call it 1,
    0                                        # otherwise, call it 0. 
  )                                          # close out most recent ifelse
)                                            # close out original ifelse

# Always be sure to preserve missing values from the original variable.


# --------------------------------------------
# FILTERING THE DATASET
# --------------------------------------------

  # Remember: dataset[rows, columns]
    # The first position selects rows.
    # The second position selects columns.
    # THE COMMA PLACEMENT IS CRUCIAL!

newDatasetName <- datasetName[is.na(datasetName$variableName),] # Only include rows where variableName is missing (NA).
newDatasetName <- datasetName[!is.na(datasetName$variableName),] # Remove rows where variableName is missing (NA).
newDatasetName <- datasetName[datasetName$variableName == 1,] # Only include rows where variableName is exactly equal to 1.
newDatasetName <- datasetName[datasetName$variableName != 1,] # Only include rows where variableName is NOT equal to 1.
newDatasetName <- datasetName[datasetName$variableName %in% c(1, 2),] # Only include rows where variableName is either 1 or 2.
newDatasetName <- datasetName[!datasetName$variableName %in% c(1, 2),] # Only include rows where variableName is NOT 1 or 2.
newDatasetName <- datasetName[datasetName$variableName %in% c("foo", "bar"),] # Same logic applies to strings; just add quotation marks.


# --------------------------------------------
# MERGING DATASETS ON COMMON ID VARIABLES
# --------------------------------------------

newDatasetName <- merge(
  datasetName1,
  datasetName2,
  by = "IDvariableName"
)

  # The unique ID variable should be exactly the same in both datasets.

newDatasetName <- merge(
  datasetName1,
  datasetName2,
  by = c("IDvariableName1", "IDvariableName2")
)

  # Sometimes your data is uniquely identified by multiple variables, such as state AND year.

newDatasetName <- merge(
  datasetName1,
  datasetName2,
  by = "IDvariableName",
  all.x = TRUE
)

  # Keep all rows from datasetName1, even if not matched in datasetName2.

newDatasetName <- merge(
  datasetName1,
  datasetName2,
  by = "IDvariableName",
  all.y = TRUE
)

  # Keep all rows from datasetName2, even if not matched in datasetName1.

newDatasetName <- merge(
  datasetName1,
  datasetName2,
  by = "IDvariableName",
  all = TRUE
)

  # Keep all rows from both datasets, even if not matched.



# --------------------------------------------
# CHANGING DATA TYPES
# --------------------------------------------

str(datasetName$newVariableName)  # check variable type

  # chr        = character (text/string)
  # num        = numeric
  # int        = integer (whole numbers)
  # Ord.factor = ordinal factor
  # Factor     = nominal or dichotomous factor

datasetName$newVariableName <- as.numeric(datasetName$variableName) # Make the variable numeric.
datasetName$newVariableName <- as.factor(datasetName$variableName) # Make the variable categorical.
datasetName$newVariableName <- as.ordered(datasetName$variableName) # Make the variable an ordinal factor.
datasetName$newVariableName <- factor(datasetName$variableName, ordered = F) # Change from ordinal to nominal factor.

# SPECIAL CASE: as_factor() versus as.factor()

datasetName$newVariableName <- as_factor(datasetName$variableName)

  # If your data come from SPSS or Stata, as_factor()
  # converts the variable to a factor and applies labels
  # (if labels exist).


# --------------------------------------------
# DESCRIPTIVE STATISTICS UTILITIES
# --------------------------------------------

freqC(datasetName$variableName) # Check the distribution of categories.
freqC(datasetName$variableName, plot = FALSE) # Turn off the plot (and annoying prompts in console)
describeC(datasetName$variableName) # Get descriptive statistics and check for missing values.
histC(datasetName$variableName) # View the distribution of a continuous variable.
View(datasetName) # View the dataset in a spreadsheet-style window.
head(datasetName) # View the first few rows of the dataset.

plot(x = datasetName$variableName, y = datasetName$variableName)


# --------------------------------------------
# INFERENTIAL STATISTICAL TESTS
# --------------------------------------------

  # T-TEST HELP

testmeansC(datasetName$variableName, 50) # One-sample t-test.

testmeansC(
  dv = datasetName$variableName,
  iv = datasetName$variableName,
  printC = TRUE
)  # Independent-samples t-test.

testmeansC(
  datasetName$variableName, 
  datasetName$variableName, 
  paired = TRUE) # Paired-samples t-test.


  # ANOVA HELP

compmeansC(
  datasetName$variableName,
  datasetName$variableName,
  anova = TRUE
) # Basic ANOVA model

anova.model1 <- aov(datasetName$variableName ~ datasetName$variableName) # Advanced ANOVA model.
summary(anova.model1)
TukeyHSD(anova.model1) # Tukey post-hoc test. Compare differences across categories.


  # CHI-SQUARE HELP

crosstabC(
  dv = datasetName$variableName,
  iv = datasetName$variableName,
  compact = TRUE,
  chisq = TRUE
) # Basic chi-square model. 

crosstabC(
  dv = datasetName$variableName,
  iv = datasetName$variableName,
  compact = TRUE,
  chisq = TRUE,
  plot.response = "all"
) # Create a clustered bar chart, too.

crosstabC(
  dv = datasetName$variableName,
  iv = datasetName$variableName,
  compact = TRUE,
  chisq = TRUE,
  ivlabs = c("label1", "label2", "label3")
) # Apply custom value labels to the IV. You can also use "dvlabs =" for the DV labels.


  # CORRELATION HELP

correlateC(c(datasetName$variableName, datasetName$variableName)) # Basic correlation matrix.
correlateC(c(datasetName$variableName, datasetName$variableName), plot = TRUE) # Include a scatterplot.


  # OLS LINEAR REGRESSION HELP

regC(
  formula = datasetName$variableName ~
    datasetName$variableName +
    datasetName$variableName
) # Basic linear regression model.

lm.model1 <- lm(
  datasetName$variableName ~
    datasetName$variableName +
    datasetName$variableName
) # Advanced linear regression model. Btter for stargazer() output.

summary(lm.model1)


  # LOGISTIC REGRESSION (Logit) HELP

logregC(
  formula = datasetName$variableName ~
    datasetName$variableName +
    datasetName$variableName
) # Basic logistic regression model.

logregC(
  formula = datasetName$variableName ~
    datasetName$variableName +
    datasetName$variableName,
  fit.stats = TRUE, # Add model p-values, pseudo R-squared, etc.
  pre = TRUE # Add classification table.
)

logit.model1 <- glm(
  formula = datasetName$variableName ~
    datasetName$variableName +
    datasetName$variableName,
  family = binomial
) # Advanced logistic regression model. Better for stargazer() output.

summary(logit.model1)


# --------------------------------------------
# PREPARING MODELS FOR PRESENTATION
# --------------------------------------------

  # Use stargazer() with lm() or glm() model objects.

stargazer(
  modelName,
  type = "text",
  star.cutoffs = c(0.05, 0.01, 0.001)
)
  # Also use ?stargazer for to documentation to explore other features,
  # like covariate.labels = c() and dep.var.labels = c() and df = FALSE

  # In Word, R output is best formatted using Lucida Console font (10pt size). 
  # Without that option, you could try the Courier New font. 

  # Or, you could try to output to an html file, open it in a browser, 
  # then copy/paste that in:

stargazer(
  modelName,
  type = "html",
  out = paste0(filepath, "outputFileName.html"),
  star.cutoffs = c(0.05, 0.01, 0.001)
)


  # Use printC = TRUE with many RCPA3 functions.

testmeansC(
  dv = datasetName$variableName,
  iv = datasetName$variableName,
  printC = TRUE
)

compmeansC(
  datasetName$variableName,
  datasetName$variableName,
  anova = TRUE,
  printC = TRUE
)

crosstabC(
  dv = datasetName$variableName,
  iv = datasetName$variableName,
  compact = TRUE,
  chisq = TRUE,
  plot.response = "all",
  printC = TRUE
)

regC(
  formula = datasetName$variableName ~
    datasetName$variableName +
    datasetName$variableName,
  printC = TRUE
) # stargazer output is preferable to printC output here.

logregC(
  formula = datasetName$variableName ~
    datasetName$variableName +
    datasetName$variableName,
  fit.stats = TRUE,
  printC = TRUE
) # stargazer output is preferable to printC output here.


  # Find and open the HTML file produced
  # (e.g., Table.Output.Nov1225.html) in a browser,
  # then take a screenshot of the relevant results.

