################################################################################
# Updated version of the code for the analysis in:
#
#   "Mortality risk attributable to high and low ambient temperature:
#     a multi-country study"
#   Antonio Gasparrini and collaborators
#   The Lancet - 2015
#   http://www.ag-myresearch.com/2015_gasparrini_lancet.html
#
# Update: 15 January 2017
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata
################################################################################

################################################################################
# PREPARE THE DATA
################################################################################
setwd('~/Desktop/GitHub/CDPH_heat_project/')
# LOAD THE PACKAGES
install.packages("dlnm") ; install.packages("mvmeta") ; install.packages("splines") ; install.packages("tsModel") ;
library(dlnm) ; library(mvmeta) ; library(splines) ; library(tsModel)

# CHECK VERSION OF THE PACKAGE
if(packageVersion("dlnm")<"2.2.0")
  stop("update dlnm package to version >= 2.2.0")

# LOAD THE DATASET (INCLUDING THE 10 UK REGIONS ONLY)
regEngWales <- read.csv("regEngWales.csv",row.names=1)
regEngWales$date <- as.Date(regEngWales$date)

# ARRANGE THE DATA AS A LIST OF DATA SETS
regions <- as.character(unique(regEngWales$regnames))
dlist <- lapply(regions,function(x) regEngWales[regEngWales$regnames==x,])
names(dlist) <- regions

# METADATA FOR LOCATIONS
cities <- data.frame(
  city = regions,
  cityname = c("North East","North West","Yorkshire & Humber","East Midlands",
               "West Midlands","East","London","South East","South West","Wales")
)

# ORDER
ord <- order(cities$cityname)
dlist <- dlist[ord]
cities <- cities[ord,]

# REMOVE ORIGINALS
rm(regEngWales,regions,ord)

################################################################################

# SPECIFICATION OF THE EXPOSURE FUNCTION
varfun = "bs"
vardegree = 2
varper <- c(10,75,90)

# SPECIFICATION OF THE LAG FUNCTION
lag <- 21
lagnk <- 3

# DEGREE OF FREEDOM FOR SEASONALITY
dfseas <- 8

# COMPUTE PERCENTILES
per <- t(sapply(dlist,function(x) 
  quantile(x$tmean,c(2.5,10,25,50,75,90,97.5)/100,na.rm=T)))

# MODEL FORMULA
formula <- death~cb+dow+ns(date,df=dfseas*length(unique(year)))

#
################################################################################
# Updated version of the code for the analysis in:
#
#   "Mortality risk attributable to high and low ambient temperature:
#     a multi-country study"
#   Antonio Gasparrini and collaborators
#   The Lancet - 2015
#   http://www.ag-myresearch.com/2015_gasparrini_lancet.html
#
# Update: 15 January 2017
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata
################################################################################

################################################################################
# FIRST-STAGE ANALYSIS: RUN THE MODEL IN EACH CITY, REDUCE AND SAVE
################################################################################

################################################################################
# CREATE THE OBJECTS TO STORE THE RESULTS

# COEFFICIENTS AND VCOV FOR OVERALL CUMULATIVE SUMMARY
coef <- matrix(NA,nrow(cities),length(varper)+vardegree,
               dimnames=list(cities$city))
vcov <- vector("list",nrow(cities))
names(vcov) <- cities$city

################################################################################
# RUN THE LOOP

# LOOP
time <- proc.time()[3]
for(i in seq(length(dlist))) {
  
  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  data <- dlist[[i]]
  
  # DEFINE THE CROSSBASIS
  argvar <- list(fun=varfun,knots=quantile(data$tmean,varper/100,na.rm=T),
                 degree=vardegree)
  cb <- crossbasis(data$tmean,lag=lag,argvar=argvar,
                   arglag=list(knots=logknots(lag,lagnk)))
  #summary(cb)
  
  # RUN THE MODEL AND OBTAIN PREDICTIONS
  # NB: NO CENTERING NEEDED HERE, AS THIS DOES NOT AFFECT COEF-VCOV
  model <- glm(formula,data,family=quasipoisson,na.action="na.exclude")
  cen <- mean(data$tmean,na.rm=T)
  pred <- crosspred(cb,model,cen=cen)
  
  # REDUCTION TO OVERALL CUMULATIVE
  red <- crossreduce(cb,model,cen=cen)
  coef[i,] <- coef(red)
  vcov[[i]] <- vcov(red)
  
}
proc.time()[3]-time

#