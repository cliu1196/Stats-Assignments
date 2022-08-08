### Clean Up Data:
# get the "read.dta" function
# read in all the data
brdata <- read.dta("http://www.stat.ucla.edu/~handcock/216/examples/nes/nes5200_processed_voters_realideo.dta",
                   convert.factors=F,convert.underscore=T)
# A 'dta' file is a file from the statistics package "stata".
# when R reads a 'dta' file some extra information about the data
# is also imported into R
# First, look at the structure of the data:
str(brdata)
# There's a lot of stuff here, but notice that there are basically
# two sections:
# -- the list of variables (the lines beginning with "$")
# -- the list of attributes (the lines beginning with "-")
#
# The attributes contain additional information about the variables,
# from stata in this case.
#
# For example here is a brief glossary of the variable names/definitions:
cbind("description"=attr(brdata, "var.labels"),
      "type"=apply(brdata,2,class))
# Note that all of the variables are numeric.  However, with
# the exception of a few, such as "age", "age.sq", etc,
# they all only have a few unique values, suggesting that they are
# really categorical/factor variables:
#apply(brdata,2,unique)
# With a little bit of work we can get the definitions of all of the
# categorical variables:
for (i in 1:dim(brdata)[2]) {
  R.name <- names(brdata)[i]
  stata.name <- attr(brdata,"val.labels")[i]
  stata.definition <- attr(brdata,"var.labels")[i]
  if (stata.name != "") {
    cat(R.name," (",stata.definition,"):\n",sep="")
    print(attr(brdata,"label.table")[[stata.name]])
  }
  else {
    R.def <- ifelse(stata.definition!="",stata.definition,"No definition given")
    cat(R.name," (",R.def,")\n",sep="")
    print(summary(brdata[[R.name]]))
  }
  cat("\n\n")
}
# If that seems messy, we can also make a function that will give us
# the definition of any variable we specify in the data set:
define.variable <- function(varname,data=brdata) {
  i <- match(varname,names(data))
  R.name <- names(data)[i]
  stata.name <- attr(data,"val.labels")[i]
  stata.definition <- attr(data,"var.labels")[i]
  if (stata.name != "") {
    cat(R.name," (",stata.definition,"):\n",sep="")
    print(attr(brdata,"label.table")[[stata.name]])
  }
  else {
    R.def <- ifelse(stata.definition!="",stata.definition,"No definition given")
    cat(R.name," (",R.def,")\n",sep="")
    print(summary(brdata[[R.name]]))
  }
  cat("\n")
}
define.variable("income")
define.variable("age")
define.variable("age.sq")
define.variable("educ1")
define.variable("ideo7")
define.variable("dem.therm") # interesting: it's a quantitative
# variable, except that values as high
# as 97 or higher are coding different
# categories of response...
# etc...
###############################################################
# Now let's cut the data set down to something manageable
# for the exercise:
# get rid of some of the missing data, though this isn't such a big deal...
brdata1 <- brdata[is.na(brdata$black)==FALSE & is.na(brdata$female)==FALSE &
                    is.na(brdata$educ1)==FALSE & is.na(brdata$age)==FALSE &
                    is.na(brdata$income)==FALSE&is.na(brdata$state)==FALSE,]
# restrict to just the stuff from the 1992 election
# * republican candidate: George Herbert Walker Bush (father of Dubya)
# * democratic candidate: Michael Dukakis
data.92 <- brdata1[brdata1$year==1992,]
str(data.92)
dim(data.92)
################################################################
# for a response variable, you need something that counts as a
# vote for Bush in 1992
define.variable("presvote")
# presvote (party of pres vote- all major candidates):
# 0. dk/na if voted/didn't vote for pres/i 
#                                        0 
#                              1. democrat 
#                                        1 
#                            2. republican 
#                                        2 
# 3. major third party cand (wallace 1968/ 
#                                        3 
# So... we can define a vote for bush as follows:
#
for.bush <- ifelse(data.92$presvote==2,1,0)
# (hey, I just noticed that the variable "rep.presvote" in the data
# is the same as "for.bush"...)
table(data.92$rep.presvote,for.bush)
##################################################################
# now you are more or less ready to begin the exercise.
# I suggest  using "presvote", "female", "black", "educ3", "income",  "partyid7", "ideo_feel", "state2"
# to select among the alternative versions.