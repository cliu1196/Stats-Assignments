require(arm)
require(foreign)
require(MASS)
df <- read.dta("http://www.stat.columbia.edu/~gelman/arm/examples/nes/nes5200_processed_voters_realideo.dta")

df$partyid3 <- factor(df$partyid3, labels=c("democrats", "independents",
                                            "republicans", "apolitical"))
df$gender <- factor(df$gender, labels=c("male", "female"))
df$race <- factor(df$race, labels=c("white", "black", "asian", 
                                    "native american", "hispanic",
                                    "other"))
df$south <- factor(df$south)
df$ideo <- factor(df$ideo, labels=c("liberal", "moderate", "conservative"))

# filter out cases where `partyid3` is NA
x = df$partyid3
df <- df[!is.na(levels(x)[x]),]

# exclude apolitical to have an ordered outcome (i.e. democrats, independents, republicans)
df <- subset(df, partyid3!="apolitical")
df$partyid3 <- factor(df$partyid3)


multi.log <- polr(partyid3 ~ ideo + race + age_10, Hess=TRUE, data=df)
summary(multi.log)

confint(multi.log)

residuals(multi.log)

binnedplot(predict(multi.log), resid(multi.log))
