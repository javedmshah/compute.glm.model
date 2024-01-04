install.packages("haven")
library(haven)
pol_rev = read_dta("pol_rev.dta")

relig0 = relig
setDT(relig0)
cols = relig0[, c(4:21)]
cols = names(cols)[]
relig0[, paste0('sq_', cols) := lapply(.SD, \(x) x^2), .SDcols = cols]

relig0 %>% select(starts_with("sq_")) %>% rowSums(.) -> relig0$rel_heg0

sum.pos <- function(x) sum(x[x < 0.5], na.rm = TRUE)
relig0$rel_min0 = apply(relig0[, 4:21], 1, sum.pos)

relig1 <- select(relig0, 1, 3, 45:46)

# sample extrapolation - but isn't needed per se
ry = approxExtrap(relig0$year, relig0$rel_heg0, xout=2016, ties=mean, method="linear", na.rm=TRUE)

dfrelig = subset(relig0, select = -c(4:44))
dfrelig <- dfrelig %>% rename("rel_heg" = "rel_heg0", "rel_min" = "rel_min0")

dfrelig$rel_heg = 100*(dfrelig$rel_heg)
dfrelig$rel_min = 100*(dfrelig$rel_min)

dfrelig$rel_min = if_else(dfrelig$rel_min > 100, 100, dfrelig$rel_min)
dfrelig$rel_heg = if_else(dfrelig$rel_heg > 100, 100, dfrelig$rel_heg)
dfrelig$rel_min = if_else(dfrelig$rel_min < 0, 0, dfrelig$rel_min)

rC = c("Yugoslvaia")
rV = c("Yugoslavia")
dfrelig$country = replace(dfrelig$country, dfrelig$country %in% rC, rV)
save(dfrelig, file="rel_heg.rda")

df2relig = merge(dfrelig, pol_rev, by.x=c("country","year"), by.y=c("country","year"))
# OR
df2relig = dfrelig %>% inner_join( pol_rev, by=c('country','year'))

ucdp_f2 = read_dta("ucdp-dyadic_F2.dta")
require(tidyverse)
# checking if ucdp-dyadic_F2 didnt have Korea
sum(str_detect(ucdp_f2$country, '^Korea$')) > 0
ucdp_f3 = read_dta("ucdp-dyadic_F3.dta")
sum(str_detect(ucdp_f3$country, '^Korea$')) > 0

# check if policty2 is in ucdp_f3
"polity2" %in% colnames(ucdp_f3)

ucdp_f3$r_pol2 = ucdp_f3$polity2+10
ucdp_f3$r_pol2_sq = ucdp_f3$r_pol2^2
ucdp_f3$Totpop = ucdp_f3$Totpop/10000000
ucdp_f3$GDP = ucdp_f3$GDP/10000000


