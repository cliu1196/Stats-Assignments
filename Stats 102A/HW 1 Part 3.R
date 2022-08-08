# Unfortunately, I do not know how to form loops/if/while/etc. statements for things regarding characters and factors.
# I did not learn anything of this in my Stats 20 class. Part 3 is shown as much as I can do.

month_convert <- function(x, from_lang, to_lang) {
  vlan1 <- factor(c(), levels = c())
  vlan2 <- factor(c(), levels = c())
  if(from_lang == "English") {
  for(i in x) {
    if(i == 'January' | i == 'February' | i == 'March' | i == 'April' | i == 'May' |
       i == 'June' | i == 'July' | i == 'August' | 
       i == 'September' | i == 'October' | i == 'November' | i == 'Decemeber') {
      vlan1 <- factor(c(vlan1, i))
    }
    }
  if(to_lang == "Spanish") {
  for(i in vlan1) {
    if(i == 'enero' | i == 'febrero' | i == 'marzo' | i == 'abril' | i == 'mayo' |
       i == 'junio' | i == 'julio' | i == 'agosto' | 
       i == 'septiembre' | i == 'octubre' | i == 'noviembre' | i == 'diciembre') {
      vlan2 <- factor(c(vlan1, i))
  }
  }
  }
  return(vlan2)
  }
}



# Check:
x <- factor(c("March", "March", "February", "June"))
month_convert(x, "English", "Spanish")
month_names <- read.delim("UCLA Works/UCLA Winter 2020/Stats 102A/Homeworks/HW 1/month_names.txt", encoding="UTF-8", row.names=1)