table(discwomen, discgaylesbian)
mytable <- table(discwomen, discgaywomen)
prop.table(mytable, 1)
# Row table like conditional probability and be mindful at what you look at
# Of those individuals who discriminate a lot against women, 87% also discriminate
# a lot against the gay and lesbian population.
prop.table(mytable, 2)
# Column table