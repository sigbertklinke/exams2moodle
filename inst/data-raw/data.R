library("rio")
distributions <- import("distributions.xlsx")
save(distributions, file="../../data/distributions.rda", version=2)