# create one hypotheses pair
hypothesis_latex("\\mu")
hypothesis_latex("\\pi")
hypothesis_latex("\\mu", alternative="two.sided")
hypothesis_latex("\\mu", alternative="two.sided", null="lt")
hypothesis_latex("\\mu", alternative="ne", null="eq")
# create several hypotheses pairs
hypothesis_latex("\\mu", right=c(0,1))
hypothesis_latex("\\mu", alternative=c("eq", "ne", "lt", "le", "gt", "ge"))
hypothesis_latex("\\mu", alternative=c("eq", "ne", "lt", "le", "gt", "ge"), 
                 null=c("eq", "ne", "lt", "le", "gt", "ge"))