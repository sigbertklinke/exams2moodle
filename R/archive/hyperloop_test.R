library("exams2moodle")
x <- runif(100)
correct <- ttest_num(x=x, mu0=0.5, sigma=sqrt(1/12))
str(correct)

res <- hyperloop(ttest_num, 
                 n           = list(1, correct$n, correct$n+1),
                 mu0         = list(correct$mu0, correct$mean),
                 mean        = list(correct$mu0, correct$mean), 
                 sigma       = list(correct$sigma, correct$sd, sqrt(correct$sigma), sqrt(correct$sd)),
                 sd          = list(correct$sigma, correct$sd, sqrt(correct$sigma), sqrt(correct$sd)),
                 norm        = list(TRUE, FALSE)
)


x   <- rnorm(100)
trm <- hyperloop(mean, x=list(x), trim=as.list(seq(0, 0.5, by=0.05)))
# automatic conversion of x to list(x)
trm <- hyperloop(mean, x=x, trim=as.list(seq(0, 0.5, by=0.05))) 
unlist(trm)