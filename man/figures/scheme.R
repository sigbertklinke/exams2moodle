methods <- c("outlier", "extreme", "minmax")
#svg("scheme.svg")
pdf("scheme.pdf")
plot(c(-5,5), c(0,length(methods)+1), type="n", axes=FALSE, xlab="", ylab="")
for (i in 1:length(methods)) {
  if (methods[i]=="outlier") {
    b <- qnorm(c(0.25, 0.75))
    polygon(c(b[1], b[1], b[2], b[2]), c(i+0.3, i-0.3, i-0.3, i+0.3))
    text(0, i+0.3, "IQR", pos=3, col="blue")
    lines(b, c(i+0.35, i+0.35), col="blue")
    text(0, i, methods[i], font=2)
    lines(c(b[1]-3*diff(b), b[1]-1.5*diff(b)), c(i,i), col="red")
    lines(c(b[2]+1.5*diff(b), b[2]+3*diff(b)), c(i,i), col="red")
    text(b[1]-2.25*diff(b), i, "1.5 IQR", pos=3, col="red")
    text(b[2]+2.25*diff(b), i, "1.5 IQR", pos=3, col="red")
  }
  if (methods[i]=="extreme") {
    b <- qnorm(c(0.25, 0.75))
    polygon(c(b[1], b[1], b[2], b[2]), c(i+0.3, i-0.3, i-0.3, i+0.3))
    text(0, i+0.3, "IQR", pos=3, col="blue")
    lines(b, c(i+0.35, i+0.35), col="blue")
    text(0, i, methods[i], font=2)
    lines(c(b[1]-1.5*diff(b), b[1]-0*diff(b)), c(i,i), col="red")
    lines(c(b[2]+0*diff(b), b[2]+1.5*diff(b)), c(i,i), col="red")
    text(b[1]-0.75*diff(b), i, "1.5 IQR", pos=3, col="red")
    text(b[2]+0.75*diff(b), i, "1.5 IQR", pos=3, col="red")
  }
  if (methods[i]=="minmax") {
    b <- c(-2.0, 2.0)
    polygon(c(b[1], b[1], b[2], b[2]), c(i+0.3, i-0.3, i-0.3, i+0.3))
    text(0, i+0.3, "Range", pos=3, col="blue")
    lines(b, c(i+0.35, i+0.35), col="blue")
    text(0, i, methods[i], font=2)
    lines(c(b[1]-0.5*diff(b), b[1]-0*diff(b)), c(i,i), col="red")
    lines(c(b[2]+0*diff(b), b[2]+0.5*diff(b)), c(i,i), col="red")
    text(b[1]-0.25*diff(b), i, "0.5 Range", pos=3, col="red")
    text(b[2]+0.25*diff(b), i, "0.5 Range", pos=3, col="red")
  }
}
box()
axis(1, c(-2, qnorm(c(0.25, 0.75)), 2), c("min", "25%", "75%", "max"))
dev.off()