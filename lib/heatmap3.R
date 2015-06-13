library(gplots)
library(gtools)

m <- matrix(1:30, ncol=6)
colnames(m) <- paste("C", 1:6, sep="")
rownames(m) <- paste("R", 1:5, sep="")
m

image(1:ncol(m), 1:nrow(m), t(m), col = terrain.colors(60), axes = FALSE)
axis(1, 1:ncol(m), colnames(m))
axis(2, 1:nrow(m), rownames(m))
for (x in 1:ncol(m))
  for (y in 1:nrow(m))
    text(x, y, m[y,x])